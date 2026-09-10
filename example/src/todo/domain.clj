(ns todo.domain
  (:require [cheshire.core :as json]
            [clojure.string :as str]
            [fx.core :as fx]
            [fx.http-client :as http]
            [fx.jdbc :as fx-jdbc]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [fx.observability.trace :as trace]
            [fx.schedule :as sched]
            [todo.db :as db]
            [todo.resilience :as resilience]
            [todo.schema :as schema])
  (:import (java.time Instant)))

;; ---------------------------------------------------------------------------
;; Input Validation
;; ---------------------------------------------------------------------------

(defn validate-create-payload>
  "Validates and coerces payload for creating a todo."
  [payload]
  (schema/validate-create-todo> payload))

(defn validate-update-payload>
  "Validates and coerces payload for updating a todo."
  [payload]
  (schema/validate-update-todo> payload))

;; ---------------------------------------------------------------------------
;; Domain Effect Pipelines
;; ---------------------------------------------------------------------------

(defn list-todos>
  "Retrieves all todos, optionally filtered by `:completed` boolean."
  ([]
   (list-todos> nil))
  ([completed-filter]
   (trace/with-span> "todo.list" {:completed-filter completed-filter}
     (db/query-todos> completed-filter))))

(defn get-todo-by-id>
  "Retrieves a single todo by `id`.
   Fails with `:todo/not-found` if no matching record exists."
  [id]
  (trace/with-span> "todo.get" {:id id}
    (-> (db/query-todo-by-id> id)
        (fx/mapcat> (fn [todo]
                      (if (nil? todo)
                        (-> (log/log-warn> (str "Todo not found with id " id) {:id id})
                            (fx/chain> (fx/fail> :todo/not-found {:message (str "Todo not found with id " id)
                                                                  :id      id})))
                        (fx/succeed> todo)))))))

(defn create-todo>
  "Validates input payload and inserts a new todo with timestamps.
   Fails with `:todo/invalid-input` if payload validation fails."
  [payload]
  (trace/with-span> "todo.create" {:title (:title payload)}
    (->> (-> (validate-create-payload> payload)
             (fx/mapcat> (fn [{:keys [title description completed]}]
                           (let [now (str (Instant/now))
                                 record {:title       title
                                         :description description
                                         :completed   (boolean completed)
                                         :created-at  now
                                         :updated-at  now}]
                             (-> (db/insert-todo!> record)
                                 (log/log-info> "Todo created" {:title title}))))))
         (metrics/track-success-count> (metrics/metric-counter "todo.created.total")))))

(defn update-todo>
  "Validates update payload, verifies existence, and updates todo fields.
   Fails with `:todo/invalid-input` or `:todo/not-found`."
  [id payload]
  (trace/with-span> "todo.update" {:id id}
    (-> (validate-update-payload> payload)
        (fx/mapcat> (fn [valid-payload]
                      (-> (get-todo-by-id> id)
                          (fx/mapcat> (fn [_existing]
                                        (let [now (str (Instant/now))
                                              updates (cond-> {:updated-at now}
                                                        (contains? valid-payload :title)
                                                        (assoc :title (:title valid-payload))

                                                        (contains? valid-payload :description)
                                                        (assoc :description (:description valid-payload))

                                                        (contains? valid-payload :completed)
                                                        (assoc :completed (:completed valid-payload)))]
                                          (-> (db/update-todo!> id updates)
                                              (log/log-info> "Todo updated" {:id id})))))))))))

(defn toggle-todo>
  "Flips the `:completed` boolean of an existing todo.
   Fails with `:todo/not-found` if the todo does not exist."
  [id]
  (trace/with-span> "todo.toggle" {:id id}
    (-> (get-todo-by-id> id)
        (fx/mapcat> (fn [_existing]
                      (let [now (str (Instant/now))]
                        (-> (db/toggle-todo!> id now)
                            (log/log-info> "Todo toggled" {:id id}))))))))

(defn delete-todo>
  "Deletes an existing todo by `id`.
   Fails with `:todo/not-found` if the todo does not exist.
   Returns `{:deleted true, :id id}` on success."
  [id]
  (trace/with-span> "todo.delete" {:id id}
    (->> (-> (get-todo-by-id> id)
             (fx/mapcat> (fn [_existing]
                           (-> (db/delete-todo!> id)
                               (fx/map> (fn [_]
                                          {:deleted true
                                           :id      id}))
                               (log/log-info> "Todo deleted" {:id id})))))
         (metrics/track-success-count> (metrics/metric-counter "todo.deleted.total")))))

(defn import-remote-todos>
  "Fetches remote todos from an external JSON endpoint and persists them into SQLite.
   Accepts `payload` map `{:keys [url limit]}`.
   Validates input payload, performs GET request with timeout, extracts todo items,
   normalizes attributes, and batch inserts records inside a transaction."
  [payload]
  (trace/with-span> "todo.import-remote" {:payload payload}
    (->> (-> (schema/validate-import-payload> payload)
             (fx/mapcat> (fn [{:keys [url limit]}]
                           (-> (http/get> url {:as :json :timeout 5000})
                               (fx/mapcat> (fn [resp]
                                             (let [raw-body (:body resp)
                                                   items (cond
                                                           (sequential? raw-body) raw-body
                                                           (and (map? raw-body) (sequential? (:todos raw-body))) (:todos raw-body)
                                                           (and (map? raw-body) (sequential? (:items raw-body))) (:items raw-body)
                                                           (map? raw-body) [raw-body]
                                                           :else [])
                                                   limited-items (if (and limit (pos? limit))
                                                                   (take limit items)
                                                                   items)
                                                   now (str (Instant/now))
                                                   records (keep (fn [item]
                                                                   (let [title (or (:title item) (:name item))]
                                                                     (when (and (string? title) (not (str/blank? title)))
                                                                       {:title       (str/trim title)
                                                                        :description (some-> (or (:description item) (:body item)) str/trim)
                                                                        :completed   (boolean (or (:completed item) false))
                                                                        :created-at  now
                                                                        :updated-at  now})))
                                                                 limited-items)]
                                               (if (empty? records)
                                                 (fx/succeed> {:imported-count 0 :todos []})
                                                 (fx-jdbc/with-transaction>
                                                   (-> (reduce (fn [acc-eff record]
                                                                 (-> acc-eff
                                                                     (fx/mapcat> (fn [acc]
                                                                                   (-> (db/insert-todo!> record)
                                                                                       (fx/map> (fn [inserted]
                                                                                                  (conj acc inserted))))))))
                                                               (fx/succeed> [])
                                                               records)
                                                       (fx/map> (fn [inserted-todos]
                                                                  {:imported-count (count inserted-todos)
                                                                   :todos          inserted-todos}))))))))
                               (log/log-info> "Remote todos imported" {:url url})))))
         (metrics/track-success-count> (metrics/metric-counter "todo.imported.total")))))

(defn notify-webhook>
  "Fetches a todo by `id` and dispatches a JSON notification payload to `webhook-url`.
   Applies exponential backoff retries on transient network/server failures."
  [todo-id payload]
  (trace/with-span> "todo.notify-webhook" {:todo-id todo-id :payload payload}
    (->> (-> (schema/validate-webhook-payload> payload)
             (fx/mapcat> (fn [{:keys [webhook-url]}]
                           (-> (get-todo-by-id> todo-id)
                               (fx/mapcat> (fn [todo]
                                             (-> (http/post> webhook-url {:body    (json/generate-string todo)
                                                                          :headers {"content-type" "application/json"}
                                                                          :as      :json
                                                                          :timeout 5000})
                                                 (sched/retry-schedule> resilience/webhook-retry-policy)
                                                 (fx/map> (fn [resp]
                                                            {:notified      true
                                                             :todo-id       todo-id
                                                             :webhook-url   webhook-url
                                                             :remote-status (:status resp)}))
                                                 (log/log-info> "Webhook notification dispatched" {:todo-id todo-id :webhook-url webhook-url}))))))))
         (metrics/track-success-count> (metrics/metric-counter "todo.webhook.notified.total")))))
