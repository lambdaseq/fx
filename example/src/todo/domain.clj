(ns todo.domain
  (:require [clojure.string :as str]
            [fx.core :as fx]
            [todo.db :as db])
  (:import (java.time Instant)))

;; ---------------------------------------------------------------------------
;; Input Validation
;; ---------------------------------------------------------------------------

(defn- blank-str? [s]
  (or (nil? s) (not (string? s)) (str/blank? s)))

(defn- validate-create-payload [payload]
  (cond
    (not (map? payload))
    (fx/fail> :todo/invalid-input {:message "Request body must be a JSON object"})

    (blank-str? (:title payload))
    (fx/fail> :todo/invalid-input {:message "Field 'title' is required and must not be blank"
                                   :field   :title})

    :else
    (fx/succeed> payload)))

(defn- validate-update-payload [payload]
  (cond
    (not (map? payload))
    (fx/fail> :todo/invalid-input {:message "Request body must be a JSON object"})

    (and (contains? payload :title) (blank-str? (:title payload)))
    (fx/fail> :todo/invalid-input {:message "Field 'title' must not be blank if provided"
                                   :field   :title})

    (and (contains? payload :completed) (not (boolean? (:completed payload))))
    (fx/fail> :todo/invalid-input {:message "Field 'completed' must be a boolean if provided"
                                   :field   :completed})

    :else
    (fx/succeed> payload)))

;; ---------------------------------------------------------------------------
;; Domain Effect Pipelines
;; ---------------------------------------------------------------------------

(defn list-todos>
  "Retrieves all todos, optionally filtered by `:completed` boolean."
  ([]
   (db/query-todos>))
  ([completed-filter]
   (db/query-todos> completed-filter)))

(defn get-todo-by-id>
  "Retrieves a single todo by `id`.
   Fails with `:todo/not-found` if no matching record exists."
  [id]
  (-> (db/query-todo-by-id> id)
      (fx/mapcat> (fn [todo]
                    (if (nil? todo)
                      (fx/fail> :todo/not-found {:message (str "Todo not found with id " id)
                                                 :id      id})
                      (fx/succeed> todo))))))

(defn create-todo>
  "Validates input payload and inserts a new todo with timestamps.
   Fails with `:todo/invalid-input` if payload validation fails."
  [payload]
  (-> (validate-create-payload payload)
      (fx/mapcat> (fn [{:keys [title description]}]
                    (let [now (str (Instant/now))
                          record {:title       (str/trim title)
                                  :description (some-> description str/trim)
                                  :completed   false
                                  :created-at  now
                                  :updated-at  now}]
                      (db/insert-todo!> record))))))

(defn update-todo>
  "Validates update payload, verifies existence, and updates todo fields.
   Fails with `:todo/invalid-input` or `:todo/not-found`."
  [id payload]
  (-> (validate-update-payload payload)
      (fx/mapcat> (fn [valid-payload]
                    (-> (get-todo-by-id> id)
                        (fx/mapcat> (fn [_existing]
                                      (let [now (str (Instant/now))
                                            updates (cond-> {:updated-at now}
                                                      (contains? valid-payload :title)
                                                      (assoc :title (str/trim (:title valid-payload)))

                                                      (contains? valid-payload :description)
                                                      (assoc :description (some-> (:description valid-payload) str/trim))

                                                      (contains? valid-payload :completed)
                                                      (assoc :completed (:completed valid-payload)))]
                                        (db/update-todo!> id updates)))))))))

(defn toggle-todo>
  "Flips the `:completed` boolean of an existing todo.
   Fails with `:todo/not-found` if the todo does not exist."
  [id]
  (-> (get-todo-by-id> id)
      (fx/mapcat> (fn [_existing]
                    (let [now (str (Instant/now))]
                      (db/toggle-todo!> id now))))))

(defn delete-todo>
  "Deletes an existing todo by `id`.
   Fails with `:todo/not-found` if the todo does not exist.
   Returns `{:deleted true, :id id}` on success."
  [id]
  (-> (get-todo-by-id> id)
      (fx/mapcat> (fn [_existing]
                    (-> (db/delete-todo!> id)
                        (fx/map> (fn [_]
                                   {:deleted true
                                    :id      id})))))))
