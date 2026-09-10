(ns todo.api-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.observability.metrics :as metrics]
            [muuntaja.core :as m]
            [todo.db :as db]
            [todo.domain :as domain]
            [todo.routes :as routes])
  (:import (java.util UUID)))

(def ^:private m-instance (m/create))

(defn- test-db-url []
  (str "jdbc:sqlite:file:testdb-" (UUID/randomUUID) "?mode=memory&cache=shared"))

(defn- setup-test-app []
  (let [url (test-db-url)
        ds (db/create-datasource url)
        _ (fx/run-sync! (db/init-db!> ds))
        reg (metrics/make-metrics-registry)
        app (routes/create-app {:fx.jdbc/datasource ds
                                :fx.observability/metrics-registry reg})]
    {:ds  ds
     :reg reg
     :app app}))

(defn- parse-response-body [resp]
  (let [body (:body resp)]
    (cond
      (nil? body)
      nil

      (instance? java.io.InputStream body)
      (m/decode m-instance "application/json" (slurp body))

      (string? body)
      (m/decode m-instance "application/json" body)

      (map? body)
      body

      (vector? body)
      body

      :else
      body)))

(defn- request
  ([app method uri]
   (request app method uri nil nil nil))
  ([app method uri body-params]
   (request app method uri body-params nil nil))
  ([app method uri body-params query-params]
   (request app method uri body-params query-params nil))
  ([app method uri body-params query-params headers]
   (let [req (cond-> {:request-method method
                      :uri            uri
                      :headers        (merge {"accept"       "application/json"
                                              "content-type" "application/json"}
                                             headers)}
               body-params  (assoc :body-params body-params
                                   :body (m/encode m-instance "application/json" body-params))
               query-params (assoc :query-params query-params
                                   :params query-params))
         resp (app req)]
     (update resp :body (fn [_] (parse-response-body resp))))))

;; ---------------------------------------------------------------------------
;; Integration Tests
;; ---------------------------------------------------------------------------

(deftest test-crud-lifecycle
  (let [{:keys [app]} (setup-test-app)]
    (testing "Create a new todo (POST /api/todos)"
      (let [resp (request app :post "/api/todos" {:title       "Buy groceries"
                                                 :description "Milk, eggs, and bread"})]
        (is (= 201 (:status resp)))
        (is (= "Buy groceries" (get-in resp [:body :title])))
        (is (= "Milk, eggs, and bread" (get-in resp [:body :description])))
        (is (false? (get-in resp [:body :completed])))
        (is (number? (get-in resp [:body :id])))
        (is (some? (get-in resp [:body :created-at])))
        (is (some? (get-in resp [:body :updated-at])))))

    (testing "Fetch the created todo by ID (GET /api/todos/:id)"
      (let [create-resp (request app :post "/api/todos" {:title "Walk the dog"})
            todo-id (get-in create-resp [:body :id])
            get-resp (request app :get (str "/api/todos/" todo-id))]
        (is (= 200 (:status get-resp)))
        (is (= todo-id (get-in get-resp [:body :id])))
        (is (= "Walk the dog" (get-in get-resp [:body :title])))
        (is (false? (get-in get-resp [:body :completed])))))

    (testing "List all todos (GET /api/todos)"
      (let [list-resp (request app :get "/api/todos")]
        (is (= 200 (:status list-resp)))
        (is (vector? (:body list-resp)))
        (is (>= (count (:body list-resp)) 2))))

    (testing "Update todo details (PUT /api/todos/:id)"
      (let [create-resp (request app :post "/api/todos" {:title "Read book" :description "Chapter 1"})
            todo-id (get-in create-resp [:body :id])
            update-resp (request app :put (str "/api/todos/" todo-id)
                                 {:title       "Read Clojure book"
                                  :description "Chapters 1 and 2"
                                  :completed   false})]
        (is (= 200 (:status update-resp)))
        (is (= "Read Clojure book" (get-in update-resp [:body :title])))
        (is (= "Chapters 1 and 2" (get-in update-resp [:body :description])))))

    (testing "Toggle todo completion status (PATCH /api/todos/:id/toggle)"
      (let [create-resp (request app :post "/api/todos" {:title "Deploy service"})
            todo-id (get-in create-resp [:body :id])
            toggle1 (request app :patch (str "/api/todos/" todo-id "/toggle"))
            toggle2 (request app :patch (str "/api/todos/" todo-id "/toggle"))]
        (is (= 200 (:status toggle1)))
        (is (true? (get-in toggle1 [:body :completed])))
        (is (= 200 (:status toggle2)))
        (is (false? (get-in toggle2 [:body :completed])))))

    (testing "Filter todos by completion status (GET /api/todos?completed=true|false)"
      (let [create-resp (request app :post "/api/todos" {:title "Finish report"})
            todo-id (get-in create-resp [:body :id])
            _ (request app :patch (str "/api/todos/" todo-id "/toggle"))
            completed-resp (request app :get "/api/todos" nil {"completed" "true"})
            pending-resp (request app :get "/api/todos" nil {"completed" "false"})]
        (is (= 200 (:status completed-resp)))
        (is (every? #(true? (:completed %)) (:body completed-resp)))
        (is (some #(= todo-id (:id %)) (:body completed-resp)))

        (is (= 200 (:status pending-resp)))
        (is (every? #(false? (:completed %)) (:body pending-resp)))
        (is (not-any? #(= todo-id (:id %)) (:body pending-resp)))))

    (testing "Delete todo (DELETE /api/todos/:id) and confirm 404 on subsequent get"
      (let [create-resp (request app :post "/api/todos" {:title "Temporary todo"})
            todo-id (get-in create-resp [:body :id])
            del-resp (request app :delete (str "/api/todos/" todo-id))
            get-after-del (request app :get (str "/api/todos/" todo-id))]
        (is (= 200 (:status del-resp)))
        (is (true? (get-in del-resp [:body :deleted])))
        (is (= todo-id (get-in del-resp [:body :id])))

        (is (= 404 (:status get-after-del)))
        (is (= "Not Found" (get-in get-after-del [:body :error])))))))

(deftest test-validation-and-failure-handling
  (let [{:keys [app]} (setup-test-app)]
    (testing "Create todo with missing title returns 400"
      (let [resp (request app :post "/api/todos" {:description "No title"})]
        (is (= 400 (:status resp)))
        (is (= "Bad Request" (get-in resp [:body :error])))
        (is (= "title" (name (get-in resp [:body :details :field]))))))

    (testing "Create todo with blank title returns 400"
      (let [resp (request app :post "/api/todos" {:title "   "})]
        (is (= 400 (:status resp)))
        (is (= "Bad Request" (get-in resp [:body :error])))))

    (testing "Update todo with invalid boolean returns 400"
      (let [create-resp (request app :post "/api/todos" {:title "Valid title"})
            todo-id (get-in create-resp [:body :id])
            resp (request app :put (str "/api/todos/" todo-id) {:completed "not-a-bool"})]
        (is (= 400 (:status resp)))
        (is (= "Bad Request" (get-in resp [:body :error])))
        (is (= "completed" (name (get-in resp [:body :details :field]))))))

    (testing "Non-existent todo operations return 404"
      (let [get-resp (request app :get "/api/todos/99999")
            put-resp (request app :put "/api/todos/99999" {:title "New title"})
            patch-resp (request app :patch "/api/todos/99999/toggle")
            del-resp (request app :delete "/api/todos/99999")]
        (is (= 404 (:status get-resp)))
        (is (= 404 (:status put-resp)))
        (is (= 404 (:status patch-resp)))
        (is (= 404 (:status del-resp)))))

    (testing "Invalid ID string returns 400"
      (let [resp (request app :get "/api/todos/not-a-number")]
        (is (= 400 (:status resp)))
        (is (= "Bad Request" (get-in resp [:body :error])))))))

(deftest test-pure-domain-effects
  (let [url (test-db-url)
        ds (db/create-datasource url)
        _ (fx/run-sync! (db/init-db!> ds))
        ctx {:fx.jdbc/datasource ds}]
    (testing "Direct execution of domain effect pipelines with fx/run-sync!"
      (let [created (fx/run-sync! (domain/create-todo> {:title "Direct effect test"
                                                       :description "Testing fx pipelines"})
                                  ctx)]
        (is (not (fx/failure? created)))
        (is (= "Direct effect test" (:title created)))
        (is (false? (:completed created)))

        (let [fetched (fx/run-sync! (domain/get-todo-by-id> (:id created)) ctx)]
          (is (= (:id created) (:id fetched))))

        (let [toggled (fx/run-sync! (domain/toggle-todo> (:id created)) ctx)]
          (is (true? (:completed toggled))))

        (let [deleted (fx/run-sync! (domain/delete-todo> (:id created)) ctx)]
          (is (= {:deleted true :id (:id created)} deleted)))

        (let [not-found (fx/run-sync! (domain/get-todo-by-id> (:id created)) ctx)]
          (is (fx/failure? not-found))
          (is (= :todo/not-found (fx/tag not-found))))))))

(deftest test-observability-endpoints-and-headers
  (let [{:keys [app]} (setup-test-app)]
    (testing "GET /api/metrics returns 200 with structured metrics map"
      (let [resp (request app :get "/api/metrics")]
        (is (= 200 (:status resp)))
        (is (map? (:body resp)))
        (is (contains? (:body resp) :counters))
        (is (contains? (:body resp) :gauges))
        (is (contains? (:body resp) :timers))))

    (testing "W3C traceparent header propagation with incoming custom traceparent"
      (let [custom-tp "00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01"
            resp (request app :get "/api/todos" nil nil {"traceparent" custom-tp})
            resp-tp (get-in resp [:headers "traceparent"])]
        (is (= 200 (:status resp)))
        (is (some? resp-tp))
        (is (re-find #"^00-4bf92f3577b34da6a3ce929d0e0e4736-[0-9a-f]{16}-01$" resp-tp))))

    (testing "W3C traceparent header generated automatically when omitted"
      (let [resp (request app :get "/api/todos")
            resp-tp (get-in resp [:headers "traceparent"])]
        (is (= 200 (:status resp)))
        (is (some? resp-tp))
        (is (re-find #"^00-[0-9a-f]{32}-[0-9a-f]{16}-01$" resp-tp))))))

(deftest test-metrics-accumulation
  (let [{:keys [app]} (setup-test-app)]
    (testing "CRUD operations accumulate counters and timers in metrics snapshot"
      (let [c-resp (request app :post "/api/todos" {:title "Tracked Todo" :description "Measuring metrics"})
            todo-id (get-in c-resp [:body :id])
            _ (request app :get (str "/api/todos/" todo-id))
            _ (request app :delete (str "/api/todos/" todo-id))
            m-resp (request app :get "/api/metrics")
            metrics-data (:body m-resp)
            counters (:counters metrics-data)
            timers (:timers metrics-data)
            get-counter (fn [k] (or (get-in counters [k :value])
                                   (get-in counters [(keyword k) :value])
                                   (get-in counters [(name k) :value])
                                   0))
            get-timer-count (fn [k] (or (get-in timers [k :count])
                                       (get-in timers [(keyword k) :count])
                                       (get-in timers [(name k) :count])
                                       0))]

        (is (= 200 (:status m-resp)))
        (is (>= (get-counter :todo.created.total) 1))
        (is (>= (get-counter :todo.deleted.total) 1))
        (is (>= (get-counter :http.server.requests.total) 1))
        (is (>= (get-timer-count :http.server.requests.duration) 3))
        (is (pos? (get-timer-count :db.query.duration)))))))
