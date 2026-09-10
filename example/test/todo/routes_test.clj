(ns todo.routes-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.observability.metrics :as metrics]
            [fx.ring.response :as fx-resp]
            [fx.utils :as fxu]
            [todo.db :as db]
            [todo.domain :as domain]
            [todo.routes :as routes])
  (:import (java.util UUID)))

;; ---------------------------------------------------------------------------
;; Test Setup Helpers
;; ---------------------------------------------------------------------------

(defn- fresh-test-ds []
  (let [url (str "jdbc:sqlite:file:testdb-routes-" (UUID/randomUUID) "?mode=memory&cache=shared")
        ds (db/create-datasource url)]
    (fx/run-sync! (db/init-db!> ds))
    ds))

;; ---------------------------------------------------------------------------
;; 1. Failure Translation Map Tests
;; ---------------------------------------------------------------------------

(deftest test-failure-map-translations
  (testing ":todo/not-found maps to 404 response"
    (let [handler (get routes/failure-map :todo/not-found)
          resp (handler {:message "Item 42 missing" :id 42})]
      (is (= 404 (:status resp)))
      (is (= {:error "Not Found" :details {:message "Item 42 missing" :id 42}}
             (:body resp)))))

  (testing ":todo/invalid-input maps to 400 response"
    (let [handler (get routes/failure-map :todo/invalid-input)
          resp (handler {:message "Title cannot be blank" :field :title})]
      (is (= 400 (:status resp)))
      (is (= {:error "Bad Request" :details {:message "Title cannot be blank" :field :title}}
             (:body resp)))))

  (testing ":rate-limiter/exceeded maps to 429 response"
    (let [handler (get routes/failure-map :rate-limiter/exceeded)
          resp (handler {:message "Rate limit exceeded" :retry-after-ms 1000})]
      (is (= 429 (:status resp)))
      (is (= {:error "Too Many Requests" :details {:message "Rate limit exceeded" :retry-after-ms 1000}}
             (:body resp)))))

  (testing ":circuit-breaker/open maps to 503 response"
    (let [handler (get routes/failure-map :circuit-breaker/open)
          resp (handler {:message "Service circuit breaker open"})]
      (is (= 503 (:status resp)))
      (is (= {:error "Service Unavailable" :details {:message "Service circuit breaker open"}}
             (:body resp)))))

  (testing ":jdbc/error maps to 500 response"
    (let [handler (get routes/failure-map :jdbc/error)
          resp (handler {:message "Table locked" :sqlstate "HY000"})]
      (is (= 500 (:status resp)))
      (is (= {:error "Database Error" :details "Table locked"}
             (:body resp))))))

;; ---------------------------------------------------------------------------
;; 2. AST Structure & Handler Inspection (Programs as Data)
;; ---------------------------------------------------------------------------

(deftest test-route-handlers-ast-inspection
  (testing "route handlers return pure effect pipelines for a given request"
    (let [list-h (routes/list-todos-handler> {})
          create-h (routes/create-todo-handler> {:body-params {:title "Task"}})
          get-h (routes/get-todo-handler> {:path-params {:id "1"}})
          update-h (routes/update-todo-handler> {:path-params {:id "1"}})
          toggle-h (routes/toggle-todo-handler> {:path-params {:id "1"}})
          delete-h (routes/delete-todo-handler> {:path-params {:id "1"}})
          metrics-h (routes/metrics-handler> {})]

      (is (fx/effect? list-h))
      (is (fx/effect? create-h))
      (is (fx/effect? get-h))
      (is (fx/effect? update-h))
      (is (fx/effect? toggle-h))
      (is (fx/effect? delete-h))
      (is (fx/effect? metrics-h)))))

;; ---------------------------------------------------------------------------
;; 3. Direct Execution of Route Effect Handlers with Mock Request Context
;; ---------------------------------------------------------------------------

(deftest test-route-handlers-direct-execution
  (let [ds (fresh-test-ds)
        base-ctx {:fx.jdbc/datasource ds
                  :fx.observability/metrics-registry (metrics/make-metrics-registry)}]

    (testing "metrics-handler> returns 200 with metrics snapshot map"
      (let [resp (fx/run-sync!
                   (routes/metrics-handler> {})
                   base-ctx)]
        (is (= 200 (:status resp)))
        (is (map? (:body resp)))
        (is (contains? (:body resp) :counters))
        (is (contains? (:body resp) :gauges))
        (is (contains? (:body resp) :timers))))

    (testing "create-todo-handler> processes request body and returns 201 status"
      (let [req {:body-params {:title       "Handler Test Task"
                               :description "Testing effect handler directly"}}
            resp (fx/run-sync!
                   (routes/create-todo-handler> req)
                   base-ctx)]
        (is (= 201 (:status resp)))
        (is (= "Handler Test Task" (get-in resp [:body :title])))
        (is (= 1 (get-in resp [:body :id])))))

    (testing "get-todo-handler> returns 200 for valid ID, or :todo/invalid-input for non-numeric ID"
      (let [req-valid {:path-params {:id "1"}}
            resp-valid (fx/run-sync!
                         (routes/get-todo-handler> req-valid)
                         base-ctx)

            req-invalid {:path-params {:id "not-a-number"}}
            resp-invalid (fx/run-sync!
                           (routes/get-todo-handler> req-invalid)
                           base-ctx)]

        (is (= 200 (:status resp-valid)))
        (is (= 1 (get-in resp-valid [:body :id])))

        (is (fx/failure? resp-invalid))
        (is (= :todo/invalid-input (fx/tag resp-invalid)))))

    (testing "list-todos-handler> handles query parameters for filtering"
      ;; Create a completed todo
      (fx/run-sync! (domain/create-todo> {:title "Task 2"}) base-ctx)
      (fx/run-sync! (domain/toggle-todo> 2) base-ctx)

      (let [req-all {}
            resp-all (fx/run-sync!
                       (routes/list-todos-handler> req-all)
                       base-ctx)

            req-comp {:params {"completed" "true"}}
            resp-comp (fx/run-sync!
                        (routes/list-todos-handler> req-comp)
                        base-ctx)

            req-act {:params {"completed" "false"}}
            resp-act (fx/run-sync!
                       (routes/list-todos-handler> req-act)
                       base-ctx)]

        (is (= 200 (:status resp-all)))
        (is (= 2 (count (:body resp-all))))

        (is (= 200 (:status resp-comp)))
        (is (= 1 (count (:body resp-comp))))
        (is (= 2 (:id (first (:body resp-comp)))))

        (is (= 200 (:status resp-act)))
        (is (= 1 (count (:body resp-act))))
        (is (= 1 (:id (first (:body resp-act)))))))

    (testing "update-todo-handler> updates entity or rejects bad id"
      (let [req-valid {:path-params {:id "1"}
                       :body-params {:title "Updated via Handler"}}
            resp-valid (fx/run-sync!
                         (routes/update-todo-handler> req-valid)
                         base-ctx)

            req-bad-id {:path-params {:id "abc"}
                        :body-params {:title "Whatever"}}
            resp-bad-id (fx/run-sync!
                          (routes/update-todo-handler> req-bad-id)
                          base-ctx)]

        (is (= 200 (:status resp-valid)))
        (is (= "Updated via Handler" (get-in resp-valid [:body :title])))

        (is (fx/failure? resp-bad-id))
        (is (= :todo/invalid-input (fx/tag resp-bad-id)))))

    (testing "toggle-todo-handler> toggles completion or rejects bad id"
      (let [req-valid {:path-params {:id "1"}}
            resp-valid (fx/run-sync!
                         (routes/toggle-todo-handler> req-valid)
                         base-ctx)

            req-bad-id {:path-params {:id "abc"}}
            resp-bad-id (fx/run-sync!
                          (routes/toggle-todo-handler> req-bad-id)
                          base-ctx)]

        (is (= 200 (:status resp-valid)))
        (is (true? (get-in resp-valid [:body :completed])))

        (is (fx/failure? resp-bad-id))
        (is (= :todo/invalid-input (fx/tag resp-bad-id)))))

    (testing "delete-todo-handler> deletes entity or rejects bad id"
      (let [req-valid {:path-params {:id "1"}}
            resp-valid (fx/run-sync!
                         (routes/delete-todo-handler> req-valid)
                         base-ctx)

            req-bad-id {:path-params {:id "abc"}}
            resp-bad-id (fx/run-sync!
                          (routes/delete-todo-handler> req-bad-id)
                          base-ctx)]

        (is (= 200 (:status resp-valid)))
        (is (= {:deleted true :id 1} (:body resp-valid)))

        (is (fx/failure? resp-bad-id))
        (is (= :todo/invalid-input (fx/tag resp-bad-id)))))))
