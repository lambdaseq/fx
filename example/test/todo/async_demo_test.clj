(ns todo.async-demo-test
  (:require [cheshire.core :as json]
            [clojure.test :refer [deftest is testing]]
            [fx.async :as fxa]
            [fx.core :as fx]
            [todo.db :as db]
            [todo.domain :as domain]
            [todo.examples.async-demo :as demo]
            [todo.routes :as routes])
  (:import (java.util UUID)))

(defn- test-db-url []
  (str "jdbc:sqlite:file:testdb-async-demo-" (UUID/randomUUID) "?mode=memory&cache=shared"))

(defn- parse-body [body]
  (cond
    (nil? body) nil
    (instance? java.io.InputStream body) (json/parse-string (slurp body) true)
    (string? body) (json/parse-string body true)
    :else body))

(deftest test-async-demo-scenarios
  (testing "demo-parallel-processing> executes without error"
    (let [res (fx/run-sync! (demo/demo-parallel-processing>))]
      (is (= 8 (count res)))
      (is (every? #(= :done (:status %)) res))))

  (testing "demo-speculative-race> returns fast mirror winner"
    (let [res (fx/run-sync! (demo/demo-speculative-race>))]
      (is (= :fast-mirror (:source res)))))

  (testing "demo-fiber-supervision> executes finalizers"
    (let [res (fx/run-sync! (demo/demo-fiber-supervision>))]
      (is (not (fx/failure? res)))))

  (testing "demo-channel-pipeline> completes successfully"
    (let [res (fx/run-sync! (demo/demo-channel-pipeline>))]
      (is (not (fx/failure? res)))))

  (testing "demo-coordination-primitives> completes successfully"
    (let [res (fx/run-sync! (demo/demo-coordination-primitives>))]
      (is (not (fx/failure? res)))))

  (testing "demo-execution-scopes> completes successfully"
    (let [res (fx/run-sync! (demo/demo-execution-scopes>))]
      (is (not (fx/failure? res))))))

(deftest test-batch-import-api-endpoint
  (let [ds (db/create-datasource (test-db-url))
        _ (fx/run-sync! (db/init-db!> ds))
        ctx {:fx.jdbc/datasource ds}
        app (routes/create-app ctx)]

    (testing "POST /api/todos/batch-import persists items concurrently"
      (let [payload {:concurrency 3
                     :todos [{:title "Async Item 1" :description "D1"}
                             {:title "Async Item 2" :description "D2"}
                             {:title "Async Item 3" :description "D3"}]}
            req {:request-method :post
                 :uri            "/api/todos/batch-import"
                 :headers        {"content-type" "application/json"
                                  "accept"       "application/json"}
                 :body-params    payload}
            resp (app req)
            parsed-body (parse-body (:body resp))]
        (is (= 200 (:status resp)))
        (is (= 3 (:imported-count parsed-body)))
        (is (= 3 (count (:todos parsed-body))))
        (let [db-todos (fx/run-sync! (db/query-todos>) ctx)]
          (is (= 3 (count db-todos))))))))
