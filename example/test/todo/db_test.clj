(ns todo.db-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.jdbc :as fx-jdbc]
            [fx.utils :as fxu]
            [honey.sql :as sql]
            [todo.db :as db])
  (:import (java.util UUID)
           (javax.sql DataSource)))

;; ---------------------------------------------------------------------------
;; Test Setup Helpers
;; ---------------------------------------------------------------------------

(defn- fresh-test-ds []
  (let [url (str "jdbc:sqlite:file:testdb-db-" (UUID/randomUUID) "?mode=memory&cache=shared")
        ds (db/create-datasource url)]
    (fx/run-sync! (db/init-db!> ds))
    ds))

;; ---------------------------------------------------------------------------
;; 1. Pure HoneySQL Query Builders & Row Transformers
;; ---------------------------------------------------------------------------

(deftest test-pure-sql-query-builders
  (testing "sql-insert-todo produces expected HoneySQL map"
    (let [params {:title       "Test Title"
                  :description "Test Description"
                  :completed   false
                  :created-at  "2026-09-08T00:00:00Z"
                  :updated-at  "2026-09-08T00:00:00Z"}
          hsql (db/sql-insert-todo params)
          [sql-str & sql-params] (sql/format hsql)]
      (is (= :todos (:insert-into hsql)))
      (is (re-find #"INSERT INTO todos" sql-str))
      (is (= ["Test Title" "Test Description" 0 "2026-09-08T00:00:00Z" "2026-09-08T00:00:00Z"]
             sql-params))))

  (testing "sql-select-all produces unfiltered and filtered queries"
    (let [unfiltered (db/sql-select-all)
          [sql-unfiltered] (sql/format unfiltered)
          filtered-completed (db/sql-select-all true)
          [sql-comp & params-comp] (sql/format filtered-completed)
          filtered-active (db/sql-select-all false)
          [sql-act & params-act] (sql/format filtered-active)]
      (is (re-find #"SELECT \* FROM todos ORDER BY id ASC" sql-unfiltered))
      (is (re-find #"WHERE completed = \?" sql-comp))
      (is (= [1] params-comp))
      (is (re-find #"WHERE completed = \?" sql-act))
      (is (= [0] params-act))))

  (testing "sql-select-by-id generates parameterized lookup"
    (let [[sql-str & params] (sql/format (db/sql-select-by-id 42))]
      (is (re-find #"SELECT \* FROM todos WHERE id = \?" sql-str))
      (is (= [42] params))))

  (testing "sql-update-todo generates partial update maps"
    (let [[sql-str & params] (sql/format (db/sql-update-todo 10 {:title "Updated" :completed true}))]
      (is (re-find #"UPDATE todos SET" sql-str))
      (is (re-find #"WHERE id = \?" sql-str))
      (is (.contains (vec params) "Updated"))
      (is (.contains (vec params) 1))
      (is (.contains (vec params) 10))))

  (testing "sql-toggle-todo generates CASE expression"
    (let [[sql-str & params] (sql/format (db/sql-toggle-todo 5 "2026-09-08T01:00:00Z"))]
      (is (re-find #"UPDATE todos SET" sql-str))
      (is (re-find #"CASE WHEN completed = \? THEN \? ELSE \? END" sql-str))
      (is (.contains (vec params) 5))
      (is (.contains (vec params) "2026-09-08T01:00:00Z"))))

  (testing "sql-delete-todo generates delete statement"
    (let [[sql-str & params] (sql/format (db/sql-delete-todo 7))]
      (is (re-find #"DELETE FROM todos WHERE id = \?" sql-str))
      (is (= [7] params)))))

(deftest test-row-normalization
  (testing "row->todo converts SQLite integer 1/0 completed column to boolean"
    (is (nil? (db/row->todo nil)))
    (is (= {:id 1 :title "Task" :completed true}
           (db/row->todo {:id 1 :title "Task" :completed 1})))
    (is (= {:id 2 :title "Task" :completed false}
           (db/row->todo {:id 2 :title "Task" :completed 0})))
    (is (= {:id 3 :title "Task" :completed true}
           (db/row->todo {:id 3 :title "Task" :completed true})))
    (is (= {:id 4 :title "Task" :completed false}
           (db/row->todo {:id 4 :title "Task" :completed false})))))

;; ---------------------------------------------------------------------------
;; 2. AST & Effect Structure Inspection (Programs as Data)
;; ---------------------------------------------------------------------------

(deftest test-db-effect-ast-inspection
  (testing "db query effects are valid Effect AST records before execution"
    (let [q-all (db/query-todos>)
          q-by-id (db/query-todo-by-id> 1)
          ins (db/insert-todo!> {:title "Test" :completed false})
          del (db/delete-todo!> 1)]
      (is (fx/effect? q-all))
      (is (fx/effect? q-by-id))
      (is (fx/effect? ins))
      (is (fx/effect? del))
      (is (= [:map-ctx :map] (fxu/effect-tags q-all)))
      (is (= [:map-ctx :map] (fxu/effect-tags q-by-id)))
      (is (= [:map-ctx :mapcat] (fxu/effect-tags ins)))
      (is (= :map-ctx (fx/tag del)))))

  (testing "get-datasource> is a pure effect that returns a DataSource"
    (let [ds-eff (db/get-datasource> db/default-db-spec)]
      (is (fx/effect? ds-eff))
      (is (= [:succeed] (fxu/effect-tags ds-eff)))
      (let [ds (fx/run-sync! ds-eff)]
        (is (instance? DataSource ds))))))

;; ---------------------------------------------------------------------------
;; 3. Direct Execution of Database Effects (Synchronous Runtime)
;; ---------------------------------------------------------------------------

(deftest test-db-crud-operations-direct-execution
  (let [ds (fresh-test-ds)
        now "2026-09-08T00:00:00Z"]

    (testing "insert-todo!> creates record and returns full entity"
      (let [created (fx/run-sync!
                      (-> (db/insert-todo!> {:title       "Write Unit Tests"
                                             :description "For db and domain"
                                             :completed   false
                                             :created-at  now
                                             :updated-at  now})
                          (fx/provide-service> :fx.jdbc/datasource ds)))]
        (is (map? created))
        (is (= 1 (:id created)))
        (is (= "Write Unit Tests" (:title created)))
        (is (= "For db and domain" (:description created)))
        (is (false? (:completed created)))
        (is (= now (:created-at created)))))

    (testing "query-todo-by-id> returns entity when exists, nil when absent"
      (let [found (fx/run-sync!
                    (-> (db/query-todo-by-id> 1)
                        (fx/provide-service> :fx.jdbc/datasource ds)))
            not-found (fx/run-sync!
                        (-> (db/query-todo-by-id> 999)
                            (fx/provide-service> :fx.jdbc/datasource ds)))]
        (is (= 1 (:id found)))
        (is (= "Write Unit Tests" (:title found)))
        (is (nil? not-found))))

    (testing "query-todos> returns list of all entities"
      ;; Insert a second todo
      (fx/run-sync!
        (-> (db/insert-todo!> {:title       "Second Task"
                               :description nil
                               :completed   true
                               :created-at  now
                               :updated-at  now})
            (fx/provide-service> :fx.jdbc/datasource ds)))

      (let [all (fx/run-sync!
                  (-> (db/query-todos>)
                      (fx/provide-service> :fx.jdbc/datasource ds)))
            completed (fx/run-sync!
                        (-> (db/query-todos> true)
                            (fx/provide-service> :fx.jdbc/datasource ds)))
            active (fx/run-sync!
                     (-> (db/query-todos> false)
                         (fx/provide-service> :fx.jdbc/datasource ds)))]
        (is (= 2 (count all)))
        (is (= 1 (count completed)))
        (is (= "Second Task" (:title (first completed))))
        (is (= 1 (count active)))
        (is (= "Write Unit Tests" (:title (first active))))))

    (testing "update-todo!> modifies fields and returns refreshed entity"
      (let [updated (fx/run-sync!
                      (-> (db/update-todo!> 1 {:title       "Updated Title"
                                               :description "Updated Desc"
                                               :completed   true
                                               :updated-at  "2026-09-08T02:00:00Z"})
                          (fx/provide-service> :fx.jdbc/datasource ds)))]
        (is (= 1 (:id updated)))
        (is (= "Updated Title" (:title updated)))
        (is (= "Updated Desc" (:description updated)))
        (is (true? (:completed updated)))
        (is (= "2026-09-08T02:00:00Z" (:updated-at updated)))))

    (testing "toggle-todo!> flips completed boolean"
      (let [toggled1 (fx/run-sync!
                       (-> (db/toggle-todo!> 1 "2026-09-08T03:00:00Z")
                           (fx/provide-service> :fx.jdbc/datasource ds)))
            toggled2 (fx/run-sync!
                       (-> (db/toggle-todo!> 1 "2026-09-08T04:00:00Z")
                           (fx/provide-service> :fx.jdbc/datasource ds)))]
        (is (false? (:completed toggled1)))
        (is (= "2026-09-08T03:00:00Z" (:updated-at toggled1)))
        (is (true? (:completed toggled2)))
        (is (= "2026-09-08T04:00:00Z" (:updated-at toggled2)))))

    (testing "delete-todo!> removes record from database"
      (fx/run-sync!
        (-> (db/delete-todo!> 1)
            (fx/provide-service> :fx.jdbc/datasource ds)))
      (let [remaining (fx/run-sync!
                        (-> (db/query-todos>)
                            (fx/provide-service> :fx.jdbc/datasource ds)))]
        (is (= 1 (count remaining)))
        (is (= 2 (:id (first remaining))))))))

;; ---------------------------------------------------------------------------
;; 4. Context & Dependency Injection Testing
;; ---------------------------------------------------------------------------

(deftest test-db-context-dependency-injection
  (let [ds (fresh-test-ds)]
    (testing "query-todos> resolves datasource from execution context map"
      (let [todos (fx/run-sync! (db/query-todos>) {:fx.jdbc/datasource ds})]
        (is (vector? todos))
        (is (empty? todos))))

    (testing "query-todos> fails with :jdbc/missing-connectable if context lacks datasource"
      (let [res (fx/run-sync! (db/query-todos>))]
        (is (fx/failure? res))
        (is (= :jdbc/missing-connectable (fx/tag res)))))))
