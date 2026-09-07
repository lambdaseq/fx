(ns com.lambdaseq.fx.jdbc-test
  (:require [clojure.test :refer [deftest is testing]]
            [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.jdbc :as fx-jdbc]
            [com.lambdaseq.fx.jdbc.sql :as sql])
  (:import (java.sql Connection)
           (javax.sql DataSource)))

(def h2-db-spec
  {:dbtype "h2:mem"
   :dbname "test_fx_jdbc;DB_CLOSE_DELAY=-1"})

(def sqlite-db-spec
  {:dbtype "sqlite"
   :dbname "file:test_sqlite_mem?mode=memory&cache=shared"})

(defn- init-accounts-table! [ds]
  (fx/run-sync!
    (fx-jdbc/with-connection> ds
      (fn [_]
        (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS accounts"])
            (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE accounts (id INT PRIMARY KEY, name VARCHAR(255), balance INT)"])))
            (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO accounts (id, name, balance) VALUES (?, ?, ?)" 1 "Alice" 1000])))
            (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO accounts (id, name, balance) VALUES (?, ?, ?)" 2 "Bob" 500]))))))))

(deftest test-datasource-and-connection-lifecycle
  (testing "get-datasource> creates a valid DataSource for H2 and SQLite"
    (let [h2-ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          sqlite-ds (fx/run-sync! (fx-jdbc/get-datasource> sqlite-db-spec))]
      (is (instance? DataSource h2-ds))
      (is (instance? DataSource sqlite-ds))))

  (testing "get-connection> acquires connection from explicit connectable"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (is (instance? Connection conn))
      (is (not (.isClosed ^Connection conn)))
      (fx/run-sync! (fx-jdbc/close-connection> conn))
      (is (.isClosed ^Connection conn))))

  (testing "get-connection> acquires connection from context ::fx-jdbc/datasource"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (-> (fx-jdbc/get-connection>)
                                 (fx/provide-service> ::fx-jdbc/datasource ds)))]
      (is (instance? Connection conn))
      (is (not (.isClosed ^Connection conn)))
      (fx/run-sync! (fx-jdbc/close-connection> conn))))

  (testing "get-connection> returns missing-connectable failure when context and arg are empty"
    (let [res (fx/run-sync! (fx-jdbc/get-connection>))]
      (is (fx/failure? res))
      (is (= :jdbc/missing-connectable (fx/tag res)))))

  (testing "with-connection> binds ::fx-jdbc/datasource in context and closes connection upon success"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn-ref (atom nil)
          res (fx/run-sync!
                (fx-jdbc/with-connection> ds
                  (fn [conn]
                    (reset! conn-ref conn)
                    (fx/map-ctx> (fn [_ ctx]
                                   (is (identical? conn (::fx-jdbc/datasource ctx)))
                                   :success-result)))))]
      (is (= :success-result res))
      (is (some? @conn-ref))
      (is (.isClosed ^Connection @conn-ref))))

  (testing "with-connection> guarantees connection close when inner effect returns an IFailure"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn-ref (atom nil)
          res (fx/run-sync!
                (fx-jdbc/with-connection> ds
                  (fn [conn]
                    (reset! conn-ref conn)
                    (fx/fail> :business/error "insufficient funds"))))]
      (is (fx/failure? res))
      (is (= :business/error (fx/tag res)))
      (is (some? @conn-ref))
      (is (.isClosed ^Connection @conn-ref))))

  (testing "with-connection> guarantees connection close when exception is thrown"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn-ref (atom nil)]
      (is (thrown? RuntimeException
            (fx/run-sync!
              (fx-jdbc/with-connection> ds
                (fn [conn]
                  (reset! conn-ref conn)
                  (fx/map> (fn [_] (throw (RuntimeException. "Unexpected explosion")))))))))
      (is (some? @conn-ref))
      (is (.isClosed ^Connection @conn-ref)))))

(deftest test-statement-execution-and-streaming
  (testing "execute!>, execute-one!>, and plan!> operations with DDL, DML, and querying"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (fx/run-sync!
        (fx-jdbc/with-connection> ds
          (fn [_]
            (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS users"])
                (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE users (id INT PRIMARY KEY, user_name VARCHAR(255), role VARCHAR(255), score INT)"])))
                (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO users (id, user_name, role, score) VALUES (?, ?, ?, ?)" 1 "Alice" "admin" 100])))
                (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO users (id, user_name, role, score) VALUES (?, ?, ?, ?)" 2 "Bob" "dev" 80])))
                (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO users (id, user_name, role, score) VALUES (?, ?, ?, ?)" 3 "Charlie" "dev" 90])))))))

      ;; Query with explicit connectable and context-resolved connectable
      (fx/run-sync!
        (fx-jdbc/with-connection> ds
          (fn [_]
            (fx/map>
              (fn [_]
                (let [users (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT * FROM users ORDER BY id ASC"] {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
                                              (fx/provide-service> ::fx-jdbc/datasource ds)))
                      alice (fx/run-sync! (-> (fx-jdbc/execute-one!> ["SELECT * FROM users WHERE id = ?" 1] {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
                                              (fx/provide-service> ::fx-jdbc/datasource ds)))
                      total-score (fx/run-sync! (-> (fx-jdbc/plan!> ["SELECT score FROM users"] {:builder-fn fx-jdbc/as-unqualified-lower-maps} (fn [acc row] (+ acc (:score row))) 0)
                                                    (fx/provide-service> ::fx-jdbc/datasource ds)))]
                  (is (= 3 (count users)))
                  (is (= "Alice" (:user-name (first users))))
                  (is (= {:id 1 :user-name "Alice" :role "admin" :score 100} alice))
                  (is (= 270 total-score))))))))))

  (testing "typed JDBC error handling on invalid SQL"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          res (fx/run-sync!
                (-> (fx-jdbc/execute!> ["SELECT * FROM non_existent_table"])
                    (fx/provide-service> ::fx-jdbc/datasource ds)))]
      (is (fx/failure? res))
      (is (= :jdbc/error (fx/tag res)))
      (is (map? (fx/error-data res)))
      (is (some? (:message (fx/error-data res))))
      (is (some? (:cause (fx/error-data res))))))

  (testing "typed JDBC error handling on constraint violation"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                  (-> (fx-jdbc/execute!> ["INSERT INTO accounts (id, name, balance) VALUES (?, ?, ?)" 1 "Duplicate" 200])
                      (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))))

  (testing "prepared statement execution with with-prepared-statement>"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (fx/run-sync!
        (fx-jdbc/with-connection> ds
          (fn [conn]
            (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS items"])
                (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE items (id INT, val VARCHAR(255))"])))
                (fx/mapcat> (fn [_]
                              (fx-jdbc/with-prepared-statement> conn ["INSERT INTO items (id, val) VALUES (?, ?)" 1 "abc"]
                                (fn [stmt]
                                  (fx-jdbc/execute!> stmt []))))))))))))

(deftest test-transaction-semantics-and-rollback
  (testing "with-transaction> commits changes upon successful completion"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                  (fx-jdbc/with-connection> ds
                    (fn [conn]
                      (-> (fx-jdbc/with-transaction> conn
                            (fn [_]
                              (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                                  (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 100 WHERE id = 2"]))))))
                          (fx/mapcat> (fn [_]
                                        (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                           {:builder-fn fx-jdbc/as-unqualified-lower-maps})))))))]
        (is (= [{:id 1 :balance 900} {:id 2 :balance 600}] res)))))

  (testing "with-transaction> rolls back changes when inner effect returns an IFailure"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [tx-res (fx/run-sync!
                     (fx-jdbc/with-connection> ds
                       (fn [conn]
                         (fx-jdbc/with-transaction> conn
                           (fn [_]
                             (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                                 (fx/mapcat> (fn [_] (fx/fail> :transfer/insufficient-funds {:account 1})))))))))]
        (is (fx/failure? tx-res))
        (is (= :transfer/insufficient-funds (fx/tag tx-res)))
        ;; Check balances remained unchanged
        (let [balances (fx/run-sync!
                         (fx-jdbc/with-connection> ds
                           (fn [_]
                             (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                {:builder-fn fx-jdbc/as-unqualified-lower-maps}))))]
          (is (= [{:id 1 :balance 1000} {:id 2 :balance 500}] balances))))))

  (testing "with-transaction> rolls back changes when an unhandled exception is thrown"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (is (thrown? RuntimeException
            (fx/run-sync!
              (fx-jdbc/with-connection> ds
                (fn [conn]
                  (fx-jdbc/with-transaction> conn
                    (fn [_]
                      (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                          (fx/map> (fn [_] (throw (RuntimeException. "Network partition simulating failure"))))))))))))
      ;; Check balances remained unchanged
      (let [balances (fx/run-sync!
                       (fx-jdbc/with-connection> ds
                         (fn [_]
                           (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                              {:builder-fn fx-jdbc/as-unqualified-lower-maps}))))]
        (is (= [{:id 1 :balance 1000} {:id 2 :balance 500}] balances)))))

  (testing "with-transaction> rolls back when :rollback-only is true"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                  (fx-jdbc/with-connection> ds
                    (fn [conn]
                      (-> (fx-jdbc/with-transaction> conn {:rollback-only true}
                            (fn [_]
                              (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])))
                          (fx/mapcat> (fn [_]
                                        (fx-jdbc/execute!> ["SELECT id, balance FROM accounts WHERE id = 1"]
                                                           {:builder-fn fx-jdbc/as-unqualified-lower-maps})))))))]
        (is (= [{:id 1 :balance 1000}] res)))))

  (testing "with-transaction> nested transaction savepoints"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                  (fx-jdbc/with-connection> ds
                    (fn [conn]
                      (-> (fx-jdbc/with-transaction> conn
                            (fn [_]
                              (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                                  (fx/mapcat> (fn [_]
                                                (fx-jdbc/with-transaction> conn
                                                  (fn [_]
                                                    (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 500 WHERE id = 2"])
                                                        (fx/mapcat> (fn [_] (fx/fail> :nested/error "nested rollback"))))))))
                                  (fx/catch> {:nested/error (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 100 WHERE id = 2"])}))))
                          (fx/mapcat> (fn [_]
                                        (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                           {:builder-fn fx-jdbc/as-unqualified-lower-maps})))))))]
        (is (= [{:id 1 :balance 900} {:id 2 :balance 600}] res))))))

(deftest test-sql-crud-combinators
  (testing "insert!>, insert-multi!>, query!>, find-by-keys!>, get-by-id!>, update!>, delete!>"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (fx/run-sync!
        (fx-jdbc/with-connection> ds
          (fn [_]
            (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS products"])
                (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE products (id INT PRIMARY KEY, name VARCHAR(255), category VARCHAR(255), price INT)"])))))))

      ;; Test insert!> and insert-multi!>
      (fx/run-sync!
        (-> (sql/insert!> :products {:id 1 :name "Laptop" :category "electronics" :price 1200})
            (fx/mapcat> (fn [_]
                          (sql/insert-multi!> :products
                                              [{:id 2 :name "Mouse" :category "electronics" :price 25}
                                               {:id 3 :name "Desk" :category "furniture" :price 300}
                                               {:id 4 :name "Chair" :category "furniture" :price 150}])))
            (fx/provide-service> ::fx-jdbc/datasource ds)))

      ;; Test query!>
      (let [all-products (fx/run-sync!
                           (-> (sql/query!> ["SELECT * FROM products ORDER BY id ASC"] {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                               (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (= 4 (count all-products))))

      ;; Test find-by-keys!>
      (let [furniture (fx/run-sync!
                        (-> (sql/find-by-keys!> :products {:category "furniture"} {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                            (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (= 2 (count furniture)))
        (is (= #{"Desk" "Chair"} (set (map :name furniture)))))

      ;; Test get-by-id!>
      (let [item (fx/run-sync!
                   (-> (sql/get-by-id!> :products 1 {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                       (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (= {:id 1 :name "Laptop" :category "electronics" :price 1200} item)))

      ;; Test update!>
      (fx/run-sync!
        (-> (sql/update!> :products {:price 1100} ["id = ?" 1])
            (fx/provide-service> ::fx-jdbc/datasource ds)))
      (let [updated-item (fx/run-sync!
                           (-> (sql/get-by-id!> :products 1 {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                               (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (= 1100 (:price updated-item))))

      ;; Test delete!>
      (fx/run-sync!
        (-> (sql/delete!> :products ["id = ?" 2])
            (fx/provide-service> ::fx-jdbc/datasource ds)))
      (let [remaining (fx/run-sync!
                        (-> (sql/query!> ["SELECT * FROM products"] {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                            (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (= 3 (count remaining)))
        (is (not (some #(= 2 (:id %)) remaining)))))))

  (testing "SQLite integration with CRUD and transactions"
    (let [sqlite-ds (fx/run-sync! (fx-jdbc/get-datasource> sqlite-db-spec))]
      (fx/run-sync!
        (fx-jdbc/with-connection> sqlite-ds
          (fn [conn]
            (fx/map>
              (fn [_]
                ;; 1. Initialize table and test CRUD
                (let [init-res (fx/run-sync!
                                 (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS test_users"])
                                     (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE test_users (id INTEGER PRIMARY KEY, username TEXT, active INTEGER)"])))
                                     (fx/mapcat> (fn [_] (sql/insert!> :test_users {:id 1 :username "bob" :active 1})))
                                     (fx/mapcat> (fn [_] (sql/insert!> :test_users {:id 2 :username "carol" :active 0})))
                                     (fx/mapcat> (fn [_] (sql/find-by-keys!> :test_users {:active 1} {:builder-fn fx-jdbc/as-unqualified-lower-maps})))
                                     (fx/provide-service> ::fx-jdbc/datasource conn)))]
                  (is (= 1 (count init-res)))
                  (is (= "bob" (:username (first init-res)))))

                ;; 2. Transaction commit
                (let [tx-commit-res (fx/run-sync!
                                      (-> (fx-jdbc/with-transaction> conn
                                            (fn [_]
                                              (sql/insert!> :test_users {:id 3 :username "dave" :active 1})))
                                          (fx/mapcat> (fn [_]
                                                        (sql/get-by-id!> :test_users 3 {:builder-fn fx-jdbc/as-unqualified-lower-maps})))
                                          (fx/provide-service> ::fx-jdbc/datasource conn)))]
                  (is (= {:id 3 :username "dave" :active 1} tx-commit-res)))

                ;; 3. Transaction rollback on IFailure
                (let [tx-fail-res (fx/run-sync!
                                    (-> (fx-jdbc/with-transaction> conn
                                          (fn [_]
                                            (-> (sql/insert!> :test_users {:id 4 :username "eve" :active 1})
                                                (fx/mapcat> (fn [_] (fx/fail> :tx/aborted "aborting transaction"))))))
                                        (fx/provide-service> ::fx-jdbc/datasource conn)))]
                  (is (fx/failure? tx-fail-res))
                  (is (= :tx/aborted (fx/tag tx-fail-res))))

                ;; 4. Verify eve was not inserted due to rollback
                (let [eve (fx/run-sync!
                            (-> (sql/get-by-id!> :test_users 4 {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                                (fx/provide-service> ::fx-jdbc/datasource conn)))]
                  (is (nil? eve))))))))))

(deftest test-exception-and-failure-propagation
  (testing "catch-jdbc macro converts thrown exceptions to IFailure"
    (let [res1 (fx-jdbc/catch-jdbc (throw (RuntimeException. "Threw directly")))
          res2 (fx-jdbc/catch-jdbc ["SELECT 1"] (throw (java.sql.SQLException. "SQL boom" "42000" 1234)))]
      (is (fx/failure? res1))
      (is (= :jdbc/error (fx/tag res1)))
      (is (= "Threw directly" (:message (fx/error-data res1))))

      (is (fx/failure? res2))
      (is (= :jdbc/error (fx/tag res2)))
      (is (= "SQL boom" (:message (fx/error-data res2))))
      (is (= "42000" (:sqlstate (fx/error-data res2))))
      (is (= 1234 (:error-code (fx/error-data res2))))
      (is (= ["SELECT 1"] (:statement (fx/error-data res2))))))

  (testing "get-datasource> converts spec / driver exceptions to IFailure"
    (let [res (fx/run-sync! (fx-jdbc/get-datasource> {:dbtype "invalid-dbtype-that-does-not-exist"}))]
      (is (fx/failure? res))
      (is (= :jdbc/error (fx/tag res)))
      (is (some? (:cause (fx/error-data res))))))

  (testing "get-connection> converts acquisition errors on closed/invalid connectable to IFailure"
    (let [res (fx/run-sync! (fx-jdbc/get-connection> {:dbtype "invalid-dbtype-does-not-exist"}))]
      (is (fx/failure? res))
      (is (= :jdbc/error (fx/tag res)))))

  (testing "with-connection> converts acquisition failures into IFailure"
    (let [res (fx/run-sync!
                (fx-jdbc/with-connection> {:dbtype "invalid-dbtype-does-not-exist"}
                  (fn [conn]
                    (fx-jdbc/execute!> conn ["SELECT 1"]))))]
      (is (fx/failure? res))
      (is (= :jdbc/error (fx/tag res)))))

  (testing "with-transaction> on closed connection returns IFailure"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (fx/run-sync! (fx-jdbc/close-connection> conn))
      (let [res (fx/run-sync!
                  (fx-jdbc/with-transaction> conn
                    (fn [c] (fx-jdbc/execute!> c ["SELECT 1"]))))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))))

  (testing "execute!> and execute-one!> return IFailure on closed connection and syntax error"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (fx/run-sync! (fx-jdbc/close-connection> conn))
      (let [res1 (fx/run-sync! (fx-jdbc/execute!> conn ["SELECT 1"]))
            res2 (fx/run-sync! (fx-jdbc/execute-one!> conn ["SELECT 1"]))]
        (is (fx/failure? res1))
        (is (= :jdbc/error (fx/tag res1)))
        (is (fx/failure? res2))
        (is (= :jdbc/error (fx/tag res2)))))

    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          syntax-err-res (fx/run-sync! (-> (fx-jdbc/execute!> ["SYNTAX ERROR INVALID SQL"])
                                           (fx/provide-service> ::fx-jdbc/datasource ds)))]
      (is (fx/failure? syntax-err-res))
      (is (= :jdbc/error (fx/tag syntax-err-res)))
      (is (some? (:statement (fx/error-data syntax-err-res))))))

  (testing "plan!> returns IFailure when reduction function throws or SQL is invalid"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      ;; 1. SQL syntax error in plan!>
      (let [res1 (fx/run-sync! (-> (fx-jdbc/plan!> ["SELECT FROM INVALID"] (fn [acc _] acc) 0)
                                   (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res1))
        (is (= :jdbc/error (fx/tag res1))))

      ;; 2. Reduction function throwing an exception
      (let [res2 (fx/run-sync! (-> (fx-jdbc/plan!> ["SELECT * FROM accounts"]
                                                   (fn [_ _] (throw (RuntimeException. "Exception inside reduce-fn")))
                                                   0)
                                   (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res2))
        (is (= :jdbc/error (fx/tag res2)))
        (is (= "Exception inside reduce-fn" (:message (fx/error-data res2)))))))

  (testing "prepare-statement> and with-prepared-statement> return IFailure on closed connection or bad SQL"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (fx/run-sync! (fx-jdbc/close-connection> conn))
      (let [res1 (fx/run-sync! (fx-jdbc/prepare-statement> conn ["SELECT 1"]))
            res2 (fx/run-sync! (fx-jdbc/with-prepared-statement> conn ["SELECT 1"]
                                 (fn [stmt] (fx-jdbc/execute!> stmt []))))]
        (is (fx/failure? res1))
        (is (= :jdbc/error (fx/tag res1)))
        (is (fx/failure? res2))
        (is (= :jdbc/error (fx/tag res2))))))

  (testing "SQL CRUD helpers return IFailure on invalid tables, columns, or constraint violations"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)

      ;; insert!> with invalid column
      (let [res (fx/run-sync! (-> (sql/insert!> :accounts {:non_existent_col "value"})
                                  (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res)))
        (is (= :accounts (get-in (fx/error-data res) [:statement :table]))))

      ;; insert-multi!> with invalid column
      (let [res (fx/run-sync! (-> (sql/insert-multi!> :accounts [{:non_existent_col "value"}])
                                  (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))

      ;; query!> with syntax error
      (let [res (fx/run-sync! (-> (sql/query!> ["SELECT * FROM non_existent_table"])
                                  (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))

      ;; find-by-keys!> on invalid table
      (let [res (fx/run-sync! (-> (sql/find-by-keys!> :non_existent_table {:id 1})
                                  (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))

      ;; get-by-id!> on invalid table
      (let [res (fx/run-sync! (-> (sql/get-by-id!> :non_existent_table 1)
                                  (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))

      ;; update!> with invalid column
      (let [res (fx/run-sync! (-> (sql/update!> :accounts {:bad_col 100} ["id = ?" 1])
                                  (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))

      ;; delete!> with invalid where clause syntax
      (let [res (fx/run-sync! (-> (sql/delete!> :accounts ["INVALID WHERE SYNTAX"])
                                  (fx/provide-service> ::fx-jdbc/datasource ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res)))))))
