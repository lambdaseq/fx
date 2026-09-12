(ns fx.jdbc-test
  (:require [clojure.test :refer [deftest is testing]]
            [fx.core :as fx]
            [fx.jdbc :as fx-jdbc]
            [fx.jdbc.sql :as sql])
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
     (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS accounts"])
         (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE accounts (id INT PRIMARY KEY, name VARCHAR(255), balance INT)"])))
         (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO accounts (id, name, balance) VALUES (?, ?, ?)" 1 "Alice" 1000])))
         (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO accounts (id, name, balance) VALUES (?, ?, ?)" 2 "Bob" 500])))))))

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

  (testing "get-connection> acquires connection from context ::fx-jdbc/connection"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          orig-conn (fx/run-sync! (fx-jdbc/get-connection> ds))
          conn (fx/run-sync! (-> (fx-jdbc/get-connection>)
                                 (fx/provide-service> ::fx-jdbc/connection orig-conn)))]
      (is (instance? Connection conn))
      (is (identical? orig-conn conn))
      (is (not (.isClosed ^Connection conn)))
      (fx/run-sync! (fx-jdbc/close-connection> orig-conn))))

  (testing "get-connection> returns missing-connectable failure when context and arg are empty"
    (let [res (fx/run-sync! (fx-jdbc/get-connection>))]
      (is (fx/failure? res))
      (is (= :jdbc/missing-connectable (fx/tag res)))))

  (testing "with-connection> binds ::fx-jdbc/datasource and ::fx-jdbc/connection in context and closes connection upon success"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn-ref (atom nil)
          res (fx/run-sync!
               (fx-jdbc/with-connection> ds
                 (fx/map-ctx> (fn [_ ctx]
                                (reset! conn-ref (::fx-jdbc/connection ctx))
                                (is (identical? @conn-ref (::fx-jdbc/datasource ctx)))
                                (is (identical? @conn-ref (::fx-jdbc/connection ctx)))
                                :success-result))))]
      (is (= :success-result res))
      (is (some? @conn-ref))
      (is (.isClosed ^Connection @conn-ref))))

  (testing "with-connection> resolves ::fx-jdbc/datasource from context implicitly"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn-ref (atom nil)
          res (fx/run-sync!
               (-> (fx-jdbc/with-connection>
                     (fx/map-ctx> (fn [_ ctx]
                                    (reset! conn-ref (::fx-jdbc/connection ctx))
                                    (is (identical? @conn-ref (::fx-jdbc/datasource ctx)))
                                    :implicit-success)))
                   (fx-jdbc/provide-datasource> ds)))]
      (is (= :implicit-success res))
      (is (some? @conn-ref))
      (is (.isClosed ^Connection @conn-ref))))

  (testing "with-connection> guarantees connection close when inner effect returns an IFailure"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn-ref (atom nil)
          res (fx/run-sync!
               (fx-jdbc/with-connection> ds
                 (-> (fx/map-ctx> (fn [_ ctx]
                                    (reset! conn-ref (::fx-jdbc/connection ctx))))
                     (fx/mapcat> (fn [_] (fx/fail> :business/error "insufficient funds"))))))]
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
                      (-> (fx/map-ctx> (fn [_ ctx]
                                         (reset! conn-ref (::fx-jdbc/connection ctx))))
                          (fx/map> (fn [_] (throw (RuntimeException. "Unexpected explosion")))))))))
      (is (some? @conn-ref))
      (is (.isClosed ^Connection @conn-ref)))))

(deftest test-statement-execution-and-streaming
  (testing "execute!>, execute-one!>, and plan!> operations with DDL, DML, and querying"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (fx/run-sync!
       (fx-jdbc/with-connection> ds
         (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS users"])
             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE users (id INT PRIMARY KEY, user_name VARCHAR(255), role VARCHAR(255), score INT)"])))
             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO users (id, user_name, role, score) VALUES (?, ?, ?, ?)" 1 "Alice" "admin" 100])))
             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO users (id, user_name, role, score) VALUES (?, ?, ?, ?)" 2 "Bob" "dev" 80])))
             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO users (id, user_name, role, score) VALUES (?, ?, ?, ?)" 3 "Charlie" "dev" 90]))))))

      ;; Query with explicit connectable and context-resolved connectable
      (fx/run-sync!
       (fx-jdbc/with-connection> ds
         (fx/map>
          (fn [_]
            (let [users (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT * FROM users ORDER BY id ASC"] {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
                                          (fx-jdbc/provide-datasource> ds)))
                  alice (fx/run-sync! (-> (fx-jdbc/execute-one!> ["SELECT * FROM users WHERE id = ?" 1] {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
                                          (fx-jdbc/provide-datasource> ds)))
                  total-score (fx/run-sync! (-> (fx-jdbc/plan!> ["SELECT score FROM users"] {:builder-fn fx-jdbc/as-unqualified-lower-maps} (fn [acc row] (+ acc (:score row))) 0)
                                                (fx-jdbc/provide-datasource> ds)))]
              (is (= 3 (count users)))
              (is (= "Alice" (:user-name (first users))))
              (is (= {:id 1 :user-name "Alice" :role "admin" :score 100} alice))
              (is (= 270 total-score)))))))))

  (testing "typed JDBC error handling on invalid SQL"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          res (fx/run-sync!
               (-> (fx-jdbc/execute!> ["SELECT * FROM non_existent_table"])
                   (fx-jdbc/provide-datasource> ds)))]
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
                     (fx-jdbc/provide-datasource> ds)))]
        (is (fx/failure? res))
        (is (= :jdbc/error (fx/tag res))))))

  (testing "prepared statement execution with with-prepared-statement>"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (fx/run-sync!
       (fx-jdbc/with-connection> ds
         (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS items"])
             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE items (id INT, val VARCHAR(255))"])))
             (fx/mapcat> (fn [_]
                           (fx-jdbc/with-prepared-statement> ["INSERT INTO items (id, val) VALUES (?, ?)" 1 "abc"]
                             (fn [stmt]
                               (fx-jdbc/execute!> stmt [])))))
             (fx/mapcat> (fn [_]
                            ;; Implicitly resolves ::fx-jdbc/connection in context
                           (fx-jdbc/with-prepared-statement> ["INSERT INTO items (id, val) VALUES (?, ?)" 2 "def"]
                             (fn [stmt]
                               (fx-jdbc/execute!> stmt [])))))))))))

(deftest test-transaction-semantics-and-rollback
  (testing "with-transaction> binds ::fx-jdbc/connection, ::fx-jdbc/datasource, and ::fx-jdbc/transaction in context"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                 (fx-jdbc/with-connection> ds
                   (fx-jdbc/with-transaction>
                     (fx/map-ctx> (fn [_ ctx]
                                    (is (some? (::fx-jdbc/datasource ctx)))
                                    (is (some? (::fx-jdbc/connection ctx)))
                                    (is (some? (::fx-jdbc/transaction ctx)))
                                    (is (identical? (::fx-jdbc/connection ctx) (::fx-jdbc/transaction ctx)))
                                    :tx-ok)))))]
        (is (= :tx-ok res)))))

  (testing "with-transaction> resolves ::fx-jdbc/datasource from context implicitly and scopes lifecycle"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                 (-> (fx-jdbc/with-transaction>
                       (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                           (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 100 WHERE id = 2"])))))
                     (fx-jdbc/provide-datasource> ds)))
            balances (fx/run-sync!
                      (-> (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                             {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                          (fx-jdbc/provide-datasource> ds)))]
        (is (vector? res))
        (is (= [{:id 1 :balance 900} {:id 2 :balance 600}] balances)))))

  (testing "with-transaction> commits changes upon successful completion"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                 (fx-jdbc/with-connection> ds
                   (-> (fx-jdbc/with-transaction>
                         (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 100 WHERE id = 2"])))))
                       (fx/mapcat> (fn [_]
                                     (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                        {:builder-fn fx-jdbc/as-unqualified-lower-maps}))))))]
        (is (= [{:id 1 :balance 900} {:id 2 :balance 600}] res)))))

  (testing "with-transaction> rolls back changes when inner effect returns an IFailure"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [tx-res (fx/run-sync!
                    (fx-jdbc/with-connection> ds
                      (fx-jdbc/with-transaction>
                        (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                            (fx/mapcat> (fn [_] (fx/fail> :transfer/insufficient-funds {:account 1})))))))]
        (is (fx/failure? tx-res))
        (is (= :transfer/insufficient-funds (fx/tag tx-res)))
        ;; Check balances remained unchanged
        (let [balances (fx/run-sync!
                        (fx-jdbc/with-connection> ds
                          (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                             {:builder-fn fx-jdbc/as-unqualified-lower-maps})))]
          (is (= [{:id 1 :balance 1000} {:id 2 :balance 500}] balances))))))

  (testing "with-transaction> rolls back changes when an unhandled exception is thrown"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (is (thrown? RuntimeException
                   (fx/run-sync!
                    (fx-jdbc/with-connection> ds
                      (fx-jdbc/with-transaction>
                        (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                            (fx/map> (fn [_] (throw (RuntimeException. "Network partition simulating failure"))))))))))
      ;; Check balances remained unchanged
      (let [balances (fx/run-sync!
                      (fx-jdbc/with-connection> ds
                        (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                           {:builder-fn fx-jdbc/as-unqualified-lower-maps})))]
        (is (= [{:id 1 :balance 1000} {:id 2 :balance 500}] balances)))))

  (testing "with-transaction> rolls back when :rollback-only is true"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                 (fx-jdbc/with-connection> ds
                   (-> (fx-jdbc/with-transaction> {:rollback-only true}
                         (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"]))
                       (fx/mapcat> (fn [_]
                                     (fx-jdbc/execute!> ["SELECT id, balance FROM accounts WHERE id = 1"]
                                                        {:builder-fn fx-jdbc/as-unqualified-lower-maps}))))))]
        (is (= [{:id 1 :balance 1000}] res)))))

  (testing "with-transaction> nested transaction rollback"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      (let [res (fx/run-sync!
                 (fx-jdbc/with-connection> ds
                   (fx-jdbc/with-transaction>
                     (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 100 WHERE id = 1"])
                         (fx/mapcat> (fn [_]
                                       (fx-jdbc/with-transaction>
                                         (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 500 WHERE id = 2"])
                                             (fx/mapcat> (fn [_] (fx/fail> :nested/error "nested rollback")))))))))))]
        (is (fx/failure? res))
        (is (= :nested/error (fx/tag res)))
        (let [balances (fx/run-sync!
                        (fx-jdbc/with-connection> ds
                          (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                             {:builder-fn fx-jdbc/as-unqualified-lower-maps})))]
          (is (= [{:id 1 :balance 1000} {:id 2 :balance 500}] balances))))))

  (testing "with-transaction> supports isolation options and pipeline threadings"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)
      ;; Standalone with options and explicit connectable
      (let [res (fx/run-sync!
                 (fx-jdbc/with-transaction> ds {:isolation :serializable}
                   (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 50 WHERE id = 1"])))]
        (is (vector? res)))
      ;; Pipeline with options and context datasource
      (let [res (fx/run-sync!
                 (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 50 WHERE id = 1"])
                     (fx-jdbc/with-transaction> {:isolation :read-committed})
                     (fx-jdbc/provide-datasource> ds)))]
        (is (vector? res)))
      (let [bal (fx/run-sync!
                 (-> (fx-jdbc/execute-one!> ["SELECT balance FROM accounts WHERE id = 1"]
                                            {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                     (fx-jdbc/provide-datasource> ds)))]
        (is (= {:balance 1100} bal)))))

  (testing "with-transaction> is an inspectable higher-order effect record"
    (let [tx-eff (fx-jdbc/with-transaction> (fx/succeed> 42))]
      (is (fx/effect? tx-eff))
      (is (= :transaction (fx/tag tx-eff)))))

  (testing "with-transaction> returns missing connectable failure when no datasource or connection available"
    (let [res (fx/run-sync! (fx-jdbc/with-transaction> (fx/succeed> :ok)))]
      (is (fx/failure? res))
      (is (= :jdbc/missing-connectable (fx/tag res))))))

(deftest test-context-provider-functions
  (testing "provide-transaction>, provide-connection>, provide-datasource> standalone and pipeline usage"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (init-accounts-table! ds)
      ;; Standalone
      (let [res-ds (fx/run-sync! (fx-jdbc/provide-datasource> ds (sql/query!> ["SELECT count(*) AS cnt FROM accounts"])))
            res-conn (fx/run-sync! (fx-jdbc/provide-connection> conn (sql/query!> ["SELECT count(*) AS cnt FROM accounts"])))
            res-tx (fx/run-sync! (fx-jdbc/provide-transaction> conn (sql/query!> ["SELECT count(*) AS cnt FROM accounts"])))]
        (is (= [{:CNT 2}] res-ds))
        (is (= [{:CNT 2}] res-conn))
        (is (= [{:CNT 2}] res-tx)))

      ;; Pipeline
      (let [res-ds-pipe (fx/run-sync! (-> (sql/query!> ["SELECT count(*) AS cnt FROM accounts"])
                                          (fx-jdbc/provide-datasource> ds)))
            res-conn-pipe (fx/run-sync! (-> (sql/query!> ["SELECT count(*) AS cnt FROM accounts"])
                                            (fx-jdbc/provide-connection> conn)))
            res-tx-pipe (fx/run-sync! (-> (sql/query!> ["SELECT count(*) AS cnt FROM accounts"])
                                          (fx-jdbc/provide-transaction> conn)))]
        (is (= [{:CNT 2}] res-ds-pipe))
        (is (= [{:CNT 2}] res-conn-pipe))
        (is (= [{:CNT 2}] res-tx-pipe)))

      (fx/run-sync! (fx-jdbc/close-connection> conn)))))

(deftest test-sql-crud-combinators
  (testing "insert!>, insert-multi!>, query!>, find-by-keys!>, get-by-id!>, update!>, delete!>"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (fx/run-sync!
       (fx-jdbc/with-connection> ds
         (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS products"])
             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE products (id INT PRIMARY KEY, name VARCHAR(255), category VARCHAR(255), price INT)"]))))))

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
       (fx/map>
        (fn [_]
              ;; 1. Initialize table and test CRUD
          (let [init-res (fx/run-sync!
                          (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS test_users"])
                              (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE test_users (id INTEGER PRIMARY KEY, username TEXT, active INTEGER)"])))
                              (fx/mapcat> (fn [_] (sql/insert!> :test_users {:id 1 :username "bob" :active 1})))
                              (fx/mapcat> (fn [_] (sql/insert!> :test_users {:id 2 :username "carol" :active 0})))
                              (fx/mapcat> (fn [_] (sql/find-by-keys!> :test_users {:active 1} {:builder-fn fx-jdbc/as-unqualified-lower-maps})))
                              (fx/provide-service> ::fx-jdbc/datasource sqlite-ds)))]
            (is (= 1 (count init-res)))
            (is (= "bob" (:username (first init-res)))))

              ;; 2. Transaction commit
          (let [tx-commit-res (fx/run-sync!
                               (-> (fx-jdbc/with-transaction> sqlite-ds
                                     (sql/insert!> :test_users {:id 3 :username "dave" :active 1}))
                                   (fx/mapcat> (fn [_]
                                                 (sql/get-by-id!> :test_users 3 {:builder-fn fx-jdbc/as-unqualified-lower-maps})))
                                   (fx/provide-service> ::fx-jdbc/datasource sqlite-ds)))]
            (is (= {:id 3 :username "dave" :active 1} tx-commit-res)))

              ;; 3. Transaction rollback on IFailure
          (let [tx-fail-res (fx/run-sync!
                             (-> (fx-jdbc/with-transaction> sqlite-ds
                                   (-> (sql/insert!> :test_users {:id 4 :username "eve" :active 1})
                                       (fx/mapcat> (fn [_] (fx/fail> :tx/aborted "aborting transaction")))))
                                 (fx/provide-service> ::fx-jdbc/datasource sqlite-ds)))]
            (is (fx/failure? tx-fail-res))
            (is (= :tx/aborted (fx/tag tx-fail-res))))

              ;; 4. Verify eve was not inserted due to rollback
          (let [eve (fx/run-sync!
                     (-> (sql/get-by-id!> :test_users 4 {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                         (fx/provide-service> ::fx-jdbc/datasource sqlite-ds)))]
            (is (nil? eve)))))))))

(deftest test-exception-and-failure-propagation
  (testing "jdbc-failure constructs typed IFailure records"
    (let [res1 (fx-jdbc/jdbc-failure (RuntimeException. "Threw directly"))
          res2 (fx-jdbc/jdbc-failure (java.sql.SQLException. "SQL boom" "42000" 1234) ["SELECT 1"])]
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
                 (fx-jdbc/execute!> ["SELECT 1"])))]
      (is (fx/failure? res))
      (is (= :jdbc/error (fx/tag res)))))

  (testing "with-transaction> on closed connection returns IFailure"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (fx/run-sync! (fx-jdbc/close-connection> conn))
      (let [res (fx/run-sync!
                 (fx-jdbc/with-transaction> conn
                   (fx-jdbc/execute!> conn ["SELECT 1"])))]
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

(deftest test-transaction-monitoring-and-advanced-rollback
  (testing "monitoring transaction lifecycle, commit, and rollback via ensure> and catch>"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          events (atom [])]
      (init-accounts-table! ds)

      ;; 1. Successful transaction monitored via ensure>
      (let [tx-success (-> (fx/succeed> :start)
                           (fx/tap> (fn [_] (swap! events conj [:start-tx :tx-1])))
                           (fx/mapcat> (fn [_]
                                         (fx-jdbc/with-transaction> ds
                                           (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 200 WHERE id = 1"])
                                               (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 200 WHERE id = 2"])))
                                               (fx/tap> (fn [_] (swap! events conj [:tx-executed :tx-1])))))))
                           (fx/ensure> (fx/map> (fn [_] (swap! events conj [:tx-finalized :tx-1])))))]
        (let [res (fx/run-sync! tx-success)]
          (is (vector? res))
          (is (= [[:start-tx :tx-1]
                  [:tx-executed :tx-1]
                  [:tx-finalized :tx-1]]
                 @events))))

      ;; 2. Failed transaction monitored via ensure> and catch>
      (reset! events [])
      (let [tx-failure (-> (fx/succeed> :start)
                           (fx/tap> (fn [_] (swap! events conj [:start-tx :tx-2])))
                           (fx/mapcat> (fn [_]
                                         (fx-jdbc/with-transaction> ds
                                           (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 500 WHERE id = 1"])
                                               (fx/mapcat> (fn [_] (fx/fail> :transfer/limit-exceeded {:amount 500})))
                                               (fx/tap> (fn [_] (swap! events conj [:unreachable])))))))
                           (fx/ensure> (fx/map> (fn [_] (swap! events conj [:tx-finalized :tx-2]))))
                           (fx/catch> {:transfer/limit-exceeded
                                       (fx/map> (fn [err]
                                                  (swap! events conj [:rollback-monitored err])
                                                  {:handled true :err err}))}))]
        (let [res (fx/run-sync! tx-failure)
              balances (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                            {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                                         (fx-jdbc/provide-datasource> ds)))]
          (is (= {:handled true :err {:amount 500}} res))
          ;; Verify DB state was rolled back
          (is (= [{:id 1 :balance 800} {:id 2 :balance 700}] balances))
          ;; Verify monitoring captured start, finalization, and rollback handling
          (is (= [[:start-tx :tx-2]
                  [:tx-finalized :tx-2]
                  [:rollback-monitored {:amount 500}]]
                 @events))))))

  (testing "nested savepoint rollback with outer transaction recovery and audit logging"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          audit-log (atom [])]
      (init-accounts-table! ds)

      ;; Create audit table for monitoring
      (fx/run-sync!
       (fx-jdbc/with-connection> ds
         (-> (fx-jdbc/execute!> ["DROP TABLE IF EXISTS transfer_audit"])
             (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["CREATE TABLE transfer_audit (id INT PRIMARY KEY, status VARCHAR(50), note VARCHAR(255))"]))))))

      ;; Outer transaction updates account 1 and inserts an audit log.
      ;; Inner nested transaction attempts an invalid transfer to account 2 and fails.
      ;; Inner savepoint rolls back.
      ;; Outer transaction catches the failure, updates the audit log status to 'failed', and commits.
      (let [workflow
            (fx-jdbc/with-transaction> ds
              (-> (fx-jdbc/execute!> ["INSERT INTO transfer_audit (id, status, note) VALUES (?, ?, ?)" 101 "pending" "Attempting transfer"])
                  (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 50 WHERE id = 1"])))
                  (fx/mapcat>
                   (fn [_]
                      ;; Nested transaction boundary
                     (-> (fx-jdbc/with-transaction>
                           (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 50 WHERE id = 2"])
                               (fx/mapcat> (fn [_] (fx/fail> :transfer/fraud-detected {:account 2 :reason "suspicious activity"})))))
                          ;; Catch inner rollback and recover within outer transaction
                         (fx/catch> {:transfer/fraud-detected
                                     (fx/mapcat> (fn [err]
                                                   (swap! audit-log conj [:inner-rolled-back err])
                                                   (fx-jdbc/execute!> ["UPDATE transfer_audit SET status = ?, note = ? WHERE id = ?" "failed" "Fraud detected on target" 101])))}))))))]
        (let [res (fx/run-sync! workflow)
              balances (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                            {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                                         (fx-jdbc/provide-datasource> ds)))
              audit-records (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT * FROM transfer_audit WHERE id = ?" 101]
                                                                 {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                                              (fx-jdbc/provide-datasource> ds)))]
          (is (vector? res))
          ;; Account 1 was deducted, Account 2 was NOT incremented (rolled back to savepoint)
          (is (= [{:id 1 :balance 950} {:id 2 :balance 500}] balances))
          ;; Audit record reflects the failure recorded by outer transaction
          (is (= [{:id 101 :status "failed" :note "Fraud detected on target"}] audit-records))
          (is (= [[:inner-rolled-back {:account 2 :reason "suspicious activity"}]] @audit-log))))))

  (testing "multi-level nested transaction rollback across multiple savepoints"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))]
      (init-accounts-table! ds)

      ;; Level 1 (root tx) -> Level 2 (nested tx A) -> Level 3 (nested tx B)
      ;; Level 3 succeeds and releases savepoint.
      ;; Level 2 fails and rolls back -> changes in Level 2 AND Level 3 are rolled back to Level 2 savepoint!
      ;; Level 1 catches Level 2 failure and commits Level 1 work.
      (let [multi-level-tx
            (fx-jdbc/with-connection> ds
              (fx-jdbc/with-transaction>
                ;; Level 1: Deduct 10 from Alice (id 1)
                (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 10 WHERE id = 1"])
                    (fx/mapcat>
                     (fn [_]
                       (-> (fx-jdbc/with-transaction>
                              ;; Level 2: Deduct 20 from Bob (id 2)
                             (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 20 WHERE id = 2"])
                                 (fx/mapcat>
                                  (fn [_]
                                      ;; Level 3: Nested success
                                    (fx-jdbc/with-transaction>
                                      (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - 30 WHERE id = 2"]))))
                                  ;; Level 2 fails after Level 3
                                 (fx/mapcat> (fn [_] (fx/fail> :level-2/aborted "aborting level 2")))))
                           (fx/catch> {:level-2/aborted (fx/succeed> :level-2-caught)})))))))]
        (let [res (fx/run-sync! multi-level-tx)
              balances (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                            {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                                         (fx-jdbc/provide-datasource> ds)))]
          (is (= :level-2-caught res))
          ;; Only Level 1 change (Alice -10: 1000 -> 990) was committed.
          ;; Bob's balance remains 500 (both Level 2 and Level 3 rolled back).
          (is (= [{:id 1 :balance 990} {:id 2 :balance 500}] balances))))))

  (testing "connection attribute restoration after transaction rollback"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (init-accounts-table! ds)
      (let [orig-auto-commit (.getAutoCommit conn)
            orig-isolation   (.getTransactionIsolation conn)
            orig-read-only   (.isReadOnly conn)]
        (is (true? orig-auto-commit))

        ;; Run with-transaction> with custom isolation and read-only, forcing failure and rollback
        (let [res (fx/run-sync!
                   (fx-jdbc/with-transaction> conn {:isolation :serializable :read-only true}
                     (-> (fx-jdbc/execute!> conn ["SELECT count(*) FROM accounts"])
                         (fx/mapcat> (fn [_] (fx/fail> :custom/rollback {:reason "test restoration"}))))))]
          (is (fx/failure? res))
          (is (= :custom/rollback (fx/tag res)))
          ;; Connection must remain open because it was passed explicitly
          (is (not (.isClosed conn)))
          ;; Auto-commit, isolation level, and read-only flags must be cleanly restored
          (is (= orig-auto-commit (.getAutoCommit conn)))
          (is (= orig-isolation (.getTransactionIsolation conn)))
          (is (= orig-read-only (.isReadOnly conn))))

        (fx/run-sync! (fx-jdbc/close-connection> conn))
        (is (.isClosed conn)))))

  (testing "monitoring rollback triggered by SQL constraint violation"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          monitored-error (atom nil)]
      (init-accounts-table! ds)

      ;; Transaction attempts duplicate primary key insertion which triggers SQLException rollback
      (let [tx (-> (fx-jdbc/with-transaction> ds
                     (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + 500 WHERE id = 1"])
                         (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["INSERT INTO accounts (id, name, balance) VALUES (?, ?, ?)" 1 "Duplicate Alice" 2000])))))
                   (fx/catch> {:jdbc/error
                               (fx/map> (fn [err-data]
                                          (reset! monitored-error err-data)
                                          :error-monitored-and-handled))}))]
        (let [res (fx/run-sync! tx)
              balances (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                            {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                                         (fx-jdbc/provide-datasource> ds)))]
          (is (= :error-monitored-and-handled res))
          ;; Verify DB update on Alice was rolled back
          (is (= [{:id 1 :balance 1000} {:id 2 :balance 500}] balances))
          ;; Verify error metadata was captured
          (is (some? @monitored-error))
          (is (some? (:message @monitored-error)))
          (is (some? (:sqlstate @monitored-error)))
          (is (= ["INSERT INTO accounts (id, name, balance) VALUES (?, ?, ?)" 1 "Duplicate Alice" 2000]
                 (:statement @monitored-error)))))))

  (testing "IUnwindable rollback during unhandled execution unwinding"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          conn (fx/run-sync! (fx-jdbc/get-connection> ds))]
      (init-accounts-table! ds)
      (.setAutoCommit conn false)
      (let [orig-auto-commit true
            frame (fx-jdbc/->TransactionFrame conn false nil orig-auto-commit nil nil nil {})]
        ;; Execute an update on connection
        (fx/run-sync! (fx-jdbc/execute!> conn ["UPDATE accounts SET balance = 0 WHERE id = 1"]))
        ;; Trigger unwind directly on TransactionFrame
        (fx/-unwind frame (RuntimeException. "Unwinding stack") [])
        ;; Verify auto-commit restored and rollback was executed
        (is (.getAutoCommit conn))
        (let [bal (fx/run-sync! (-> (fx-jdbc/execute-one!> conn ["SELECT balance FROM accounts WHERE id = 1"]
                                                           {:builder-fn fx-jdbc/as-unqualified-lower-maps})))]
          (is (= {:balance 1000} bal)))
        (fx/run-sync! (fx-jdbc/close-connection> conn)))))

  (testing "saga compensating action and audit tracking pattern upon rollback"
    (let [ds (fx/run-sync! (fx-jdbc/get-datasource> h2-db-spec))
          audit-trail (atom [])]
      (init-accounts-table! ds)

      (let [transfer-saga
            (fn [from-id to-id amount]
              (-> (fx/succeed> {:from from-id :to to-id :amount amount})
                  (fx/tap> (fn [ctx] (swap! audit-trail conj [:saga-started ctx])))
                  (fx/mapcat>
                   (fn [_]
                     (fx-jdbc/with-transaction> ds
                       (-> (fx-jdbc/execute-one!> ["SELECT balance FROM accounts WHERE id = ?" from-id]
                                                  {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                           (fx/mapcat>
                            (fn [{:keys [balance]}]
                              (if (< balance amount)
                                (fx/fail> :saga/insufficient-funds {:requested amount :available balance})
                                (-> (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance - ? WHERE id = ?" amount from-id])
                                    (fx/mapcat> (fn [_] (fx-jdbc/execute!> ["UPDATE accounts SET balance = balance + ? WHERE id = ?" amount to-id])))))))))))
                  (fx/catch> {:saga/insufficient-funds
                              (fx/map> (fn [err]
                                         (swap! audit-trail conj [:saga-compensated err])
                                         {:status :failed :reason err}))})))
            res (fx/run-sync! (transfer-saga 1 2 5000))
            balances (fx/run-sync! (-> (fx-jdbc/execute!> ["SELECT id, balance FROM accounts ORDER BY id ASC"]
                                                          {:builder-fn fx-jdbc/as-unqualified-lower-maps})
                                       (fx-jdbc/provide-datasource> ds)))]
        (is (= {:status :failed :reason {:requested 5000 :available 1000}} res))
        ;; Check accounts remain at original 1000 and 500
        (is (= [{:id 1 :balance 1000} {:id 2 :balance 500}] balances))
        ;; Check audit trail accurately captured start and rollback compensation
        (is (= [[:saga-started {:from 1 :to 2 :amount 5000}]
                [:saga-compensated {:requested 5000 :available 1000}]]
               @audit-trail))))))
