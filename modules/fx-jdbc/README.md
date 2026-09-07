# fx/jdbc

Effectful, purely functional JDBC database access for the `fx` effect system, built on [`next.jdbc`](https://github.com/seancorfield/next-jdbc).

`fx-jdbc` bridges relational database operations into pure `fx` effect pipelines. It manages connection lifecycles, statement preparation, streaming result reductions, CRUD operations, and atomic transactions that automatically roll back on exceptions or functional `IFailure` returns.

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {io.github.conjurernix/fx.jdbc {:mvn/version "0.0.1-alpha"}}}
;; or local module coordinate
{:deps {fx/jdbc {:mvn/version "0.0.1-alpha"}}}
```

Requires `fx/core` (`io.github.conjurernix/fx.core`).

## Philosophy & Key Concepts

- **Effects as Blueprints**: Database calls are descriptions of queries and transactions represented as immutable data records, executed only at pipeline boundaries via `fx/run-sync!` or `fx/run-async!`.
- **Automatic Resource Lifecycles**: Connection pools and statements are acquired and closed deterministically via bracket combinators (`acquire-release>`), avoiding connection leaks even under unhandled failures.
- **Dual-Failure Transaction Rollbacks**: Transactions wrapped in `with-transaction>` automatically roll back if an exception is thrown **or** if any step in the pipeline returns an `IFailure`.
- **Context-Driven Connection Resolution**: Operations resolve their target connection from an explicit argument, an upstream piped value, or dynamically from `::fx-jdbc/datasource` (`:fx.jdbc/datasource`) in the execution context.
- **Structured Failure Payloads**: SQL exceptions are captured as typed `:jdbc/error` failure records containing `:sqlstate`, `:error-code`, `:statement`, and `:cause`.

---

## Quickstart

```clojure
(ns example.db
  (:require [fx.core :as fx]
            [fx.jdbc :as fx-jdbc]
            [fx.jdbc.sql :as sql]))

(def db-spec
  {:dbtype "h2:mem"
   :dbname "example_db;DB_CLOSE_DELAY=-1"})

(def program
  (fx/acquire-release>
    (fx-jdbc/get-datasource> db-spec)
    (fn [ds]
      (fx-jdbc/with-connection> ds
        (fn [_conn]
          (-> (fx-jdbc/execute!> ["CREATE TABLE users (id INT PRIMARY KEY, name VARCHAR(255), email VARCHAR(255))"])
              (fx/mapcat> (fn [_] (sql/insert!> :users {:id 1 :name "Alice" :email "alice@example.com"})))
              (fx/mapcat> (fn [_] (sql/get-by-id!> :users 1 {:builder-fn fx-jdbc/as-unqualified-kebab-maps})))))))
    (fn [_ds]
      (fx/succeed> nil))))

;; Evaluate the effect pipeline
(fx/run-sync! program)
;; => {:id 1, :name "Alice", :email "alice@example.com"}
```

---

## API Reference

### Connection & Datasource Lifecycle (`fx.jdbc`)

| Function | Signature | Description |
|---|---|---|
| `get-datasource>` | `[db-spec]` | Creates an effect yielding a `javax.sql.DataSource` from a db-spec map or connection URL string. |
| `get-connection>` | `([] [connectable] [connectable opts])` | Creates an effect acquiring a `java.sql.Connection` from explicit connectable or context `::fx-jdbc/datasource`. |
| `close-connection>` | `[conn]` | Creates an effect safely closing an `AutoCloseable` connection or statement. |
| `with-connection>` | `([use-eff-fn] [connectable use-eff-fn] [connectable opts use-eff-fn])` | Scopes connection acquisition, binds `::fx-jdbc/datasource` into context, executes `use-eff-fn`, and guarantees close. |

### Transactions (`fx.jdbc`)

| Function | Signature | Description |
|---|---|---|
| `with-transaction>` | `([tx-eff-fn] [connectable tx-eff-fn] [opts tx-eff-fn] [connectable opts tx-eff-fn])` | Executes `tx-eff-fn` within a transactional boundary. Automatically commits on success and rolls back on exception or `IFailure`. |

#### Transaction Options Map

- `:isolation` - Isolation level: `:none`, `:read-uncommitted`, `:read-committed`, `:repeatable-read`, `:serializable`, or raw integer code.
- `:read-only` - Boolean flag setting connection read-only mode for the duration of the transaction.
- `:rollback-only` - Boolean flag forcing rollback at the end of the transaction even if the effect succeeded.

Nested calls to `with-transaction>` automatically leverage JDBC savepoints on the existing connection.

### Statement & Query Execution (`fx.jdbc`)

| Function | Signature | Description |
|---|---|---|
| `execute!>` | `([sql-params] [conn sql-params] [sql-params opts] [conn sql-params opts])` | Executes a SQL statement, returning a vector of row maps or update counts. |
| `execute-one!>` | `([sql-params] [conn sql-params] [sql-params opts] [conn sql-params opts])` | Executes a SQL statement, returning the first row map or `nil`. |
| `plan!>` | `([sql-params reduce-fn init] [conn sql-params reduce-fn init] [sql-params opts reduce-fn init] [conn sql-params opts reduce-fn init])` | Streams query results lazily through `(reduce reduce-fn init plan)` without loading all rows into memory. |
| `prepare-statement>` | `([sql-params] [conn sql-params] [sql-params opts] [conn sql-params opts])` | Creates an effect yielding a `java.sql.PreparedStatement`. |
| `with-prepared-statement>` | `([sql-params use-eff-fn] [conn sql-params use-eff-fn] [sql-params opts use-eff-fn] [conn sql-params opts use-eff-fn])` | Prepares a statement, passes it to `use-eff-fn`, and guarantees statement closure upon completion. |

### Result-Set Builders (`fx.jdbc`)

Re-exported from `next.jdbc.result-set` for passing into `:builder-fn` options:

- `as-maps` - Qualified Clojure maps with table and column names (default `next.jdbc` format).
- `as-unqualified-maps` - Unqualified Clojure maps with column names as keys.
- `as-kebab-maps` - Qualified Clojure maps with column names transformed to kebab-case keywords.
- `as-unqualified-kebab-maps` - Unqualified Clojure maps with column names transformed to kebab-case keywords.
- `as-lower-maps` - Qualified Clojure maps with lowercase column names.
- `as-unqualified-lower-maps` - Unqualified Clojure maps with lowercase column names.
- `as-modified-maps` - Configurable builder factory for customized row and column transformations.

---

### SQL CRUD Combinators (`fx.jdbc.sql`)

High-level helpers wrapping `next.jdbc.sql` that accept an optional leading connectable or resolve `::fx-jdbc/datasource` from the ambient effect context:

| Function | Signature | Description |
|---|---|---|
| `insert!>` | `([table row] [conn table row] [table row opts] [conn table row opts])` | Inserts a single map into `table`. Returns generated keys or row map. |
| `insert-multi!>` | `([table rows] [conn table rows] [table cols rows] [table rows opts] [conn table rows opts] [table cols rows opts] [conn table cols rows opts])` | Batch inserts multiple rows into `table`. |
| `query!>` | `([sql-params] [conn sql-params] [sql-params opts] [conn sql-params opts])` | Executes a query and returns a vector of row maps. |
| `find-by-keys!>` | `([table col-map] [conn table col-map] [table col-map opts] [conn table col-map opts])` | Queries rows matching column equality map `{column value}`. |
| `get-by-id!>` | `([table id] [conn table id] [table id opts] [conn table id opts] [table id pk-col opts] [conn table id pk-col opts])` | Retrieves a single row matching primary key `id` (defaults to `:id` column). |
| `update!>` | `([table col-map where-params] [conn table col-map where-params] [table col-map where-params opts] [conn table col-map where-params opts])` | Updates rows matching `where-params` with values in `col-map`. |
| `delete!>` | `([table where-params] [conn table where-params] [table where-params opts] [conn table where-params opts])` | Deletes rows matching `where-params`. |

---

## Failure Model

`fx-jdbc` converts Java database exceptions into structured `Failure` records:

### `:jdbc/error`

Generated whenever `SQLException` or other database throwables are caught.

```clojure
{:tag :jdbc/error
 :error-data {:message "Table \"USERS\" not found"
              :sqlstate "42S02"
              :error-code 42102
              :statement ["SELECT * FROM users"]
              :cause #error [...]}}
```

### `:jdbc/missing-connectable`

Generated when a JDBC combinator is called without an explicit connection argument, no connectable was passed upstream in the pipeline, and `::fx-jdbc/datasource` is missing from the effect execution context.

```clojure
{:tag :jdbc/missing-connectable
 :error-data {:message "No connectable provided and ::datasource not found in context"}}
```

---

## Detailed Usage Patterns

### 1. Connection Scoping & Context Resolution

When executing queries inside `with-connection>`, the acquired connection is automatically injected into the effect context under `::fx-jdbc/datasource` (`:fx.jdbc/datasource`). All nested SQL operations resolve this connection automatically:

```clojure
(defn find-active-users []
  (fx-jdbc/with-connection> ds
    (fn [_conn]
      (-> (sql/query!> ["SELECT * FROM users WHERE active = ?" true]
                       {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
          (fx/map> (fn [users]
                     (mapv :email users)))))))
```

### 2. Transactional Rollback on Functional Failure

`with-transaction>` enforces atomic transaction semantics over pure functional effect pipelines. If any step yields an `IFailure`, the entire transaction rolls back automatically without needing Java exceptions:

```clojure
(defn transfer-funds [from-id to-id amount]
  (fx-jdbc/with-transaction> ds {:isolation :serializable}
    (fn [_conn]
      (-> (sql/get-by-id!> :accounts from-id)
          (fx/mapcat> (fn [sender]
                        (if (< (:balance sender) amount)
                          ;; Returning an IFailure triggers automatic rollback
                          (fx/fail> :transfer/insufficient-funds
                                    {:sender-id from-id :balance (:balance sender) :required amount})
                          (-> (sql/update!> :accounts {:balance (- (:balance sender) amount)} {:id from-id})
                              (fx/mapcat> (fn [_] (sql/get-by-id!> :accounts to-id)))
                              (fx/mapcat> (fn [receiver]
                                            (sql/update!> :accounts {:balance (+ (:balance receiver) amount)} {:id to-id})))
                              (fx/map> (fn [_] {:status :transferred :amount amount}))))))))))
```

### 3. Memory-Efficient Streaming with `plan!>`

For large datasets, `plan!>` processes rows via a reducing function over `next.jdbc/plan` without materializing the full result set in memory:

```clojure
(defn compute-total-payroll [department-id]
  (fx-jdbc/plan!> ds
                  ["SELECT salary FROM employees WHERE department_id = ?" department-id]
                  (fn [acc row] (+ acc (:salary row)))
                  0))
```

### 4. Statement Preparation with Resource Management

`with-prepared-statement>` handles `PreparedStatement` lifecycle deterministically:

```clojure
(defn batch-update-status [status user-ids]
  (fx-jdbc/with-prepared-statement> ds
    ["UPDATE users SET status = ? WHERE id = ?"]
    (fn [stmt]
      (fx/all>
        (mapv (fn [uid]
                (fx-jdbc/execute!> stmt [status uid]))
              user-ids)))))
```
