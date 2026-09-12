(ns fx.jdbc
  (:require [fx.core :as fx]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import (java.sql Connection PreparedStatement SQLException)
           (javax.sql DataSource)))

;; ---------------------------------------------------------------------------
;; Result-Set Builders (Re-exported for convenience)
;; ---------------------------------------------------------------------------

(def as-maps rs/as-maps)
(def as-unqualified-maps rs/as-unqualified-maps)
(def as-kebab-maps rs/as-kebab-maps)
(def as-unqualified-kebab-maps rs/as-unqualified-kebab-maps)
(def as-lower-maps rs/as-lower-maps)
(def as-unqualified-lower-maps rs/as-unqualified-lower-maps)
(def as-modified-maps rs/as-modified-maps)

;; ---------------------------------------------------------------------------
;; Typed Failure Constructors
;; ---------------------------------------------------------------------------

(defn jdbc-failure
  "Constructs a typed failure with tag `:jdbc/error` capturing SQLState, error codes,
   and optional statement context."
  ([e]
   (jdbc-failure e nil))
  ([^Throwable e statement]
   (if (instance? SQLException e)
     (let [^SQLException sqle e]
       (fx/make-failure :jdbc/error
                        {:message    (.getMessage sqle)
                         :sqlstate   (.getSQLState sqle)
                         :error-code (.getErrorCode sqle)
                         :statement  statement
                         :cause      sqle}))
     (fx/make-failure :jdbc/error
                      {:message   (or (.getMessage e) (str (type e)))
                       :statement statement
                       :cause     e}))))

(defn missing-connectable-failure
  "Constructs a typed failure when no connectable is supplied and ::datasource is absent in context."
  []
  (fx/make-failure :jdbc/missing-connectable
                   {:message "No connectable provided and ::datasource not found in context"}))

;; ---------------------------------------------------------------------------
;; Connectable Resolution Helpers
;; ---------------------------------------------------------------------------

(defn- connectable-map? [m]
  (and (map? m)
       (not (fx/effect? m))
       (boolean (or (:dbtype m) (:jdbcUrl m) (:dbname m) (:connection-uri m) (:datasource m)))))

(defn- connectable? [x]
  (or (instance? DataSource x)
      (instance? Connection x)
      (instance? PreparedStatement x)
      (connectable-map? x)))

(defn- resolve-connectable [explicit-conn upstream-val ctx]
  (or explicit-conn
      (when (and (some? upstream-val) (not (fx/failure? upstream-val)) (connectable? upstream-val))
        upstream-val)
      (::transaction ctx)
      (:fx.jdbc/transaction ctx)
      (::connection ctx)
      (:fx.jdbc/connection ctx)
      (::datasource ctx)
      (:fx.jdbc/datasource ctx)))

(defn- parse-exec-args-2 [a b]
  (cond
    ;; (execute!> sql-params opts)
    (or (vector? a) (string? a) (instance? PreparedStatement a))
    [nil a b]

    ;; (execute!> connectable sql-params)
    (or (vector? b) (string? b) (instance? PreparedStatement b))
    [a b nil]

    ;; If both are maps:
    (and (map? a) (map? b))
    (if (connectable-map? a)
      [a b nil]
      [nil a b])

    :else
    [a b nil]))

;; ---------------------------------------------------------------------------
;; Connection & Datasource Lifecycle
;; ---------------------------------------------------------------------------

(defn get-datasource>
  "Creates an effect yielding a `javax.sql.DataSource` from a db-spec map or string."
  [db-spec]
  (fx/try> (fx/map> (fn [_] (jdbc/get-datasource db-spec)))
           (fn [e] (jdbc-failure e nil))))

(defn get-connection>
  "Creates an effect acquiring a `java.sql.Connection` from `connectable` (or ::connection / ::datasource in context)."
  ([]
   (get-connection> nil nil))
  ([connectable]
   (get-connection> connectable nil))
  ([connectable opts]
   (fx/try>
    (fx/map-ctx>
     (fn [val ctx]
       (let [target (resolve-connectable connectable val ctx)]
         (if (nil? target)
           (missing-connectable-failure)
           (if (instance? Connection target)
             target
             (if (nil? opts)
               (jdbc/get-connection target)
               (jdbc/get-connection target opts)))))))
    (fn [e] (jdbc-failure e nil)))))

(defn close-connection>
  "Creates an effect closing an `AutoCloseable` connection or statement."
  [conn]
  (fx/try>
   (fx/map>
    (fn [_]
      (when (and conn (instance? java.lang.AutoCloseable conn))
        (.close ^java.lang.AutoCloseable conn))
      nil))
   (fx/succeed> nil)))

(defn with-connection>
  "Executes `eff` within a scoped connection, ensuring deterministic close
   and dynamically binding `::datasource` and `::connection` in context.
   Resolves connectable from explicit `connectable`, upstream value, or context."
  ([eff]
   (with-connection> nil nil eff))
  ([a b]
   (if (fx/effect? a)
     (if (and (map? b) (not (connectable-map? b)))
       (with-connection> nil b a)
       (with-connection> b nil a))
     (if (and (map? a) (not (connectable-map? a)))
       (with-connection> nil a b)
       (with-connection> a nil b))))
  ([connectable opts eff]
   (fx/acquire-release>
    (get-connection> connectable opts)
    (fn [conn]
      (fx/provide> {::datasource conn ::connection conn} eff))
    (fn [conn]
      (close-connection> conn)))))

(defn provide-datasource>
  "Executes `eff` within a dynamic context where `::datasource` is bound to `ds`.
   Supports point-free pipeline threading and standalone wrapping:
     (-> (sql/query!> [\"SELECT * FROM users\"])
         (fx-jdbc/provide-datasource> ds))
     (fx-jdbc/provide-datasource> ds eff)"
  ([ds]
   (fx/provide-service> ::datasource ds))
  ([a b]
   (if (fx/effect? a)
     (fx/provide-service> ::datasource b a)
     (fx/provide-service> ::datasource a b))))

(defn provide-connection>
  "Executes `eff` within a dynamic context where `::connection` is bound to `conn`.
   Supports point-free pipeline threading and standalone wrapping:
     (-> (sql/query!> [\"SELECT * FROM users\"])
         (fx-jdbc/provide-connection> conn))
     (fx-jdbc/provide-connection> conn eff)"
  ([conn]
   (fx/provide-service> ::connection conn))
  ([a b]
   (if (fx/effect? a)
     (fx/provide-service> ::connection b a)
     (fx/provide-service> ::connection a b))))

(defn provide-transaction>
  "Executes `eff` within a dynamic context where `::transaction` is bound to `tx`.
   Supports point-free pipeline threading and standalone wrapping:
     (-> (sql/insert!> :users {:name \"Alice\"})
         (fx-jdbc/provide-transaction> tx))
     (fx-jdbc/provide-transaction> tx eff)"
  ([tx]
   (fx/provide-service> ::transaction tx))
  ([a b]
   (if (fx/effect? a)
     (fx/provide-service> ::transaction b a)
     (fx/provide-service> ::transaction a b))))

;; ---------------------------------------------------------------------------
;; Transactions & Dual Failure Rollback
;; ---------------------------------------------------------------------------

(def ^:private isolation-levels
  {:none             Connection/TRANSACTION_NONE
   :read-uncommitted Connection/TRANSACTION_READ_UNCOMMITTED
   :read-committed   Connection/TRANSACTION_READ_COMMITTED
   :repeatable-read  Connection/TRANSACTION_REPEATABLE_READ
   :serializable     Connection/TRANSACTION_SERIALIZABLE})

(defn- cleanup-tx-connection! [^Connection conn is-new? old-auto-commit old-isolation old-read-only]
  (try
    (when (some? old-isolation)
      (.setTransactionIsolation conn old-isolation))
    (catch Throwable _ nil))
  (try
    (when (some? old-read-only)
      (.setReadOnly conn old-read-only))
    (catch Throwable _ nil))
  (try
    (when (true? old-auto-commit)
      (.setAutoCommit conn true))
    (catch Throwable _ nil))
  (try
    (when is-new?
      (.close conn))
    (catch Throwable _ nil)))

(defn- rollback-tx! [^Connection conn is-new? sp old-auto-commit old-isolation old-read-only]
  (try
    (if (some? sp)
      (.rollback conn sp)
      (.rollback conn))
    (catch Throwable _ nil))
  (cleanup-tx-connection! conn is-new? old-auto-commit old-isolation old-read-only))

(defn- commit-tx! [^Connection conn is-new? sp old-auto-commit old-isolation old-read-only opts]
  (if (true? (:rollback-only opts))
    (do
      (rollback-tx! conn is-new? sp old-auto-commit old-isolation old-read-only)
      nil)
    (try
      (if (some? sp)
        (try (.releaseSavepoint conn sp) (catch Throwable _ nil))
        (.commit conn))
      (cleanup-tx-connection! conn is-new? old-auto-commit old-isolation old-read-only)
      nil
      (catch Throwable e
        (rollback-tx! conn is-new? sp old-auto-commit old-isolation old-read-only)
        (jdbc-failure e nil)))))

(defrecord TransactionFrame [conn is-new? sp old-auto-commit old-isolation old-read-only opts original-context]
  fx/IContinuation
  (-resume [_ res _current-context stack]
    (if (fx/failure? res)
      (do
        (rollback-tx! conn is-new? sp old-auto-commit old-isolation old-read-only)
        [nil res original-context stack])
      (if-let [err (commit-tx! conn is-new? sp old-auto-commit old-isolation old-read-only opts)]
        [nil err original-context stack]
        [nil res original-context stack])))

  fx/IUnwindable
  (-unwind [_ _ _]
    (rollback-tx! conn is-new? sp old-auto-commit old-isolation old-read-only)
    nil))

(defrecord TransactionEffect [tag prev-effect data connectable opts eff]
  fx/ITagged
  (tag [_] tag)

  fx/IEffect
  (prev-effect [_] prev-effect)

  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (fx/failure? val)
        [nil val context stack]
        (let [target (resolve-connectable connectable val context)]
          (if (nil? target)
            [nil (missing-connectable-failure) context stack]
            (try
              (let [is-conn? (instance? Connection target)
                    is-new?  (not is-conn?)
                    ^Connection conn (if is-conn?
                                       target
                                       (jdbc/get-connection target (dissoc (or opts {}) :isolation :read-only :rollback-only)))
                    nested?  (and is-conn? (not (.getAutoCommit conn)))
                    sp       (when nested? (.setSavepoint conn))
                    old-auto-commit (when-not nested? (if is-conn? (.getAutoCommit conn) true))
                    old-isolation   (when (and (not nested?) (some? (:isolation opts)))
                                      (.getTransactionIsolation conn))
                    old-read-only   (when (and (not nested?) (some? (:read-only opts)))
                                      (.isReadOnly conn))]
                (when-not nested?
                  (when-let [iso (:isolation opts)]
                    (.setTransactionIsolation conn (get isolation-levels iso iso)))
                  (when-let [ro (:read-only opts)]
                    (.setReadOnly conn (boolean ro)))
                  (when (.getAutoCommit conn)
                    (.setAutoCommit conn false)))
                (let [child-ctx (assoc context
                                       ::datasource conn
                                       ::connection conn
                                       ::transaction conn
                                       :fx.jdbc/datasource conn
                                       :fx.jdbc/connection conn
                                       :fx.jdbc/transaction conn)
                      inner-eff (cond
                                  (and (some? val) (fx/effect? eff) (nil? (:prev-effect eff)))
                                  (fx/chain> (fx/succeed> val) eff)

                                  (fx/effect? eff)
                                  eff

                                  :else
                                  (fx/succeed> val))]
                  [inner-eff nil child-ctx (conj stack (->TransactionFrame conn is-new? sp old-auto-commit old-isolation old-read-only opts context))]))
              (catch Throwable e
                [nil (jdbc-failure e nil) context stack]))))))))

(defn with-transaction>
  "Executes `tx-eff` within a JDBC transaction boundary as a higher-order effect.
   Automatically commits on effect success, and rolls back if the inner effect
   evaluates to an `IFailure` OR throws an exception.
   Supports transaction options: `:isolation`, `:read-only`, and `:rollback-only`.
   Connectable can be passed explicitly or resolved implicitly from context."
  ([tx-eff]
   (with-transaction> nil nil tx-eff))
  ([a b]
   (if (fx/effect? a)
     (if (and (map? b) (not (connectable-map? b)))
       (with-transaction> nil b a)
       (with-transaction> b nil a))
     (if (and (map? a) (not (connectable-map? a)))
       (with-transaction> nil a b)
       (with-transaction> a nil b))))
  ([connectable opts tx-eff]
   (let [eff     (if (fx/effect? connectable) connectable tx-eff)
         conn    (if (fx/effect? connectable) opts connectable)
         options (if (fx/effect? connectable) tx-eff opts)]
     (->TransactionEffect :transaction nil {:connectable conn :opts options :eff eff} conn options eff))))

;; ---------------------------------------------------------------------------
;; Statement & Query Execution
;; ---------------------------------------------------------------------------

(defn execute!>
  "Executes a SQL statement returning a vector of row maps or update counts.
   Supports explicit connectable or context-resolved `::connection` / `::datasource`."
  ([sql-params]
   (execute!> nil sql-params nil))
  ([a b]
   (let [[conn sql-params opts] (parse-exec-args-2 a b)]
     (execute!> conn sql-params opts)))
  ([connectable sql-params opts]
   (fx/try>
    (fx/map-ctx>
     (fn [val ctx]
       (let [target (resolve-connectable connectable val ctx)]
         (if (nil? target)
           (missing-connectable-failure)
           (if (nil? opts)
             (jdbc/execute! target sql-params)
             (jdbc/execute! target sql-params opts))))))
    (fn [e] (jdbc-failure e sql-params)))))

(defn execute-one!>
  "Executes a SQL statement returning the first row map or `nil`.
   Supports explicit connectable or context-resolved `::connection` / `::datasource`."
  ([sql-params]
   (execute-one!> nil sql-params nil))
  ([a b]
   (let [[conn sql-params opts] (parse-exec-args-2 a b)]
     (execute-one!> conn sql-params opts)))
  ([connectable sql-params opts]
   (fx/try>
    (fx/map-ctx>
     (fn [val ctx]
       (let [target (resolve-connectable connectable val ctx)]
         (if (nil? target)
           (missing-connectable-failure)
           (if (nil? opts)
             (jdbc/execute-one! target sql-params)
             (jdbc/execute-one! target sql-params opts))))))
    (fn [e] (jdbc-failure e sql-params)))))

(defn- parse-plan-args-4 [a b c d]
  (if (fn? c)
    (if (or (vector? a) (string? a) (instance? PreparedStatement a))
      [nil a b c d]
      [a b nil c d])
    [nil a b c d]))

(defn plan!>
  "Executes a streaming lazy reduction over SQL results via `next.jdbc/plan`.
   Evaluates `(reduce reduce-fn initial-acc (next.jdbc/plan ...))` inside the effect.
   Supports explicit connectable or context-resolved `::connection` / `::datasource`."
  ([sql-params reduce-fn initial-acc]
   (plan!> nil sql-params nil reduce-fn initial-acc))
  ([a b c d]
   (let [[conn sql-params opts reduce-fn initial-acc] (parse-plan-args-4 a b c d)]
     (plan!> conn sql-params opts reduce-fn initial-acc)))
  ([connectable sql-params opts reduce-fn initial-acc]
   (fx/try>
    (fx/map-ctx>
     (fn [val ctx]
       (let [target (resolve-connectable connectable val ctx)]
         (if (nil? target)
           (missing-connectable-failure)
           (let [plan (if (nil? opts)
                        (jdbc/plan target sql-params)
                        (jdbc/plan target sql-params opts))]
             (reduce reduce-fn initial-acc plan))))))
    (fn [e] (jdbc-failure e sql-params)))))

(defn prepare-statement>
  "Creates an effect preparing a `java.sql.PreparedStatement` from `connectable` (or ::connection / ::datasource in context)."
  ([sql-params]
   (prepare-statement> nil sql-params nil))
  ([a b]
   (let [[conn sql-params opts] (parse-exec-args-2 a b)]
     (prepare-statement> conn sql-params opts)))
  ([connectable sql-params opts]
   (fx/try>
    (fx/map-ctx>
     (fn [val ctx]
       (let [target (resolve-connectable connectable val ctx)]
         (if (nil? target)
           (missing-connectable-failure)
           (if (nil? opts)
             (jdbc/prepare target sql-params)
             (jdbc/prepare target sql-params opts))))))
    (fn [e] (jdbc-failure e sql-params)))))

(defn with-prepared-statement>
  "Prepares a statement, executes `use-eff-fn` with the prepared statement, and guarantees closure."
  ([sql-params use-eff-fn]
   (with-prepared-statement> nil sql-params nil use-eff-fn))
  ([a b c]
   (if (fn? c)
     (let [[conn sql-params opts] (parse-exec-args-2 a b)]
       (with-prepared-statement> conn sql-params opts c))
     (with-prepared-statement> nil a b c)))
  ([connectable sql-params opts use-eff-fn]
   (fx/acquire-release>
    (prepare-statement> connectable sql-params opts)
    (fn [stmt]
      (use-eff-fn stmt))
    (fn [stmt]
      (close-connection> stmt)))))
