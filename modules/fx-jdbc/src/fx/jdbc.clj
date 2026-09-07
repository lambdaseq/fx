(ns fx.jdbc
  (:require [fx.core :as fx]
            [next.jdbc :as jdbc]
            [next.jdbc.result-set :as rs])
  (:import (fx.core IEffect ITagged StepEffectFrame)
           (java.sql Connection PreparedStatement SQLException)
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
;; Typed Error Constructors
;; ---------------------------------------------------------------------------

(defn jdbc-error
  "Constructs a typed failure with tag `:jdbc/error` capturing SQLState, error codes,
   and optional statement context."
  ([e]
   (jdbc-error e nil))
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

(defn missing-connectable-error
  "Constructs a typed failure when no connectable is supplied and ::datasource is absent in context."
  []
  (fx/make-failure :jdbc/missing-connectable
                   {:message "No connectable provided and ::datasource not found in context"}))

(defmacro catch-jdbc
  "Evaluates `body` within a try/catch block, converting any thrown `Throwable`
   into a typed `:jdbc/error` failure."
  ([body]
   `(try
      ~body
      (catch Throwable e#
        (jdbc-error e# nil))))
  ([statement-context body & more-body]
   `(try
      ~body
      ~@more-body
      (catch Throwable e#
        (jdbc-error e# ~statement-context)))))

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
      (::datasource ctx)))

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
  (fx/try> (fn [] (jdbc/get-datasource db-spec))
           (fn [e] (jdbc-error e nil))))

(defn get-connection>
  "Creates an effect acquiring a `java.sql.Connection` from `connectable` (or ::datasource in context)."
  ([]
   (get-connection> nil nil))
  ([connectable]
   (get-connection> connectable nil))
  ([connectable opts]
   (fx/map-ctx>
     (fn [val ctx]
       (catch-jdbc
         (let [target (resolve-connectable connectable val ctx)]
           (if (nil? target)
             (missing-connectable-error)
             (if (nil? opts)
               (jdbc/get-connection target)
               (jdbc/get-connection target opts)))))))))

(defn close-connection>
  "Creates an effect closing an `AutoCloseable` connection or statement."
  [conn]
  (fx/map>
    (fn [_]
      (when (and conn (instance? java.lang.AutoCloseable conn))
        (try
          (.close ^java.lang.AutoCloseable conn)
          (catch Throwable _ nil)))
      nil)))

(defn with-connection>
  "Executes `use-eff-fn` within a scoped connection, ensuring deterministic close
   and dynamically binding ::datasource in context."
  ([use-eff-fn]
   (with-connection> nil nil use-eff-fn))
  ([connectable use-eff-fn]
   (with-connection> connectable nil use-eff-fn))
  ([connectable opts use-eff-fn]
   (fx/acquire-release>
     (get-connection> connectable opts)
     (fn [conn]
       (let [eff (use-eff-fn conn)]
         (fx/provide-service> ::datasource conn eff)))
     (fn [conn]
       (close-connection> conn)))))

;; ---------------------------------------------------------------------------
;; Transactions & Dual Failure Rollback
;; ---------------------------------------------------------------------------

(defn- apply-tx-options! [^Connection conn opts]
  (when (contains? opts :read-only)
    (.setReadOnly conn (boolean (:read-only opts))))
  (when (contains? opts :isolation)
    (let [iso (:isolation opts)
          iso-val (if (keyword? iso)
                    (case iso
                      :none Connection/TRANSACTION_NONE
                      :read-uncommitted Connection/TRANSACTION_READ_UNCOMMITTED
                      :read-committed Connection/TRANSACTION_READ_COMMITTED
                      :repeatable-read Connection/TRANSACTION_REPEATABLE_READ
                      :serializable Connection/TRANSACTION_SERIALIZABLE)
                    iso)]
      (.setTransactionIsolation conn iso-val))))

(defn- begin-tx! [^Connection conn opts]
  (if-not (.getAutoCommit conn)
    ;; Already inside an active transaction -> use savepoint
    (let [sp (.setSavepoint conn)]
      {:type      :savepoint
       :conn      conn
       :savepoint sp
       :status    (atom :active)})
    ;; Root transaction
    (let [orig-auto-commit (.getAutoCommit conn)
          orig-read-only (.isReadOnly conn)
          orig-isolation (.getTransactionIsolation conn)]
      (.setAutoCommit conn false)
      (apply-tx-options! conn opts)
      {:type             :root
       :conn             conn
       :orig-auto-commit orig-auto-commit
       :orig-read-only   orig-read-only
       :orig-isolation   orig-isolation
       :status           (atom :active)})))

(defn- commit-tx! [{:keys [type ^Connection conn savepoint status]}]
  (when (= @status :active)
    (if (= type :savepoint)
      (try (.releaseSavepoint conn savepoint) (catch Throwable _ nil))
      (.commit conn))
    (reset! status :committed)))

(defn- rollback-tx! [{:keys [type ^Connection conn savepoint status]}]
  (when (= @status :active)
    (if (= type :savepoint)
      (try (.rollback conn savepoint) (catch Throwable _ nil))
      (try (.rollback conn) (catch Throwable _ nil)))
    (reset! status :rolled-back)))

(defn- cleanup-tx-state! [{:keys [type ^Connection conn orig-auto-commit orig-read-only orig-isolation status] :as tx-state}]
  (when (= @status :active)
    (rollback-tx! tx-state))
  (when (= type :root)
    (try
      (when (some? orig-read-only) (.setReadOnly conn orig-read-only))
      (when (some? orig-isolation) (.setTransactionIsolation conn orig-isolation))
      (when (some? orig-auto-commit) (.setAutoCommit conn orig-auto-commit))
      (catch Throwable _ nil))))

(defn- run-tx-on-conn> [^Connection conn opts tx-eff-fn]
  (fx/acquire-release>
    (fx/map> (fn [_]
               (catch-jdbc
                 (begin-tx! conn opts))))
    (fn [tx-state]
      (if (fx/failure? tx-state)
        (fx/succeed> tx-state)
        (-> (tx-eff-fn conn)
            (fx/provide-service> ::datasource conn)
            (fx/match>
              (fn [failure-val]
                (rollback-tx! tx-state)
                failure-val)
              (fn [success-val]
                (if (:rollback-only opts)
                  (do
                    (rollback-tx! tx-state)
                    success-val)
                  (try
                    (commit-tx! tx-state)
                    success-val
                    (catch Throwable e
                      (rollback-tx! tx-state)
                      (jdbc-error e nil)))))))))
    (fn [tx-state]
      (fx/map> (fn [_]
                 (when-not (fx/failure? tx-state)
                   (cleanup-tx-state! tx-state)))))))

(defrecord TransactionEffect [tag prev-effect data connectable opts tx-eff-fn]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (StepEffectFrame. (assoc this :prev-effect nil)))]
      (if (fx/failure? val)
        [nil val context stack]
        (try
          (let [target (resolve-connectable connectable val context)]
            (if (nil? target)
              [nil (missing-connectable-error) context stack]
              (let [eff (if (instance? Connection target)
                          (run-tx-on-conn> target opts tx-eff-fn)
                          (with-connection> target opts
                                            (fn [conn]
                                              (run-tx-on-conn> conn opts tx-eff-fn))))]
                [eff nil context stack])))
          (catch Throwable e
            [nil (jdbc-error e nil) context stack]))))))

(defn with-transaction>
  "Executes `tx-eff-fn` within a JDBC transaction boundary.
   Automatically commits on effect success, and rolls back if the inner effect
   evaluates to an `IFailure` OR throws an exception.
   Supports transaction options: `:isolation`, `:read-only`, and `:rollback-only`."
  ([tx-eff-fn]
   (with-transaction> nil nil tx-eff-fn))
  ([a b]
   (if (fn? b)
     (if (and (map? a) (not (connectable-map? a)))
       (with-transaction> nil a b)
       (with-transaction> a nil b))
     (with-transaction> nil a b)))
  ([connectable opts tx-eff-fn]
   (->TransactionEffect :with-transaction nil {:connectable connectable :opts opts :tx-eff-fn tx-eff-fn} connectable opts tx-eff-fn)))

;; ---------------------------------------------------------------------------
;; Statement & Query Execution
;; ---------------------------------------------------------------------------

(defn execute!>
  "Executes a SQL statement returning a vector of row maps or update counts.
   Supports explicit connectable or context-resolved `::datasource`."
  ([sql-params]
   (execute!> nil sql-params nil))
  ([a b]
   (let [[conn sql-params opts] (parse-exec-args-2 a b)]
     (execute!> conn sql-params opts)))
  ([connectable sql-params opts]
   (fx/map-ctx>
     (fn [val ctx]
       (catch-jdbc sql-params
                   (let [target (resolve-connectable connectable val ctx)]
                     (if (nil? target)
                       (missing-connectable-error)
                       (if (nil? opts)
                         (jdbc/execute! target sql-params)
                         (jdbc/execute! target sql-params opts)))))))))

(defn execute-one!>
  "Executes a SQL statement returning the first row map or `nil`.
   Supports explicit connectable or context-resolved `::datasource`."
  ([sql-params]
   (execute-one!> nil sql-params nil))
  ([a b]
   (let [[conn sql-params opts] (parse-exec-args-2 a b)]
     (execute-one!> conn sql-params opts)))
  ([connectable sql-params opts]
   (fx/map-ctx>
     (fn [val ctx]
       (catch-jdbc sql-params
                   (let [target (resolve-connectable connectable val ctx)]
                     (if (nil? target)
                       (missing-connectable-error)
                       (if (nil? opts)
                         (jdbc/execute-one! target sql-params)
                         (jdbc/execute-one! target sql-params opts)))))))))

(defn- parse-plan-args-4 [a b c d]
  (if (fn? c)
    (if (or (vector? a) (string? a) (instance? PreparedStatement a))
      [nil a b c d]
      [a b nil c d])
    [nil a b c d]))

(defn plan!>
  "Executes a streaming lazy reduction over SQL results via `next.jdbc/plan`.
   Evaluates `(reduce reduce-fn initial-acc (next.jdbc/plan ...))` inside the effect."
  ([sql-params reduce-fn initial-acc]
   (plan!> nil sql-params nil reduce-fn initial-acc))
  ([a b c d]
   (let [[conn sql-params opts reduce-fn initial-acc] (parse-plan-args-4 a b c d)]
     (plan!> conn sql-params opts reduce-fn initial-acc)))
  ([connectable sql-params opts reduce-fn initial-acc]
   (fx/map-ctx>
     (fn [val ctx]
       (catch-jdbc sql-params
                   (let [target (resolve-connectable connectable val ctx)]
                     (if (nil? target)
                       (missing-connectable-error)
                       (let [plan (if (nil? opts)
                                    (jdbc/plan target sql-params)
                                    (jdbc/plan target sql-params opts))]
                         (reduce reduce-fn initial-acc plan)))))))))

(defn prepare-statement>
  "Creates an effect preparing a `java.sql.PreparedStatement` from `connectable` (or ::datasource in context)."
  ([sql-params]
   (prepare-statement> nil sql-params nil))
  ([a b]
   (let [[conn sql-params opts] (parse-exec-args-2 a b)]
     (prepare-statement> conn sql-params opts)))
  ([connectable sql-params opts]
   (fx/map-ctx>
     (fn [val ctx]
       (catch-jdbc sql-params
                   (let [target (resolve-connectable connectable val ctx)]
                     (if (nil? target)
                       (missing-connectable-error)
                       (if (nil? opts)
                         (jdbc/prepare target sql-params)
                         (jdbc/prepare target sql-params opts)))))))))

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
