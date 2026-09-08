(ns todo.db
  (:require [fx.core :as fx]
            [fx.jdbc :as fx-jdbc]
            [honey.sql :as sql]
            [next.jdbc :as jdbc])
  (:import (javax.sql DataSource)
           (org.sqlite SQLiteDataSource)))

;; ---------------------------------------------------------------------------
;; Datasource Construction
;; ---------------------------------------------------------------------------

(def default-db-spec
  {:jdbc-url "jdbc:sqlite:file:tododb?mode=memory&cache=shared"})

(defn create-datasource
  "Creates a `javax.sql.DataSource` for SQLite in-memory or file-backed database.
   Maintains a keep-alive connection for shared in-memory SQLite instances so tables
   persist across connection acquisitions."
  ([]
   (create-datasource default-db-spec))
  ([spec]
   (let [url (if (string? spec)
               spec
               (or (:jdbc-url spec) (:jdbcUrl spec) (:dbname spec)))
         ds (doto (SQLiteDataSource.)
              (.setUrl url))
         keep-alive (when (and (string? url) (.contains url "mode=memory"))
                      (.getConnection ds))]
     (reify
       DataSource
       (getConnection [_] (.getConnection ds))
       (getConnection [_ user pass] (.getConnection ds user pass))
       (unwrap [_ iface] (.unwrap ds iface))
       (isWrapperFor [_ iface] (.isWrapperFor ds iface))
       (getLogWriter [_] (.getLogWriter ds))
       (setLogWriter [_ out] (.setLogWriter ds out))
       (setLoginTimeout [_ seconds] (.setLoginTimeout ds seconds))
       (getLoginTimeout [_] (.getLoginTimeout ds))
       (getParentLogger [_] (.getParentLogger ds))
       java.io.Closeable
       (close [_]
         (when keep-alive
           (try (.close keep-alive) (catch Throwable _ nil))))))))

(defn get-datasource>
  "Effect yielding a `javax.sql.DataSource`."
  ([]
   (get-datasource> default-db-spec))
  ([spec]
   (fx/succeed> (create-datasource spec))))

;; ---------------------------------------------------------------------------
;; DDL & Schema Setup
;; ---------------------------------------------------------------------------

(def ddl-create-todos-table
  "CREATE TABLE IF NOT EXISTS todos (
     id INTEGER PRIMARY KEY AUTOINCREMENT,
     title TEXT NOT NULL,
     description TEXT,
     completed INTEGER NOT NULL DEFAULT 0,
     created_at TEXT NOT NULL,
     updated_at TEXT NOT NULL
   )")

(defn init-db!
  "Executes the DDL statement creating the `todos` table."
  ([]
   (init-db! nil))
  ([connectable]
   (jdbc/execute! connectable [ddl-create-todos-table])))

(defn init-db!>
  "Effect executing the DDL statement creating the `todos` table."
  ([]
   (init-db!> nil))
  ([connectable]
   (fx-jdbc/execute!> connectable [ddl-create-todos-table])))

;; ---------------------------------------------------------------------------
;; HoneySQL Query Definitions
;; ---------------------------------------------------------------------------

(defn sql-insert-todo
  [{:keys [title description completed created-at updated-at]}]
  {:insert-into :todos
   :values [{:title       title
             :description description
             :completed   (if completed 1 0)
             :created_at  created-at
             :updated_at  updated-at}]})

(defn sql-select-all
  ([]
   (sql-select-all nil))
  ([completed-filter]
   (let [base {:select   [:*]
               :from     [:todos]
               :order-by [[:id :asc]]}]
     (if (some? completed-filter)
       (assoc base :where [:= :completed (if completed-filter 1 0)])
       base))))

(defn sql-select-by-id
  [id]
  {:select [:*]
   :from   [:todos]
   :where  [:= :id id]})

(defn sql-update-todo
  [id updates]
  (let [set-map (cond-> {}
                  (contains? updates :title)       (assoc :title (:title updates))
                  (contains? updates :description) (assoc :description (:description updates))
                  (contains? updates :completed)   (assoc :completed (if (:completed updates) 1 0))
                  (contains? updates :updated-at)  (assoc :updated_at (:updated-at updates)))]
    {:update :todos
     :set    set-map
     :where  [:= :id id]}))

(defn sql-delete-todo
  [id]
  {:delete-from :todos
   :where       [:= :id id]})

(defn sql-toggle-todo
  [id updated-at]
  {:update :todos
   :set    {:completed  [:case [:= :completed 1] 0 :else 1]
            :updated_at updated-at}
   :where  [:= :id id]})

;; ---------------------------------------------------------------------------
;; Row Mappings & Query Runners
;; ---------------------------------------------------------------------------

(defn row->todo
  "Normalizes SQLite row map by converting integer `completed` to boolean."
  [row]
  (when row
    (update row :completed (fn [c] (if (number? c) (== c 1) (boolean c))))))

(defn query-todos>
  "Queries todos with an optional `:completed` boolean filter."
  ([]
   (query-todos> nil))
  ([completed-filter]
   (-> (fx-jdbc/execute!> (sql/format (sql-select-all completed-filter))
                          {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
       (fx/map> (fn [rows] (mapv row->todo rows))))))

(defn query-todo-by-id>
  "Queries a single todo by primary key `id`."
  [id]
  (-> (fx-jdbc/execute-one!> (sql/format (sql-select-by-id id))
                             {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
      (fx/map> row->todo)))

(defn insert-todo!>
  "Inserts a new todo record and returns the created todo entity map."
  [todo-map]
  (-> (fx-jdbc/execute-one!> (sql/format (sql-insert-todo todo-map))
                             {:return-keys true
                              :builder-fn  fx-jdbc/as-unqualified-kebab-maps})
      (fx/mapcat> (fn [res]
                    (let [id (or (:id res)
                                 (:last-insert-rowid res)
                                 (get res (keyword "last-insert-rowid()"))
                                 (first (vals res)))]
                      (query-todo-by-id> id))))))

(defn update-todo!>
  "Updates fields of an existing todo record and returns the updated entity."
  [id updates]
  (-> (fx-jdbc/execute-one!> (sql/format (sql-update-todo id updates))
                             {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
      (fx/mapcat> (fn [_]
                    (query-todo-by-id> id)))))

(defn toggle-todo!>
  "Toggles the `:completed` boolean of a todo and updates `:updated-at`."
  [id updated-at]
  (-> (fx-jdbc/execute-one!> (sql/format (sql-toggle-todo id updated-at))
                             {:builder-fn fx-jdbc/as-unqualified-kebab-maps})
      (fx/mapcat> (fn [_]
                    (query-todo-by-id> id)))))

(defn delete-todo!>
  "Deletes a todo by primary key `id`."
  [id]
  (fx-jdbc/execute-one!> (sql/format (sql-delete-todo id))
                         {:builder-fn fx-jdbc/as-unqualified-kebab-maps}))
