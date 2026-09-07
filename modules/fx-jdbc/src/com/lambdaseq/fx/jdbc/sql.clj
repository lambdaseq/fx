(ns com.lambdaseq.fx.jdbc.sql
  "Convenience CRUD combinators wrapping `next.jdbc.sql` into effectful operations."
  (:require [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.jdbc :as fx-jdbc]
            [next.jdbc.sql :as sql]))

;; ---------------------------------------------------------------------------
;; Connectable Helpers
;; ---------------------------------------------------------------------------

(defn- connectable? [x]
  (or (instance? javax.sql.DataSource x)
      (instance? java.sql.Connection x)
      (instance? java.sql.PreparedStatement x)
      (and (map? x)
           (not (fx/effect? x))
           (boolean (or (:dbtype x) (:jdbcUrl x) (:dbname x) (:connection-uri x) (:datasource x))))))

(defn- resolve-target [explicit-conn upstream-val ctx]
  (or explicit-conn
      (when (and (some? upstream-val) (not (fx/failure? upstream-val)) (connectable? upstream-val))
        upstream-val)
      (:com.lambdaseq.fx.jdbc/datasource ctx)))

;; ---------------------------------------------------------------------------
;; SQL CRUD Combinators
;; ---------------------------------------------------------------------------

(defn insert!>
  "Executes a SQL INSERT for a single row map.
   Supports explicit connectable or context-resolved `::fx-jdbc/datasource`."
  ([table row-map]
   (insert!> nil table row-map nil))
  ([a b c]
   (if (connectable? a)
     (insert!> a b c nil)
     (insert!> nil a b c)))
  ([connectable table row-map opts]
   (fx/map-ctx>
     (fn [val ctx]
       (fx-jdbc/catch-jdbc {:table table :row row-map}
         (let [target (resolve-target connectable val ctx)]
           (if (nil? target)
             (fx-jdbc/missing-connectable-error)
             (if (nil? opts)
               (sql/insert! target table row-map)
               (sql/insert! target table row-map opts)))))))))

(defn insert-multi!>
  "Executes a batch SQL INSERT for multiple rows.
   Supports explicit connectable or context-resolved `::fx-jdbc/datasource`."
  ([table rows]
   (insert-multi!> nil table nil rows nil))
  ([a b c]
   (if (connectable? a)
     (insert-multi!> a b nil c nil)
     (if (map? c)
       (insert-multi!> nil a nil b c)
       (insert-multi!> nil a b c nil))))
  ([a b c d]
   (if (connectable? a)
     (if (map? d)
       (insert-multi!> a b nil c d)
       (insert-multi!> a b c d nil))
     (insert-multi!> nil a b c d)))
  ([connectable table cols rows opts]
   (fx/map-ctx>
     (fn [val ctx]
       (fx-jdbc/catch-jdbc {:table table :rows (or rows cols)}
         (let [target (resolve-target connectable val ctx)]
           (if (nil? target)
             (fx-jdbc/missing-connectable-error)
             (cond
               (nil? cols)
               (if (nil? opts)
                 (sql/insert-multi! target table rows)
                 (sql/insert-multi! target table rows opts))

               (nil? opts)
               (sql/insert-multi! target table cols rows)

               :else
               (sql/insert-multi! target table cols rows opts)))))))))

(defn query!>
  "Executes a SQL query returning a vector of maps.
   Supports explicit connectable or context-resolved `::fx-jdbc/datasource`."
  ([sql-params]
   (query!> nil sql-params nil))
  ([a b]
   (if (connectable? a)
     (query!> a b nil)
     (query!> nil a b)))
  ([connectable sql-params opts]
   (fx/map-ctx>
     (fn [val ctx]
       (fx-jdbc/catch-jdbc sql-params
         (let [target (resolve-target connectable val ctx)]
           (if (nil? target)
             (fx-jdbc/missing-connectable-error)
             (if (nil? opts)
               (sql/query target sql-params)
               (sql/query target sql-params opts)))))))))

(defn find-by-keys!>
  "Queries rows matching the specified column key-value map.
   Supports explicit connectable or context-resolved `::fx-jdbc/datasource`."
  ([table map-of-cols]
   (find-by-keys!> nil table map-of-cols nil))
  ([a b c]
   (if (connectable? a)
     (find-by-keys!> a b c nil)
     (find-by-keys!> nil a b c)))
  ([connectable table map-of-cols opts]
   (fx/map-ctx>
     (fn [val ctx]
       (fx-jdbc/catch-jdbc {:table table :keys map-of-cols}
         (let [target (resolve-target connectable val ctx)]
           (if (nil? target)
             (fx-jdbc/missing-connectable-error)
             (if (nil? opts)
               (sql/find-by-keys target table map-of-cols)
               (sql/find-by-keys target table map-of-cols opts)))))))))

(defn get-by-id!>
  "Retrieves a single row by primary key id.
   Supports explicit connectable or context-resolved `::fx-jdbc/datasource`."
  ([table id]
   (get-by-id!> nil table id nil nil))
  ([a b c]
   (if (connectable? a)
     (get-by-id!> a b c nil nil)
     (if (map? c)
       (get-by-id!> nil a b nil c)
       (get-by-id!> nil a b c nil))))
  ([a b c d]
   (if (connectable? a)
     (if (map? d)
       (get-by-id!> a b c nil d)
       (get-by-id!> a b c d nil))
     (get-by-id!> nil a b c d)))
  ([connectable table id pk-col opts]
   (fx/map-ctx>
     (fn [val ctx]
       (fx-jdbc/catch-jdbc {:table table :id id}
         (let [target (resolve-target connectable val ctx)]
           (if (nil? target)
             (fx-jdbc/missing-connectable-error)
             (cond
               (and (nil? pk-col) (nil? opts))
               (sql/get-by-id target table id)

               (nil? pk-col)
               (sql/get-by-id target table id opts)

               (nil? opts)
               (sql/get-by-id target table id pk-col)

               :else
               (sql/get-by-id target table id pk-col opts)))))))))

(defn update!>
  "Executes a SQL UPDATE modifying `map-of-cols` for rows satisfying `where-params`.
   Supports explicit connectable or context-resolved `::fx-jdbc/datasource`."
  ([table map-of-cols where-params]
   (update!> nil table map-of-cols where-params nil))
  ([a b c d]
   (if (connectable? a)
     (update!> a b c d nil)
     (update!> nil a b c d)))
  ([connectable table map-of-cols where-params opts]
   (fx/map-ctx>
     (fn [val ctx]
       (fx-jdbc/catch-jdbc {:table table :set map-of-cols :where where-params}
         (let [target (resolve-target connectable val ctx)]
           (if (nil? target)
             (fx-jdbc/missing-connectable-error)
             (if (nil? opts)
               (sql/update! target table map-of-cols where-params)
               (sql/update! target table map-of-cols where-params opts)))))))))

(defn delete!>
  "Executes a SQL DELETE removing rows satisfying `where-params`.
   Supports explicit connectable or context-resolved `::fx-jdbc/datasource`."
  ([table where-params]
   (delete!> nil table where-params nil))
  ([a b c]
   (if (connectable? a)
     (delete!> a b c nil)
     (delete!> nil a b c)))
  ([connectable table where-params opts]
   (fx/map-ctx>
     (fn [val ctx]
       (fx-jdbc/catch-jdbc {:table table :where where-params}
         (let [target (resolve-target connectable val ctx)]
           (if (nil? target)
             (fx-jdbc/missing-connectable-error)
             (if (nil? opts)
               (sql/delete! target table where-params)
               (sql/delete! target table where-params opts)))))))))
