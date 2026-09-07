(ns todo.main
  (:require [fx.core :as fx]
            [ring.adapter.jetty :as jetty]
            [todo.db :as db]
            [todo.routes :as routes])
  (:gen-class))

(defonce ^:private server-instance (atom nil))
(defonce ^:private datasource-instance (atom nil))

(defn start-server!
  "Starts the embedded Jetty HTTP server and initializes the SQLite database.
   Options:
     :port    - TCP port to bind (default: 3000 or env PORT)
     :join?   - Whether the calling thread should block (default: false)
     :db-spec - Database spec map or JDBC URL string (default: in-memory shared SQLite)"
  ([]
   (start-server! {}))
  ([opts]
   (when-let [existing @server-instance]
     (println "Stopping existing server instance...")
     (.stop existing)
     (reset! server-instance nil))

   (let [port (or (:port opts)
                  (when-let [env-port (System/getenv "PORT")]
                    (try (Integer/parseInt env-port) (catch Throwable _ nil)))
                  3000)
         join? (boolean (:join? opts false))
         db-spec (or (:db-spec opts) db/default-db-spec)
         ds (db/create-datasource db-spec)]

     (println "Initializing database schema...")
     (let [init-result (fx/run-sync! (db/init-db!> ds))]
       (when (fx/failure? init-result)
         (throw (ex-info "Database initialization failed" {:result init-result}))))

     (reset! datasource-instance ds)

     (let [app (routes/create-app ds)
           server (jetty/run-jetty app {:port  port
                                        :join? join?})]
       (reset! server-instance server)
       (println (str "Todo application server started successfully on http://localhost:" port))
       server))))

(defn stop-server!
  "Stops the active embedded Jetty HTTP server and closes the datasource."
  []
  (when-let [server @server-instance]
    (println "Stopping Jetty server...")
    (.stop server)
    (reset! server-instance nil))
  (when-let [ds @datasource-instance]
    (when (instance? java.io.Closeable ds)
      (try (.close ^java.io.Closeable ds) (catch Throwable _ nil)))
    (reset! datasource-instance nil))
  (println "Server stopped."))

(defn -main
  "Main CLI entrypoint."
  [& _args]
  (start-server! {:join? true}))
