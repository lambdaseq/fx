(ns todo.main
  (:require [fx.core :as fx]
            [fx.http-client :as http]
            [fx.jdbc :as-alias fx.jdbc]
            [fx.layer :as fx-layer]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [ring.adapter.jetty :as jetty]
            [todo.db :as db]
            [todo.routes :as routes]
            [todo.worker :as worker])
  (:gen-class)
  (:import (java.io Closeable)
           (org.eclipse.jetty.server Server)))

(defonce ^:private active-system (atom nil))

;; ---------------------------------------------------------------------------
;; Layer Definitions
;; ---------------------------------------------------------------------------

(defn metrics-layer>
  "Defines a managed metrics registry layer using `fx.observability.metrics/metrics-layer>`.
   Acquires a fresh isolated concurrent metrics registry; resets on release."
  ([]
   (metrics/metrics-layer>))
  ([registry]
   (metrics/metrics-layer> registry)))

(defn datasource-layer>
  "Defines a managed datasource layer for SQLite.
   Acquires datasource and initializes schema; closes datasource on release."
  ([]
   (datasource-layer> db/default-db-spec))
  ([db-spec]
   (fx-layer/make> ::fx.jdbc/datasource
     (-> (fx/try> (fn [] (db/create-datasource db-spec)) :db/datasource-creation-failed)
         (fx/tap> db/init-db!))
     (fn [ds]
       (fx/try> (fn []
                  (when (instance? Closeable ds)
                    (.close ^Closeable ds)))
                :db/datasource-close-failed)))))

(defn http-client-layer>
  "Defines a managed HTTP client layer for fx-http-client."
  ([]
   (http-client-layer> {:connect-timeout 5000 :version :http-2}))
  ([opts]
   (fx-layer/make> :fx.http-client/client
     (http/build-client> opts)
     (fn [_client] (fx/succeed> nil)))))

(defn http-server-layer>
  "Defines a managed HTTP server layer running Ring with embedded Jetty.
   Acquires Jetty server on `:port`; stops Jetty server on release."
  ([]
   (http-server-layer> 3000))
  ([port]
   (fx-layer/make> :todo/server
     (-> (fx/context>)
         (fx/mapcat> (fn [ctx]
                       (fx/try> (fn []
                                  (let [app (routes/create-app ctx)
                                        server (jetty/run-jetty app {:port  port
                                                                     :join? false})]
                                    (println (str "Todo application server started successfully on http://localhost:" port))
                                    server))
                                :server/start-failed)))
         (log/log-info> "HTTP server layer initialized" {:port port}))
     (fn [server]
       (-> (fx/try> (fn []
                      (println "Stopping Jetty server...")
                      (.close server))
                    :server/stop-failed)
           (log/log-info> "HTTP server layer stopped"))))))

(defn app-layer>
  "Composes datasource, metrics registry, http-client, worker, and HTTP server layers into a complete application system layer."
  ([]
   (app-layer> {}))
  ([opts]
   (let [port (or (:port opts)
                  (when-let [env-port (System/getenv "PORT")]
                    (try (Integer/parseInt env-port) (catch Throwable _ nil)))
                  3000)
         db-spec (or (:db-spec opts) db/default-db-spec)
         client-opts (or (:http-client opts) {:connect-timeout 5000 :version :http-2})
         worker-opts (select-keys opts [:interval-ms :days-old])]
     (fx-layer/compose>
       (metrics-layer>)
       (datasource-layer> db-spec)
       (http-client-layer> client-opts)
       (worker/worker-layer> worker-opts)
       (http-server-layer> port)))))

;; ---------------------------------------------------------------------------
;; Server Lifecycle Controls & Entrypoint
;; ---------------------------------------------------------------------------

(defn start-server!
  "Starts the todo application system using layers.
   Options:
     :port    - TCP port to bind (default: 3000 or env PORT)
     :join?   - Whether the calling thread should block (default: false)
     :db-spec - Database spec map or JDBC URL string (default: in-memory shared SQLite)"
  ([]
   (start-server! {}))
  ([opts]
   (when-let [existing @active-system]
     (println "Stopping existing system instance...")
     (fx-layer/stop-layer! existing)
     (reset! active-system nil))

   (let [system (fx-layer/start-layer! (app-layer> opts))]
     (reset! active-system system)
     (when (:join? opts)
       (when-let [server (get system :todo/server)]
         (.join ^Server server)))
     (get system :todo/server))))

(defn stop-server!
  "Stops the active system and closes all layer resources."
  []
  (when-let [system @active-system]
    (println "Stopping application system...")
    (fx-layer/stop-layer! system)
    (reset! active-system nil))
  (println "Server stopped."))

(defn -main
  "Main CLI entrypoint."
  [& _args]
  (let [port (when-let [env-port (System/getenv "PORT")]
               (try (Integer/parseInt env-port) (catch Throwable _ nil)))]
    (fx-layer/launch-sync! (app-layer> (cond-> {} port (assoc :port port)))
                           {:join? true})))
