(ns todo.worker
  "Background scheduled maintenance worker managed via `fx.layer`."
  (:require [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [fx.schedule :as sched]
            [todo.db :as db]))

;; ---------------------------------------------------------------------------
;; Maintenance Task
;; ---------------------------------------------------------------------------

(defn maintenance-task>
  "Effect executing one pass of background maintenance (todo cleanup and logging)."
  ([]
   (maintenance-task> 30))
  ([days-old]
   (->> (-> (log/log-info> "Running scheduled todo maintenance...")
            (fx/chain> (db/cleanup-old-completed-todos!> days-old))
            (log/log-info> "Scheduled todo maintenance completed."))
        (metrics/track-success-count> (metrics/metric-counter "todo.maintenance.runs.total")))))

;; ---------------------------------------------------------------------------
;; Worker Loop Lifecycle
;; ---------------------------------------------------------------------------

(defn start-worker-loop!
  "Starts a background thread that executes `maintenance-task>` every `interval-ms` milliseconds."
  ([ctx]
   (start-worker-loop! ctx 60000 30))
  ([ctx interval-ms days-old]
   (let [running (atom true)
         thread (Thread.
                  (fn []
                    (try
                      (while @running
                        (try
                          (fx/run-sync! (maintenance-task> days-old) ctx)
                          (catch Throwable t
                            (println "Worker task error:" (.getMessage t))))
                        (Thread/sleep (long interval-ms)))
                      (catch InterruptedException _
                        nil)))
                  "todo-maintenance-worker")]
     (.setDaemon thread true)
     (.start thread)
     {:running running :thread thread})))

(defn stop-worker-loop!
  "Stops the background worker thread gracefully."
  [{:keys [running thread]}]
  (when running
    (reset! running false))
  (when (and thread (instance? Thread thread) (.isAlive ^Thread thread))
    (.interrupt ^Thread thread)
    (try (.join ^Thread thread 2000) (catch Throwable _ nil))))

;; ---------------------------------------------------------------------------
;; Worker Layer Definition
;; ---------------------------------------------------------------------------

(defn worker-layer>
  "Defines a managed background maintenance worker layer.
   Options:
     `:interval-ms` - schedule recurrence interval in ms (default: 60000)
     `:days-old`    - cutoff threshold in days for completed todos (default: 30)"
  ([]
   (worker-layer> {}))
  ([opts]
   (let [interval-ms (or (:interval-ms opts) 60000)
         days-old    (or (:days-old opts) 30)]
     (fx-layer/make> :todo/worker
       (-> (fx/context>)
           (fx/map> (fn [ctx]
                      (start-worker-loop! ctx interval-ms days-old)))
           (log/log-info> "Maintenance worker layer started" {:interval-ms interval-ms :days-old days-old}))
       (fn [worker-handle]
         (-> (fx/try> (fn [] (stop-worker-loop! worker-handle))
                      :worker/stop-failed)
             (log/log-info> "Maintenance worker layer stopped")))))))
