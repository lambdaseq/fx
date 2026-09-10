(ns todo.worker
  "Background scheduled maintenance and asynchronous batch workers using `fx.async` and `fx.layer`."
  (:require [fx.async :as fxa]
            [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [fx.schedule :as sched]
            [todo.db :as db])
  (:import (java.time Instant)))

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
;; Parallel Batch Import Worker using map-par>
;; ---------------------------------------------------------------------------

(defn parallel-import-todos>
  "Imports a collection of raw todo items in parallel using bounded concurrency."
  ([todos]
   (parallel-import-todos> todos 4))
  ([todos concurrency]
   (fxa/map-par>
     (fn [item]
       (let [now (str (Instant/now))
             record {:title       (:title item)
                     :description (:description item)
                     :completed   (boolean (:completed item))
                     :created-at  now
                     :updated-at  now}]
         (db/insert-todo!> record)))
     todos
     {:concurrency concurrency})))

;; ---------------------------------------------------------------------------
;; Event Notification Hub Constructor
;; ---------------------------------------------------------------------------

(defn notification-hub>
  "Creates an async broadcast Hub for system notifications."
  ([]
   (notification-hub> 32))
  ([capacity]
   (fxa/hub-bounded> capacity)))

;; ---------------------------------------------------------------------------
;; Worker Loop Lifecycle (Fiber-backed)
;; ---------------------------------------------------------------------------

(defn start-worker-loop!
  "Starts a background thread/fiber executing `maintenance-task>` every `interval-ms` milliseconds."
  ([ctx]
   (start-worker-loop! ctx 60000 30))
  ([ctx interval-ms days-old]
   (let [running (atom true)
         fiber   (fxa/run-fiber!
                   (fx/try>
                     (fn []
                       (while @running
                         (try
                           (fx/run-sync! (maintenance-task> days-old) ctx)
                           (catch Throwable t
                             (println "Worker task error:" (.getMessage t))))
                         (Thread/sleep (long interval-ms))))
                     :worker/interrupted)
                   ctx)]
     {:running running :fiber fiber})))

(defn stop-worker-loop!
  "Stops the background worker fiber gracefully."
  [{:keys [running fiber]}]
  (when running
    (reset! running false))
  (when fiber
    (fxa/interrupt-fiber! fiber :worker-stopped)))

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
