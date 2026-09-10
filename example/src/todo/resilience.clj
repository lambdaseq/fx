(ns todo.resilience
  "Resilience policies, retry schedules, rate limiters, and circuit breakers for the Todo application."
  (:require [fx.schedule :as sched]))

;; ---------------------------------------------------------------------------
;; Retry Policies
;; ---------------------------------------------------------------------------

(def db-retry-policy
  "Retry policy for transient database errors.
   Uses exponential backoff (50ms base, factor 2.0, 1000ms max) with 10% jitter,
   retries at most 3 times, filtering on database failure tags."
  (-> (sched/exponential-backoff> {:initial-ms 50 :factor 2.0 :max-ms 1000})
      (sched/jitter> 0.1)
      (sched/intersect> (sched/recur-n> 3))
      (sched/while-tag> #{:fx.jdbc/error :jdbc/error :db/busy :sqlite/busy})))

;; ---------------------------------------------------------------------------
;; Rate Limiter Constructors
;; ---------------------------------------------------------------------------

(defn create-todo-rate-limiter
  "Creates an in-memory token-bucket rate limiter for write operations.
   Defaults to 10 requests per 1000ms window."
  ([]
   (create-todo-rate-limiter {}))
  ([opts]
   (sched/make-rate-limiter (merge {:limit 10 :interval-ms 1000} opts))))
