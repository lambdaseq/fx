(ns fx.observability.metrics
  "Zero-dependency, high-concurrency in-memory metrics for fx effect pipelines."
  (:require [fx.core :as fx]
            [fx.layer :as fx-layer]))

;; ---------------------------------------------------------------------------
;; Metric Descriptors
;; ---------------------------------------------------------------------------

(defrecord MetricDescriptor [type name tags])

(defn metric-counter
  "Creates a counter metric descriptor."
  ([name]
   (metric-counter name {}))
  ([name tags]
   (->MetricDescriptor :counter (clojure.core/name name) (or tags {}))))

(defn metric-gauge
  "Creates a gauge metric descriptor."
  ([name]
   (metric-gauge name {}))
  ([name tags]
   (->MetricDescriptor :gauge (clojure.core/name name) (or tags {}))))

(defn metric-timer
  "Creates a latency timer metric descriptor."
  ([name]
   (metric-timer name {}))
  ([name tags]
   (->MetricDescriptor :timer (clojure.core/name name) (or tags {}))))

(defn metric-descriptor?
  "Returns true if `x` is a MetricDescriptor."
  [x]
  (instance? MetricDescriptor x))

;; ---------------------------------------------------------------------------
;; Pure Metric Calculations
;; ---------------------------------------------------------------------------

(defn- metric-key [desc]
  [(:name desc) (or (:tags desc) {})])

(defn update-timer-stats
  "Pure function that incorporates a new duration observation (in milliseconds)
   into existing timer stats map."
  [stats duration-ms]
  (let [dur (double duration-ms)
        c   (inc (get stats :count 0))
        s   (+ (get stats :sum-ms 0.0) dur)
        mn  (if-let [cur-min (:min-ms stats)]
              (min (double cur-min) dur)
              dur)
        mx  (if-let [cur-max (:max-ms stats)]
              (max (double cur-max) dur)
              dur)]
    {:count  c
     :sum-ms s
     :min-ms mn
     :max-ms mx
     :avg-ms (/ s c)}))

(defn- format-metric-entries
  "Pure function transforming an internal registry entry map into snapshot format."
  [entries value-fn]
  (into {}
        (map (fn [[[name tags] data]]
               [name (merge {:tags (or tags {})} (value-fn data))]))
        entries))

(defn format-metrics-snapshot
  "Pure function transforming raw registry state map into a formatted snapshot map."
  [{:keys [counters gauges timers]}]
  {:counters (format-metric-entries counters (fn [v] {:value v}))
   :gauges   (format-metric-entries gauges (fn [v] {:value v}))
   :timers   (format-metric-entries timers (fn [stats]
                                             (let [c (get stats :count 0)
                                                   s (get stats :sum-ms 0.0)]
                                               {:count  c
                                                :sum-ms s
                                                :min-ms (or (:min-ms stats) 0.0)
                                                :max-ms (or (:max-ms stats) 0.0)
                                                :avg-ms (if (pos? c) (/ s c) 0.0)})))})

;; ---------------------------------------------------------------------------
;; Registry Management
;; ---------------------------------------------------------------------------

(def ^:private initial-registry-state
  {:counters {}
   :gauges   {}
   :timers   {}})

(defn make-metrics-registry
  "Creates a new isolated, concurrent metrics registry."
  []
  (atom initial-registry-state))

(defn counter-inc!
  "Increments counter descriptor in `registry` by `n` (default 1)."
  ([registry desc]
   (counter-inc! registry desc 1))
  ([registry desc n]
   (when (and registry desc)
     (let [k (metric-key desc)
           amount (long (or n 1))]
       (swap! registry update-in [:counters k] (fnil #(+ % amount) 0))))))

(defn gauge-set!
  "Sets gauge descriptor value in `registry`."
  [registry desc val]
  (when (and registry desc)
    (let [k (metric-key desc)]
      (swap! registry assoc-in [:gauges k] val))))

(defn timer-record!
  "Records a latency observation in milliseconds into timer descriptor in `registry`."
  [registry desc duration-ms]
  (when (and registry desc)
    (let [k (metric-key desc)]
      (swap! registry update-in [:timers k] update-timer-stats duration-ms))))

(defn metrics-snapshot!
  "Returns a pure Clojure map snapshot of all metrics registered in `registry`."
  [registry]
  (when registry
    (format-metrics-snapshot @registry)))

(defn reset-metrics!
  "Resets all metric accumulators in `registry` to zero."
  [registry]
  (when registry
    (reset! registry initial-registry-state)))

;; ---------------------------------------------------------------------------
;; Continuation Frames
;; ---------------------------------------------------------------------------

(defn- current-nano-time []
  #?(:clj  (System/nanoTime)
     :cljs (* (js/Date.now) 1000000)))

(defrecord TrackDurationFrame [timer-desc start-nano context]
  fx/IContinuation
  (-resume [_ val current-context stack]
    (let [end-nano (current-nano-time)
          duration-ms (/ (double (- end-nano start-nano)) 1000000.0)
          ctx (or current-context context)
          reg (:fx.observability/metrics-registry ctx)]
      (when reg
        (timer-record! reg timer-desc duration-ms))
      [nil val current-context stack]))

  fx/IUnwindable
  (-unwind [_ _exception _rest-stack]
    (let [end-nano (current-nano-time)
          duration-ms (/ (double (- end-nano start-nano)) 1000000.0)
          reg (:fx.observability/metrics-registry context)]
      (when reg
        (timer-record! reg timer-desc duration-ms))
      nil)))

(defrecord TrackSuccessCountFrame [counter-desc context]
  fx/IContinuation
  (-resume [_ val current-context stack]
    (when-not (fx/failure? val)
      (let [ctx (or current-context context)
            reg (:fx.observability/metrics-registry ctx)]
        (when reg
          (counter-inc! reg counter-desc 1))))
    [nil val current-context stack]))

(defrecord TrackFailureCountFrame [counter-desc context]
  fx/IContinuation
  (-resume [_ val current-context stack]
    (when (fx/failure? val)
      (let [ctx (or current-context context)
            reg (:fx.observability/metrics-registry ctx)]
        (when reg
          (counter-inc! reg counter-desc 1))))
    [nil val current-context stack])

  fx/IUnwindable
  (-unwind [_ _exception _rest-stack]
    (let [reg (:fx.observability/metrics-registry context)]
      (when reg
        (counter-inc! reg counter-desc 1)))
    nil))

;; ---------------------------------------------------------------------------
;; Effect Records
;; ---------------------------------------------------------------------------

(defrecord TrackDurationEffect [tag prev-effect data timer-desc body-eff]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [start-nano (current-nano-time)
            eff (if (some? val)
                  (fx/chain> (fx/succeed> val) body-eff)
                  body-eff)]
        [eff nil context (conj stack (->TrackDurationFrame timer-desc start-nano context))]))))

(defrecord TrackSuccessCountEffect [tag prev-effect data counter-desc body-eff]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (if (some? val)
                  (fx/chain> (fx/succeed> val) body-eff)
                  body-eff)]
        [eff nil context (conj stack (->TrackSuccessCountFrame counter-desc context))]))))

(defrecord TrackFailureCountEffect [tag prev-effect data counter-desc body-eff]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (if (some? val)
                  (fx/chain> (fx/succeed> val) body-eff)
                  body-eff)]
        [eff nil context (conj stack (->TrackFailureCountFrame counter-desc context))]))))

;; ---------------------------------------------------------------------------
;; Combinators & Effect Constructors
;; ---------------------------------------------------------------------------

(defn track-duration>
  "Wraps `effect` measuring elapsed execution time and recording it into `timer-descriptor`.
   
   Example:
     (fx.observability.metrics/track-duration> (fx.observability.metrics/metric-timer \"db.query.time\" {:table \"users\"})
       (query-db> sql))"
  ([timer-desc eff]
   (track-duration> nil timer-desc eff))
  ([prev-effect timer-desc eff]
   (->TrackDurationEffect :track-duration prev-effect {:timer timer-desc :body eff} timer-desc eff)))

(defn track-success-count>
  "Wraps `effect`, incrementing `counter-descriptor` upon successful execution.
   Leaves counter unmodified if the effect fails.
   
   Example:
     (fx.observability.metrics/track-success-count> (fx.observability.metrics/metric-counter \"orders.success\")
       (place-order> order))"
  ([counter-desc eff]
   (track-success-count> nil counter-desc eff))
  ([prev-effect counter-desc eff]
   (->TrackSuccessCountEffect :track-success-count prev-effect {:counter counter-desc :body eff} counter-desc eff)))

(defn track-failure-count>
  "Wraps `effect`, incrementing `counter-descriptor` upon failure or defect.
   Leaves counter unmodified if the effect succeeds.
   
   Example:
     (fx.observability.metrics/track-failure-count> (fx.observability.metrics/metric-counter \"orders.failure\")
       (place-order> order))"
  ([counter-desc eff]
   (track-failure-count> nil counter-desc eff))
  ([prev-effect counter-desc eff]
   (->TrackFailureCountEffect :track-failure-count prev-effect {:counter counter-desc :body eff} counter-desc eff)))

(defn with-metrics-registry>
  "Scopes a metrics registry to `effect`.
   
   Example:
     (fx.observability.metrics/with-metrics-registry> reg (app-pipeline>))"
  ([registry-or-eff eff-or-registry]
   (if (fx/effect? registry-or-eff)
     (fx/provide> {:fx.observability/metrics-registry eff-or-registry} registry-or-eff)
     (fx/provide> {:fx.observability/metrics-registry registry-or-eff} eff-or-registry)))
  ([prev-effect registry target-eff]
   (fx/chain> prev-effect (fx/provide> {:fx.observability/metrics-registry registry} target-eff))))

(defn metrics-layer>
  "Creates a managed Layer providing `:fx.observability/metrics-registry`.
   When called with no arguments, acquires a fresh isolated concurrent metrics registry.
   When called with `registry`, provides that registry.
   On release, resets the registry via `reset-metrics!`."
  ([]
   (metrics-layer> (make-metrics-registry)))
  ([registry]
   (fx-layer/make> :fx.observability/metrics-registry
     (fx/succeed> (or registry (make-metrics-registry)))
     (fn [reg]
       (fx/try> (fn [] (reset-metrics! reg)))))))

(defn counter-inc>
  "Creates an effect that increments `counter-descriptor` by `n` (default 1).
   
   Example:
     (-> (fx/succeed> :ok)
         (fx.observability.metrics/counter-inc> (fx.observability.metrics/metric-counter \"events.total\")))"
  ([counter-desc]
   (counter-inc> nil counter-desc 1))
  ([counter-desc-or-prev n-or-desc]
   (if (fx/effect? counter-desc-or-prev)
     (counter-inc> counter-desc-or-prev n-or-desc 1)
     (counter-inc> nil counter-desc-or-prev n-or-desc)))
  ([prev-effect counter-desc n]
   (let [eff (fx/map-ctx> (fn [val ctx]
                            (when-let [reg (:fx.observability/metrics-registry ctx)]
                              (counter-inc! reg counter-desc n))
                            val))]
     (if prev-effect (fx/chain> prev-effect eff) eff))))

(defn gauge-set>
  "Creates an effect that sets `gauge-descriptor` to `val-or-fn`.
   
   Example:
     (fx.observability.metrics/gauge-set> (fx.observability.metrics/metric-gauge \"queue.size\") 42)"
  ([gauge-desc val]
   (gauge-set> nil gauge-desc val))
  ([prev-effect gauge-desc val]
   (let [eff (fx/map-ctx> (fn [v ctx]
                            (when-let [reg (:fx.observability/metrics-registry ctx)]
                              (let [gauge-val (if (fn? val) (val v) val)]
                                (gauge-set! reg gauge-desc gauge-val)))
                            v))]
     (if prev-effect (fx/chain> prev-effect eff) eff))))
