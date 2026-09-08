(ns fx.observability.telemetry
  "Non-intrusive event taps, interceptors, and telemetry sink routing for fx effect pipelines."
  (:require [fx.core :as fx]
            [fx.layer :as fx-layer])
  #?(:clj (:import (java.time Instant))))

;; ---------------------------------------------------------------------------
;; Time Utilities
;; ---------------------------------------------------------------------------

(defn- current-nano-time []
  #?(:clj  (System/nanoTime)
     :cljs (* (js/Date.now) 1000000)))

(defn- current-timestamp []
  #?(:clj  (Instant/now)
     :cljs (js/Date.)))

;; ---------------------------------------------------------------------------
;; Telemetry Sinks & Dispatch
;; ---------------------------------------------------------------------------

(defn emit-telemetry!
  "Dispatches a `telemetry-event` map to all registered sinks in `context-or-sinks`."
  [context-or-sinks event]
  (let [sinks (cond
                (fn? context-or-sinks) [context-or-sinks]
                (sequential? context-or-sinks) context-or-sinks
                (map? context-or-sinks) (let [s (or (get context-or-sinks :fx.observability/telemetry-sinks)
                                                    (get context-or-sinks :fx/telemetry-sinks))]
                                          (cond
                                            (fn? s) [s]
                                            (sequential? s) s
                                            :else []))
                :else [])]
    (doseq [sink sinks]
      (try
        (sink event)
        (catch #?(:clj Throwable :cljs :default) _ nil)))))

;; ---------------------------------------------------------------------------
;; Continuation Frames
;; ---------------------------------------------------------------------------

(defrecord TapTelemetryFrame [event-type start-nano context meta-data]
  fx/IContinuation
  (-resume [_ val current-context stack]
    (let [end-nano (current-nano-time)
          duration-ms (/ (double (- end-nano start-nano)) 1000000.0)
          status (if (fx/failure? val) :failure :success)
          event {:event       event-type
                 :status      status
                 :duration-ms duration-ms
                 :timestamp   (current-timestamp)
                 :value       (when-not (fx/failure? val) val)
                 :failure     (when (fx/failure? val) (fx/failure->value val))
                 :metadata    (or meta-data {})}]
      (emit-telemetry! current-context event)
      [nil val current-context stack]))

  fx/IUnwindable
  (-unwind [_ exception rest-stack]
    (let [end-nano (current-nano-time)
          duration-ms (/ (double (- end-nano start-nano)) 1000000.0)
          event {:event       event-type
                 :status      :failure
                 :duration-ms duration-ms
                 :timestamp   (current-timestamp)
                 :exception   exception
                 :metadata    (or meta-data {})}]
      (emit-telemetry! context event)
      nil)))

;; ---------------------------------------------------------------------------
;; Effect Records
;; ---------------------------------------------------------------------------

(defrecord TapTelemetryEffect [tag prev-effect data event-type body-eff meta-data]
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
        [eff nil context (conj stack (->TapTelemetryFrame event-type start-nano context meta-data))]))))

;; ---------------------------------------------------------------------------
;; Public Combinators
;; ---------------------------------------------------------------------------

(defn with-telemetry-sink>
  "Registers a telemetry `sink-fn` `(fn [event-map] ...)` scoped to `effect`.
   
   Example:
     (fx.observability.telemetry/with-telemetry-sink> (fn [e] (prn e)) (work>))"
  ([sink-fn-or-eff eff-or-sink-fn]
   (if (fx/effect? sink-fn-or-eff)
     (fx/provide> {:fx.observability/telemetry-sinks [eff-or-sink-fn]} sink-fn-or-eff)
     (fx/provide> {:fx.observability/telemetry-sinks [sink-fn-or-eff]} eff-or-sink-fn)))
  ([prev-effect sink-fn target-eff]
   (fx/chain> prev-effect (fx/provide> {:fx.observability/telemetry-sinks [sink-fn]} target-eff))))

(defn telemetry-sink-layer>
  "Creates a Layer providing `:fx.observability/telemetry-sinks`."
  ([sink-fn]
   (fx-layer/from-value> :fx.observability/telemetry-sinks [sink-fn]))
  ([sink-fn & more-sinks]
   (fx-layer/from-value> :fx.observability/telemetry-sinks (into [sink-fn] more-sinks))))

(defn tap-telemetry>
  "Observes execution of `effect`, emitting structured telemetry events with timing
   and outcome status to registered sinks without altering values or failure channels.
   
   Example:
     (fx.observability.telemetry/tap-telemetry> :http/request {:route \"/items\"} (fetch-items>))"
  ([event-type eff]
   (tap-telemetry> nil event-type eff nil))
  ([event-type-or-prev eff-or-type meta-or-eff]
   (if (fx/effect? event-type-or-prev)
     (tap-telemetry> event-type-or-prev eff-or-type meta-or-eff nil)
     (if (fx/effect? meta-or-eff)
       (->TapTelemetryEffect :tap-telemetry nil {:event event-type-or-prev :meta eff-or-type :body meta-or-eff}
                             event-type-or-prev meta-or-eff eff-or-type)
       (->TapTelemetryEffect :tap-telemetry nil {:event event-type-or-prev :meta meta-or-eff :body eff-or-type}
                             event-type-or-prev eff-or-type meta-or-eff))))
  ([prev-effect event-type eff meta-data]
   (->TapTelemetryEffect :tap-telemetry prev-effect {:event event-type :meta meta-data :body eff}
                         event-type eff meta-data)))

(defn tap-telemetry-event>
  "Emits a custom one-shot telemetry event map to registered telemetry sinks in context.
   
   Example:
     (-> (fx/succeed> {:status :ready})
         (fx.observability.telemetry/tap-telemetry-event> :service/ready {:details \"Initialized\"}))"
  ([event-type]
   (tap-telemetry-event> nil event-type {}))
  ([event-type-or-prev data-or-type]
   (if (fx/effect? event-type-or-prev)
     (tap-telemetry-event> event-type-or-prev data-or-type {})
     (tap-telemetry-event> nil event-type-or-prev data-or-type)))
  ([prev-effect event-type data]
   (let [eff (fx/map-ctx> (fn [val ctx]
                            (let [event {:event     event-type
                                         :data      (or data {})
                                         :timestamp (current-timestamp)
                                         :value     val}]
                              (emit-telemetry! ctx event)
                              val)))]
     (if prev-effect (fx/chain> prev-effect eff) eff))))
