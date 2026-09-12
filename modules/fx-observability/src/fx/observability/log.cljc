(ns fx.observability.log
  "Zero-dependency contextual and structured logging for fx effect pipelines."
  (:require [fx.core :as fx]
            [fx.layer :as fx-layer])
  #?(:clj (:import (java.time Instant)
                   (java.lang System$Logger$Level))))

;; ---------------------------------------------------------------------------
;; Sinks & Formatting
;; ---------------------------------------------------------------------------

(defn- format-log-entry
  [{:keys [level message data timestamp annotations trace-id span-id]}]
  (let [ts-str (str timestamp)
        lvl-str (str "[" (name level) "]")
        ctx-str (when (seq annotations) (str " annotations=" (pr-str annotations)))
        trace-str (when trace-id (str " trace_id=" trace-id))
        span-str (when span-id (str " span_id=" span-id))
        data-str (when (and (some? data) (not= data {})) (str " data=" (pr-str data)))]
    (str ts-str " " lvl-str " " message
         trace-str span-str ctx-str data-str)))

(defn console-logger-sink
  "Default console sink that formats and prints log entries to *out* (or *err* for :error)."
  [entry]
  (try
    (let [formatted (format-log-entry entry)]
      (if (= (:level entry) :error)
        #?(:clj (binding [*out* *err*]
                  (println formatted))
           :cljs (js/console.error formatted))
        (println formatted)))
    (catch #?(:clj Throwable :cljs :default) _ nil)))

(defn system-logger-sink
  "Java System.Logger sink (delegates to System.getLogger on JVM, console on JS)."
  [entry]
  (try
    #?(:clj
       (let [logger (System/getLogger "fx.observability.log")
             sys-level (case (:level entry)
                         :debug System$Logger$Level/DEBUG
                         :info  System$Logger$Level/INFO
                         :warn  System$Logger$Level/WARNING
                         :error System$Logger$Level/ERROR
                         System$Logger$Level/INFO)]
         (.log logger sys-level (format-log-entry entry)))
       :cljs
       (console-logger-sink entry))
    (catch #?(:clj Throwable :cljs :default) _ nil)))

(def ^:dynamic *default-logger-sink* console-logger-sink)

;; ---------------------------------------------------------------------------
;; Level Utilities
;; ---------------------------------------------------------------------------

(def level-priorities
  {:debug 10
   :info  20
   :warn  30
   :error 40})

(defn- should-log?
  [configured-level entry-level]
  (if-not configured-level
    true
    (let [min-p (get level-priorities configured-level 10)
          entry-p (get level-priorities entry-level 20)]
      (>= entry-p min-p))))

;; ---------------------------------------------------------------------------
;; Effect Records
;; ---------------------------------------------------------------------------

(defrecord LogEffect [tag prev-effect data level message log-data]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [configured-level (or (:fx.observability/log-level context)
                                 (:fx/log-level context))
            min-level (or configured-level :debug)]
        (when (should-log? min-level level)
          (let [annotations (or (get context :fx.observability/log-annotations)
                                (get context :fx/log-annotations)
                                {})
                spans (or (get context :fx.observability/trace-spans)
                          (get context :fx/trace-spans)
                          [])
                active-span (peek spans)
                trace-ctx (or (get context :fx.observability/trace-context)
                              (get context :fx/trace-context))
                trace-id (or (:trace-id active-span)
                             (:trace-id trace-ctx))
                span-id (or (:span-id active-span)
                            (:span-id trace-ctx))
                entry {:level       level
                       :message     message
                       :data        (or log-data {})
                       :timestamp   #?(:clj (Instant/now) :cljs (js/Date.))
                       :annotations annotations
                       :spans       spans
                       :trace-id    trace-id
                       :span-id     span-id}
                sink (or (:fx.observability/logger context)
                         (:fx/logger context)
                         *default-logger-sink*)]
            (try
              (sink entry)
              (catch #?(:clj Throwable :cljs :default) _ nil))))
        [nil val context stack]))))

(defrecord AnnotateLogsEffect [tag prev-effect data annotations]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [next-ctx (update context :fx.observability/log-annotations (fnil merge {}) annotations)]
        [nil val next-ctx stack]))))

;; ---------------------------------------------------------------------------
;; Constructors & Combinators
;; ---------------------------------------------------------------------------

(defn log>
  "Creates a structured log effect with the specified `level`, `message`, and optional `log-data` map.
   When chained in an effect pipeline, preserves and propagates the upstream value."
  ([level message]
   (log> nil level message nil))
  ([level-or-prev message-or-level data-or-message]
   (if (fx/effect? level-or-prev)
     (log> level-or-prev message-or-level data-or-message nil)
     (log> nil level-or-prev message-or-level data-or-message)))
  ([prev-effect level message log-data]
   (->LogEffect (keyword (str "log-" (name level))) prev-effect
                {:level level :message message :data log-data}
                level message log-data)))

(defn log-debug>
  "Creates a debug-level log effect.
   
   Examples:
     (fx.observability.log/log-debug> \"Cache hit\" {:key \"users:42\"})
     (-> (fetch-user 42)
         (fx.observability.log/log-debug> \"Fetched user\"))"
  ([message]
   (log> :debug message nil))
  ([a b]
   (if (fx/effect? a)
     (log> a :debug b nil)
     (log> :debug a b)))
  ([prev-effect message log-data]
   (log> prev-effect :debug message log-data)))

(defn log-info>
  "Creates an info-level log effect.
   
   Examples:
     (fx.observability.log/log-info> \"Server started\" {:port 8080})
     (-> (process-order order)
         (fx.observability.log/log-info> \"Order processed\"))"
  ([message]
   (log> :info message nil))
  ([a b]
   (if (fx/effect? a)
     (log> a :info b nil)
     (log> :info a b)))
  ([prev-effect message log-data]
   (log> prev-effect :info message log-data)))

(defn log-warn>
  "Creates a warn-level log effect.
   
   Examples:
     (fx.observability.log/log-warn> \"Rate limit threshold exceeded\" {:current 95 :max 100})
     (-> (retry-operation op)
         (fx.observability.log/log-warn> \"Retrying operation\"))"
  ([message]
   (log> :warn message nil))
  ([a b]
   (if (fx/effect? a)
     (log> a :warn b nil)
     (log> :warn a b)))
  ([prev-effect message log-data]
   (log> prev-effect :warn message log-data)))

(defn log-error>
  "Creates an error-level log effect.
   
   Examples:
     (fx.observability.log/log-error> \"Database connection failed\" {:error (ex-message e)})
     (-> (handle-failure err)
         (fx.observability.log/log-error> \"Handled failure\"))"
  ([message]
   (log> :error message nil))
  ([a b]
   (if (fx/effect? a)
     (log> a :error b nil)
     (log> :error a b)))
  ([prev-effect message log-data]
   (log> prev-effect :error message log-data)))

(defn annotate-logs>
  "Injects structured metadata key-values into `:fx.observability/log-annotations` in the execution context.
   All subsequent effects and log calls in the downstream pipeline will automatically inherit
   these annotations.
   
   Example:
     (-> (fx.observability.log/annotate-logs> {:request-id \"req-123\" :user-id 42})
         (fx/mapcat> (fn [_] (fx.observability.log/log-info> \"User logged in\"))))"
  ([annotations]
   (annotate-logs> nil annotations))
  ([prev-effect annotations]
   (->AnnotateLogsEffect :annotate-logs prev-effect {:annotations annotations} annotations)))

(defn with-logger>
  "Executes `effect` with a scoped custom log sink function `(fn [log-entry] ...)`.
   
   Example:
     (fx.observability.log/with-logger> custom-sink-fn (do-work>))"
  ([logger-fn-or-eff eff-or-logger-fn]
   (if (fx/effect? logger-fn-or-eff)
     (fx/provide> {:fx.observability/logger eff-or-logger-fn} logger-fn-or-eff)
     (fx/provide> {:fx.observability/logger logger-fn-or-eff} eff-or-logger-fn)))
  ([prev-effect logger-fn target-eff]
   (fx/chain> prev-effect (fx/provide> {:fx.observability/logger logger-fn} target-eff))))

(defn with-log-level>
  "Executes `effect` with a scoped minimum log level (:debug, :info, :warn, :error).
   
   Example:
     (fx.observability.log/with-log-level> :warn (noisy-pipeline>))"
  ([level-or-eff eff-or-level]
   (if (fx/effect? level-or-eff)
     (fx/provide> {:fx.observability/log-level eff-or-level} level-or-eff)
     (fx/provide> {:fx.observability/log-level level-or-eff} eff-or-level)))
  ([prev-effect level target-eff]
   (fx/chain> prev-effect (fx/provide> {:fx.observability/log-level level} target-eff))))

(defn logger-layer>
  "Creates a Layer providing `:fx.observability/logger` (and optionally `:fx.observability/log-level` and `:fx.observability/log-annotations`).
   Accepts a logger sink function (e.g. `console-logger-sink`, `system-logger-sink`) or an options map
   `{:logger sink-fn :level :info :annotations {...}}`."
  ([sink-or-opts]
   (if (map? sink-or-opts)
     (fx-layer/from-values> (cond-> {}
                              (or (:logger sink-or-opts) (:sink sink-or-opts))
                              (assoc :fx.observability/logger (or (:logger sink-or-opts) (:sink sink-or-opts)))
                              (:level sink-or-opts)
                              (assoc :fx.observability/log-level (:level sink-or-opts))
                              (:annotations sink-or-opts)
                              (assoc :fx.observability/log-annotations (:annotations sink-or-opts))))
     (fx-layer/from-value> :fx.observability/logger sink-or-opts)))
  ([sink level]
   (fx-layer/from-values> {:fx.observability/logger sink :fx.observability/log-level level}))
  ([sink level annotations]
   (fx-layer/from-values> {:fx.observability/logger sink :fx.observability/log-level level :fx.observability/log-annotations annotations})))

(defn log-level-layer>
  "Creates a Layer providing `:fx.observability/log-level`."
  [level]
  (fx-layer/from-value> :fx.observability/log-level level))

(defn log-annotations-layer>
  "Creates a Layer providing `:fx.observability/log-annotations`."
  [annotations]
  (fx-layer/from-value> :fx.observability/log-annotations annotations))
