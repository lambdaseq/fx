(ns fx.observability.trace
  "Zero-dependency distributed tracing, span lifecycles, and W3C Trace Context propagation for fx."
  (:require [clojure.string :as str]
            [fx.core :as fx]
            [fx.layer :as fx-layer])
  #?(:clj (:import (java.util.concurrent ThreadLocalRandom))))

;; ---------------------------------------------------------------------------
;; ID Generation & Formatting
;; ---------------------------------------------------------------------------

(defn- current-nano-time []
  #?(:clj  (System/nanoTime)
     :cljs (* (js/Date.now) 1000000)))

(defn random-trace-id
  "Generates a random 32-character lowercase hex string suitable for W3C trace-id."
  []
  #?(:clj
     (let [rng (ThreadLocalRandom/current)
           msb (.nextLong rng)
           lsb (.nextLong rng)]
       (format "%016x%016x" msb lsb))
     :cljs
     (let [hex-chars "0123456789abcdef"]
       (apply str (repeatedly 32 #(rand-nth hex-chars))))))

(defn random-span-id
  "Generates a random 16-character lowercase hex string suitable for W3C span-id / parent-id."
  []
  #?(:clj
     (let [rng (ThreadLocalRandom/current)
           id (.nextLong rng)]
       (format "%016x" id))
     :cljs
     (let [hex-chars "0123456789abcdef"]
       (apply str (repeatedly 16 #(rand-nth hex-chars))))))

;; ---------------------------------------------------------------------------
;; W3C Trace Context Codec
;; ---------------------------------------------------------------------------

(def ^:private w3c-traceparent-regex
  #"^([0-9a-f]{2})-([0-9a-f]{32})-([0-9a-f]{16})-([0-9a-f]{2})$")

(defn parse-traceparent
  "Parses a W3C `traceparent` header string into a trace context map, or nil if invalid."
  [traceparent-str]
  (when (and (string? traceparent-str) (not (str/blank? traceparent-str)))
    (when-let [[_ version trace-id parent-id flags] (re-matches w3c-traceparent-regex (str/trim (str/lower-case traceparent-str)))]
      (when (and (not= trace-id "00000000000000000000000000000000")
                 (not= parent-id "0000000000000000"))
        {:version        version
         :trace-id       trace-id
         :parent-span-id parent-id
         :sampled?       (= flags "01")
         :trace-flags    flags}))))

(defn format-traceparent
  "Formats trace context map or IDs into a valid W3C `traceparent` header string."
  ([trace-id span-id]
   (format-traceparent trace-id span-id true))
  ([trace-id span-id sampled?]
   (str "00-"
        (or trace-id (random-trace-id))
        "-"
        (or span-id (random-span-id))
        "-"
        (if sampled? "01" "00"))))

(defn extract-trace-context
  "Extracts W3C trace context from incoming request/carrier map (checks :headers, string/keyword keys)."
  [carrier]
  (when (map? carrier)
    (let [headers (or (:headers carrier) carrier)
          header-val (or (get headers "traceparent")
                         (get headers "Traceparent")
                         (get headers :traceparent)
                         (get headers :Traceparent))]
      (parse-traceparent header-val))))

(defn inject-trace-context
  "Injects `traceparent` into `carrier` (or carrier's `:headers` if present) using active span or trace-ctx."
  ([carrier trace-ctx]
   (when (map? carrier)
     (let [{:keys [trace-id span-id parent-span-id sampled?]} trace-ctx
           active-span-id (or span-id parent-span-id (random-span-id))
           tp (format-traceparent trace-id active-span-id (if (some? sampled?) sampled? true))]
       (if (contains? carrier :headers)
         (assoc-in carrier [:headers "traceparent"] tp)
         (assoc carrier "traceparent" tp))))))

;; ---------------------------------------------------------------------------
;; Default Reporters
;; ---------------------------------------------------------------------------

(def ^:dynamic *default-span-reporter* (fn [_span] nil))

;; ---------------------------------------------------------------------------
;; Continuation Frame Records
;; ---------------------------------------------------------------------------

(defrecord SpanFrame [span-name span-id trace-id parent-id start-nano old-context initial-attrs]
  fx/IContinuation
  (-resume [_ val current-context stack]
    (let [end-nano (current-nano-time)
          duration-ms (/ (double (- end-nano start-nano)) 1000000.0)
          spans (or (get current-context :fx.observability/trace-spans)
                    (get current-context :fx/trace-spans)
                    [])
          active-span (some #(when (= (:span-id %) span-id) %) spans)
          final-attrs (merge initial-attrs (:attributes active-span))
          status (if (fx/failure? val) :failure :success)
          err-data (when (fx/failure? val) (fx/failure->value val))
          completed-span {:name        span-name
                          :trace-id    trace-id
                          :span-id     span-id
                          :parent-id   parent-id
                          :start-nano  start-nano
                          :end-nano    end-nano
                          :duration-ms duration-ms
                          :status      status
                          :attributes  (or final-attrs {})
                          :error       err-data}
          reporter (or (:fx.observability/span-reporter current-context)
                       (:fx/span-reporter current-context)
                       (:fx.observability/span-reporter old-context)
                       (:fx/span-reporter old-context)
                       *default-span-reporter*)]
      (try
        (reporter completed-span)
        (catch #?(:clj Throwable :cljs :default) _ nil))
      (let [remaining-spans (vec (remove #(= (:span-id %) span-id) spans))
            next-ctx (assoc current-context :fx.observability/trace-spans remaining-spans)]
        [nil val next-ctx stack])))

  fx/IUnwindable
  (-unwind [_ exception _rest-stack]
    (let [end-nano (current-nano-time)
          duration-ms (/ (double (- end-nano start-nano)) 1000000.0)
          completed-span {:name        span-name
                          :trace-id    trace-id
                          :span-id     span-id
                          :parent-id   parent-id
                          :start-nano  start-nano
                          :end-nano    end-nano
                          :duration-ms duration-ms
                          :status      :failure
                          :attributes  (or initial-attrs {})
                          :error       {:exception exception}}
          reporter (or (:fx.observability/span-reporter old-context)
                       (:fx/span-reporter old-context)
                       *default-span-reporter*)]
      (try
        (reporter completed-span)
        (catch #?(:clj Throwable :cljs :default) _ nil))
      nil)))

;; ---------------------------------------------------------------------------
;; Effect Records
;; ---------------------------------------------------------------------------

(defrecord WithSpanEffect [tag prev-effect data span-name body-eff attributes]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [start-nano (current-nano-time)
            spans (or (get context :fx.observability/trace-spans)
                      (get context :fx/trace-spans)
                      [])
            parent-span (peek spans)
            trace-ctx (or (:fx.observability/trace-context context)
                          (:fx/trace-context context))
            trace-id (or (:trace-id parent-span)
                         (:trace-id trace-ctx)
                         (random-trace-id))
            parent-id (or (:span-id parent-span)
                          (:parent-span-id trace-ctx))
            span-id (random-span-id)
            span-entry {:name       span-name
                        :trace-id   trace-id
                        :span-id    span-id
                        :parent-id  parent-id
                        :start-nano start-nano
                        :attributes (or attributes {})}
            child-ctx (update context :fx.observability/trace-spans (fnil conj []) span-entry)
            eff (if (some? val)
                  (fx/chain> (fx/succeed> val) body-eff)
                  body-eff)]
        [eff nil child-ctx (conj stack (->SpanFrame span-name span-id trace-id parent-id start-nano context attributes))]))))

(defrecord SpanAttributesEffect [tag prev-effect data attributes]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [spans (or (get context :fx.observability/trace-spans)
                      (get context :fx/trace-spans)
                      [])
            next-spans (if (seq spans)
                         (let [idx (dec (count spans))
                               cur (nth spans idx)
                               updated (update cur :attributes (fnil merge {}) attributes)]
                           (assoc spans idx updated))
                         spans)]
        [nil val (assoc context :fx.observability/trace-spans next-spans) stack]))))

;; ---------------------------------------------------------------------------
;; Public Combinators
;; ---------------------------------------------------------------------------

(defn with-span>
  "Wraps execution of `effect` in a named distributed tracing span.
   Measures elapsed nanoseconds, tracks hierarchical parent-child span IDs in context,
   and invokes registered span reporters on completion (success or failure).
   
   Examples:
     (fx.observability.trace/with-span> \"db.query\" (query-db> sql))
     (fx.observability.trace/with-span> \"http.request\" {:route \"/users\"} (handle-request> req))
     (-> (prev-op) (fx.observability.trace/with-span> \"step-2\" (handle-request> req)))"
  ([span-name-or-prev body-or-span-name]
   (if (fx/effect? span-name-or-prev)
     (with-span> span-name-or-prev body-or-span-name {} (fx/succeed> nil))
     (with-span> nil span-name-or-prev {} body-or-span-name)))
  ([a b c]
   (if (fx/effect? a)
     (if (fx/effect? c)
       (with-span> a b {} c)
       (if (map? c)
         (with-span> a b c (fx/succeed> nil))
         (with-span> a b {} c)))
     (if (map? b)
       (with-span> nil a b c)
       (with-span> nil a {} b))))
  ([prev-effect span-name attributes body-eff]
   (let [attrs (if (map? attributes) attributes {})
         body (if (fx/effect? body-eff) body-eff (if (fx/effect? attributes) attributes (fx/succeed> nil)))]
     (->WithSpanEffect :with-span prev-effect {:name span-name :attributes attrs :body body}
                       span-name body attrs))))

(defn with-span-attributes>
  "Attaches key-value attribute metadata to the active execution span in context.
   
   Example:
     (-> (fetch-order id)
         (fx.observability.trace/with-span-attributes> {:order-id id :status :pending}))"
  ([attributes]
   (with-span-attributes> nil attributes))
  ([prev-effect attributes]
   (->SpanAttributesEffect :with-span-attributes prev-effect {:attributes attributes} attributes)))

(defn with-span-reporter>
  "Scopes a span reporter function `(fn [span-map] ...)` to `effect`.
   
   Example:
     (fx.observability.trace/with-span-reporter> (fn [span] (println \"Completed span:\" (:name span))) (work>))"
  ([reporter-fn-or-eff eff-or-reporter-fn]
   (if (fx/effect? reporter-fn-or-eff)
     (fx/provide> {:fx.observability/span-reporter eff-or-reporter-fn} reporter-fn-or-eff)
     (fx/provide> {:fx.observability/span-reporter reporter-fn-or-eff} eff-or-reporter-fn)))
  ([prev-effect reporter-fn target-eff]
   (fx/chain> prev-effect (fx/provide> {:fx.observability/span-reporter reporter-fn} target-eff))))

(defn with-trace-context>
  "Scopes a trace context map `{:trace-id ... :parent-span-id ...}` to `effect`.
   
   Example:
     (fx.observability.trace/with-trace-context> {:trace-id \"...\"} (handle-request> req))"
  ([trace-ctx-or-eff eff-or-trace-ctx]
   (if (fx/effect? trace-ctx-or-eff)
     (fx/provide> {:fx.observability/trace-context eff-or-trace-ctx} trace-ctx-or-eff)
     (fx/provide> {:fx.observability/trace-context trace-ctx-or-eff} eff-or-trace-ctx)))
  ([prev-effect trace-ctx target-eff]
   (fx/chain> prev-effect (fx/provide> {:fx.observability/trace-context trace-ctx} target-eff))))

(defn span-reporter-layer>
  "Creates a Layer providing `:fx.observability/span-reporter`."
  [reporter-fn]
  (fx-layer/from-value> :fx.observability/span-reporter reporter-fn))

(defn trace-context-layer>
  "Creates a Layer providing `:fx.observability/trace-context`."
  [trace-ctx]
  (fx-layer/from-value> :fx.observability/trace-context trace-ctx))

(defn extract-trace-context>
  "Creates an effect that extracts W3C trace context from incoming carrier/headers
   and binds it to `:fx.observability/trace-context` in context for downstream operations."
  ([carrier]
   (extract-trace-context> nil carrier))
  ([prev-effect carrier]
   (-> (fx/map-ctx> (fn [val _ctx]
                      (let [in-carrier (or carrier val)
                            extracted (extract-trace-context in-carrier)]
                        [val extracted])))
       (fx/mapcat> (fn [[val extracted]]
                     (if extracted
                       (fx/provide> {:fx.observability/trace-context extracted} (fx/succeed> val))
                       (fx/succeed> val))))
       (#(if prev-effect (fx/chain> prev-effect %) %)))))

(defn inject-trace-context>
  "Creates an effect that injects active trace context (W3C traceparent) into carrier map."
  ([carrier]
   (inject-trace-context> nil carrier))
  ([prev-effect carrier]
   (let [eff (fx/map-ctx> (fn [val ctx]
                            (let [target-carrier (or carrier val {})
                                  spans (or (get ctx :fx.observability/trace-spans)
                                            (get ctx :fx/trace-spans)
                                            [])
                                  active-span (peek spans)
                                  trace-ctx (or (when active-span
                                                  {:trace-id (:trace-id active-span)
                                                   :span-id  (:span-id active-span)})
                                                (:fx.observability/trace-context ctx)
                                                (:fx/trace-context ctx))]
                              (inject-trace-context target-carrier trace-ctx))))]
     (if prev-effect (fx/chain> prev-effect eff) eff))))
