(ns fx.observability.diagnostics
  "Rich defect tracking, Cause algebra, sandboxing, and causal AST execution trace rendering for fx."
  (:require [clojure.string :as str]
            [fx.core :as fx]
            [fx.utils :as utils]))

;; ---------------------------------------------------------------------------
;; Cause Protocols & Records
;; ---------------------------------------------------------------------------

(defprotocol ICause
  "Protocol for structured Cause error tree algebra nodes."
  (cause-type [this] "Returns the keyword classification of this cause node."))

(defrecord Fail [failure]
  ICause
  (cause-type [_] :fail))

(defrecord Die [defect]
  ICause
  (cause-type [_] :die))

(defrecord Interrupt [reason]
  ICause
  (cause-type [_] :interrupt))

(defrecord Sequential [first second]
  ICause
  (cause-type [_] :sequential))

(defrecord Parallel [causes]
  ICause
  (cause-type [_] :parallel))

;; ---------------------------------------------------------------------------
;; Cause Constructors & Predicates
;; ---------------------------------------------------------------------------

(defn cause-fail
  "Creates a Cause representing a checked domain Failure."
  [failure]
  (->Fail failure))

(defn cause-die
  "Creates a Cause representing an unexpected defect / Throwable exception."
  [defect]
  (->Die defect))

(defn cause-interrupt
  "Creates a Cause representing computation cancellation or interruption."
  [reason]
  (->Interrupt reason))

(defn cause-sequential
  "Creates a composite Cause representing sequential failures."
  [c1 c2]
  (->Sequential c1 c2))

(defn cause-parallel
  "Creates a composite Cause representing accumulated parallel failures."
  [causes]
  (->Parallel (vec causes)))

(defn cause?
  "Returns true if `x` implements ICause."
  [x]
  (satisfies? ICause x))

(defn fail-cause?
  "Returns true if `x` is a Fail cause."
  [x]
  (instance? Fail x))

(defn die-cause?
  "Returns true if `x` is a Die cause."
  [x]
  (instance? Die x))

(defn interrupt-cause?
  "Returns true if `x` is an Interrupt cause."
  [x]
  (instance? Interrupt x))

(defn sequential-cause?
  "Returns true if `x` is a Sequential cause."
  [x]
  (instance? Sequential x))

(defn parallel-cause?
  "Returns true if `x` is a Parallel cause."
  [x]
  (instance? Parallel x))

(defn failures
  "Collects all IFailure values contained within `cause` tree into a vector."
  [cause]
  (cond
    (fail-cause? cause) [(:failure cause)]
    (sequential-cause? cause) (into (failures (:first cause)) (failures (:second cause)))
    (parallel-cause? cause) (into [] (mapcat failures (:causes cause)))
    :else []))

(defn defects
  "Collects all defect Throwables contained within `cause` tree into a vector."
  [cause]
  (cond
    (die-cause? cause) [(:defect cause)]
    (sequential-cause? cause) (into (defects (:first cause)) (defects (:second cause)))
    (parallel-cause? cause) (into [] (mapcat defects (:causes cause)))
    :else []))

;; ---------------------------------------------------------------------------
;; Sandboxing Continuation Frames
;; ---------------------------------------------------------------------------

(defrecord SandboxFrame [context]
  fx/IContinuation
  (-resume [_ val current-context stack]
    (if (fx/failure? val)
      [nil (->Fail val) current-context stack]
      [nil val current-context stack]))

  fx/IUnwindable
  (-unwind [_ exception rest-stack]
    {:value (->Die exception) :context context :rest-stack rest-stack}))

(defrecord SandboxEffect [tag prev-effect data body-eff]
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
        [eff nil context (conj stack (->SandboxFrame context))]))))

(defrecord UnsandboxFrame []
  fx/IContinuation
  (-resume [_ val current-context stack]
    (cond
      (fail-cause? val)
      [nil (:failure val) current-context stack]

      (die-cause? val)
      (throw (:defect val))

      (interrupt-cause? val)
      [nil (fx/make-failure :interrupted {:reason (:reason val)}) current-context stack]

      (sequential-cause? val)
      (if-let [d (first (defects val))]
        (throw d)
        (if-let [f (first (failures val))]
          [nil f current-context stack]
          [nil (fx/make-failure :unknown-failure {}) current-context stack]))

      (parallel-cause? val)
      (if-let [d (first (defects val))]
        (throw d)
        (if-let [f (first (failures val))]
          [nil f current-context stack]
          [nil (fx/make-failure :parallel-failure {:failures (failures val)}) current-context stack]))

      :else
      [nil val current-context stack])))

(defrecord UnsandboxEffect [tag prev-effect data body-eff]
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
        [eff nil context (conj stack (->UnsandboxFrame))]))))

;; ---------------------------------------------------------------------------
;; Sandboxing Combinators
;; ---------------------------------------------------------------------------

(defn sandbox>
  "Catches both domain `IFailure` records and unexpected thrown `Throwable` defects,
   exposing them as a typed `Cause` value (`Fail` or `Die`) on the success channel.
   
   Example:
     (-> (risky-operation>)
         (fx.observability.diagnostics/sandbox>)
         (fx/map> (fn [cause-or-val]
                    (if (fx.observability.diagnostics/cause? cause-or-val)
                      (handle-cause cause-or-val)
                      cause-or-val))))"
  ([eff]
   (sandbox> nil eff))
  ([prev-effect eff]
   (->SandboxEffect :sandbox prev-effect {:body eff} eff)))

(defn unsandbox>
  "Inverts `sandbox>` by re-emitting `Cause` values as effect failures (`Fail`) or
   throwing unexpected defects (`Die`). Passes non-Cause success values through.
   
   Example:
     (-> (fx.observability.diagnostics/sandbox> (work>))
         (fx/map> transform-cause)
         (fx.observability.diagnostics/unsandbox>))"
  ([eff]
   (unsandbox> nil eff))
  ([prev-effect eff]
   (->UnsandboxEffect :unsandbox prev-effect {:body eff} eff)))

;; ---------------------------------------------------------------------------
;; Formatting & Visualization
;; ---------------------------------------------------------------------------

(defn- render-cause-node
  [cause indent is-last?]
  (let [prefix (str indent (if is-last? "└── " "├── "))
        child-indent (str indent (if is-last? "    " "│   "))]
    (cond
      (fail-cause? cause)
      (let [f (:failure cause)
            f-tag (fx/tag f)
            f-data (fx/error-data f)]
        [(str prefix "Cause.Fail: [" (if (keyword? f-tag) (str f-tag) (pr-str f-tag)) "] " (pr-str f-data))])

      (die-cause? cause)
      (let [d (:defect cause)
            msg #?(:clj (if (instance? Throwable d) (.getMessage ^Throwable d) (str d))
                   :cljs (str d))
            cls #?(:clj (if (instance? Throwable d) (.getName (class d)) "Error")
                   :cljs "Error")]
        [(str prefix "Cause.Die: " cls ": " msg)])

      (interrupt-cause? cause)
      [(str prefix "Cause.Interrupt: " (pr-str (:reason cause)))]

      (sequential-cause? cause)
      (into [(str prefix "Cause.Sequential:")]
            (concat (render-cause-node (:first cause) child-indent false)
                    (render-cause-node (:second cause) child-indent true)))

      (parallel-cause? cause)
      (let [children (:causes cause)
            n (count children)]
        (into [(str prefix "Cause.Parallel:")]
              (mapcat (fn [i c]
                        (render-cause-node c child-indent (= i (dec n))))
                      (range n)
                      children)))

      :else
      [(str prefix "Cause.Unknown: " (pr-str cause))])))

(defn render-cause
  "Formats a `Cause` tree into a human-readable hierarchical ASCII string."
  [cause]
  (if-not (cause? cause)
    (str cause)
    (cond
      (fail-cause? cause)
      (let [f (:failure cause)]
        (str "Cause.Fail: [" (fx/tag f) "] " (pr-str (fx/error-data f))))

      (die-cause? cause)
      (let [d (:defect cause)
            msg #?(:clj (if (instance? Throwable d) (.getMessage ^Throwable d) (str d))
                   :cljs (str d))
            cls #?(:clj (if (instance? Throwable d) (.getName (class d)) "Error")
                   :cljs "Error")]
        (str "Cause.Die: " cls ": " msg))

      (interrupt-cause? cause)
      (str "Cause.Interrupt: " (pr-str (:reason cause)))

      (or (sequential-cause? cause) (parallel-cause? cause))
      (let [header (if (sequential-cause? cause) "Cause.Sequential:" "Cause.Parallel:")
            children (if (sequential-cause? cause)
                       [(:first cause) (:second cause)]
                       (:causes cause))
            n (count children)
            lines (mapcat (fn [i c]
                            (render-cause-node c "" (= i (dec n))))
                          (range n)
                          children)]
        (str/join "\n" (cons header lines)))

      :else (str cause))))

(defn render-execution-trace
  "Inspects the AST pipeline of `leaf-effect` using `fx.utils` and formats an execution trace
   showing each step from root to leaf, optionally highlighting `failure-or-cause`."
  ([leaf-effect]
   (render-execution-trace leaf-effect nil))
  ([leaf-effect failure-or-cause]
   (let [effects (utils/effect-seq leaf-effect)
         _n (count effects)
         steps (map-indexed (fn [idx eff]
                              (let [step-num (inc idx)
                                    eff-tag (fx/tag eff)
                                    eff-data (:data eff)
                                    data-summary (if (seq eff-data) (str " " (pr-str eff-data)) "")]
                                (str "  " step-num ". (" (name (or eff-tag :effect)) ">" data-summary ")")))
                            effects)
         header "[fx AST Execution Trace]"
         failure-line (when failure-or-cause
                        (str "  └── FAILED: " (if (cause? failure-or-cause)
                                                (render-cause failure-or-cause)
                                                (if (fx/failure? failure-or-cause)
                                                  (str "[" (fx/tag failure-or-cause) "] " (pr-str (fx/error-data failure-or-cause)))
                                                  (pr-str failure-or-cause)))))]
     (str/join "\n" (concat [header] steps (when failure-line [failure-line]))))))
