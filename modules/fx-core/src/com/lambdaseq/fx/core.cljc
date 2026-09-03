(ns com.lambdaseq.fx.core
  (:refer-clojure :exclude [tap>])
  #?(:clj (:import (java.util.concurrent CompletableFuture)
                   (java.util.function Supplier))))

(declare run-sync! failure->value succeed> fail> make-failure chain> handle-try-exception)

;; ---------------------------------------------------------------------------
;; Protocols
;; ---------------------------------------------------------------------------

(defprotocol ITagged
  "Protocol representing a tagged data structure in the effect system."
  (tag [this]
    "Returns the keyword identifying the tag (e.g. effect tag or failure tag)."))

(defprotocol IFailure
  "Protocol representing a typed failure in the effect system."
  (error-data [this]
    "Returns the error payload/data of the failure."))

(defprotocol IEffect
  "Protocol representing an executable and inspectable effect in a computation pipeline."
  (prev-effect [this]
    "Returns the upstream (previous) effect in the chain, or nil if this is the root.")
  (-step [this value context stack]
    "Performs one execution step, returning:
     [next-effect next-value next-context next-stack]."))

(defprotocol IContinuation
  "Protocol for execution frames held on the heap continuation stack."
  (-resume [this value context stack]
    "Resumes execution when a sub-computation completes or an upstream step finishes, returning:
     [next-effect next-value next-context next-stack]."))

(defprotocol IUnwindable
  "Protocol for continuation frames that participate in exception unwinding and resource cleanup."
  (-unwind [this exception rest-stack]
    "Handles exception unwinding. Returns a map {:effect ... :value ... :context ... :rest-stack ...}
     or nil to continue unwinding."))

;; ---------------------------------------------------------------------------
;; Failure Record
;; ---------------------------------------------------------------------------

(defrecord Failure [tag error-data]
  ITagged
  (tag [_] tag)
  IFailure
  (error-data [_] error-data))

;; ---------------------------------------------------------------------------
;; Helpers & Predicates
;; ---------------------------------------------------------------------------

(defn effect?
  "Returns true if `x` implements IEffect."
  [x]
  #?(:clj  (instance? com.lambdaseq.fx.core.IEffect x)
     :cljs (satisfies? IEffect x)))

(defn failure?
  "Returns true if `x` implements IFailure."
  [x]
  #?(:clj  (instance? com.lambdaseq.fx.core.IFailure x)
     :cljs (satisfies? IFailure x)))

(defn- attach-root [root-eff target-eff]
  (if (nil? target-eff)
    root-eff
    (if (nil? (:prev-effect target-eff))
      (assoc target-eff :prev-effect root-eff)
      (assoc target-eff :prev-effect (attach-root root-eff (:prev-effect target-eff))))))

(defn chain>
  "Chains `current-effect` onto `prev-effect` by setting `prev-effect` as its upstream dependency."
  [prev-effect current-effect]
  (if (nil? (:prev-effect current-effect))
    (assoc current-effect :prev-effect prev-effect)
    (attach-root prev-effect current-effect)))

(defn make-failure
  "Creates a new Failure record given a `tag` keyword and an `err` payload map or value."
  [tag err]
  (Failure. tag err))

(defmacro maybe-propagate-failure
  "If `v` is a failure, short-circuits and returns `v` directly. Otherwise evaluates `body`."
  [v & body]
  `(let [v# ~v]
     (if (failure? v#)
       v#
       (do ~@body))))

(defn failure->value
  "Converts an IFailure instance into a plain map `{:tag ... :error-data ...}`."
  [failure]
  {:tag        (tag failure)
   :error-data (error-data failure)})

(defn- handle-try-exception [catch-handler e]
  (cond
    (nil? catch-handler)
    (make-failure :try e)

    (keyword? catch-handler)
    (make-failure catch-handler e)

    (effect? catch-handler)
    catch-handler

    (map? catch-handler)
    (let [matched-handler (some (fn [[k handler]]
                                  (cond
                                    #?@(:clj [(and (class? k) (instance? k e)) handler])
                                    (and (fn? k) (k e)) handler
                                    (= k :default) handler
                                    :else nil))
                                catch-handler)]
      (if matched-handler
        (handle-try-exception matched-handler e)
        (make-failure :try e)))

    (ifn? catch-handler)
    (let [res (catch-handler e)]
      (if (failure? res)
        res
        (make-failure :try res)))

    :else
    (make-failure :try e)))

;; ---------------------------------------------------------------------------
;; Continuation Frame Records with Polymorphic -resume & -unwind Dispatch
;; ---------------------------------------------------------------------------

(defrecord StepEffectFrame [effect]
  IContinuation
  (-resume [_ val context stack]
    (-step effect val context stack)))

(defrecord RestoreContextFrame [context]
  IContinuation
  (-resume [_ val _current-context stack]
    [nil val context stack]))

(defrecord TryFrame [catch context]
  IContinuation
  (-resume [_ val _current-context stack]
    [nil val context stack])
  IUnwindable
  (-unwind [_ e rest-stack]
    (let [handler-res (handle-try-exception catch e)]
      (if (effect? handler-res)
        {:effect (chain> (succeed> e) handler-res) :context context :rest-stack rest-stack}
        {:value handler-res :context context :rest-stack rest-stack}))))

(defrecord EnsureFrame [original-value context]
  IContinuation
  (-resume [_ val _current-context stack]
    (if (failure? val)
      [nil val context stack]
      [nil original-value context stack]))
  IUnwindable
  (-unwind [_ e rest-stack]
    {:value (make-failure :ensure e) :context context :rest-stack rest-stack}))

(defrecord AcquireReleaseFinishFrame [use-res]
  IContinuation
  (-resume [_ rel-res context stack]
    (if (failure? rel-res)
      [nil rel-res context stack]
      [nil use-res context stack])))

(defrecord AcquireReleaseUseFrame [resource release context]
  IContinuation
  (-resume [_ use-res _current-context stack]
    (let [rel-res (try
                    (if (fn? release) (release resource) release)
                    (catch #?(:clj Throwable :cljs :default) e
                      (make-failure :release-error e)))]
      (if (effect? rel-res)
        [rel-res nil context (conj stack (->AcquireReleaseFinishFrame use-res))]
        (cond
          (failure? use-res) [nil use-res context stack]
          (failure? rel-res) [nil rel-res context stack]
          :else [nil use-res context stack]))))
  IUnwindable
  (-unwind [_ _ _]
    (try
      (if (fn? release) (release resource) release)
      (catch #?(:clj Throwable :cljs :default) _ nil))
    nil))

(defrecord AcquireResourceFrame [use release context]
  IContinuation
  (-resume [_ resource _current-context stack]
    (if (failure? resource)
      [nil resource context stack]
      (let [use-res (try
                      (if (fn? use) (use resource) use)
                      (catch #?(:clj Throwable :cljs :default) e
                        (try (if (fn? release) (release resource) release)
                             (catch #?(:clj Throwable :cljs :default) _ nil))
                        (throw e)))]
        (if (effect? use-res)
          (let [eff (if (nil? (:prev-effect use-res)) (chain> (succeed> resource) use-res) use-res)]
            [eff nil context (conj stack (->AcquireReleaseUseFrame resource release context))])
          (let [rel-res (try
                          (if (fn? release) (release resource) release)
                          (catch #?(:clj Throwable :cljs :default) e
                            (make-failure :release-error e)))]
            (if (effect? rel-res)
              [rel-res nil context (conj stack (->AcquireReleaseFinishFrame use-res))]
              (cond
                (failure? use-res) [nil use-res context stack]
                (failure? rel-res) [nil rel-res context stack]
                :else [nil use-res context stack]))))))))

(defrecord RetryFrame [target value policy attempt delay-ms]
  IContinuation
  (-resume [_ res context stack]
    (if (and (failure? res)
             (< attempt (get policy :max-attempts 3))
             ((get policy :retry-if (constantly true)) res))
      (do
        #?(:clj (when (pos? delay-ms) (Thread/sleep delay-ms)) :cljs nil)
        (let [next-delay (long (* delay-ms (get policy :backoff-factor 1.0)))]
          [target nil context (conj stack (->RetryFrame target value policy (inc attempt) next-delay))]))
      [nil res context stack])))

(defrecord AllFrame [remaining results]
  IContinuation
  (-resume [_ res context stack]
    (if (failure? res)
      [nil res context stack]
      (let [new-results (conj results res)]
        (if (empty? remaining)
          [nil new-results context stack]
          (let [[next-eff & rest-effs] remaining]
            [next-eff nil context (conj stack (->AllFrame rest-effs new-results))]))))))

(defrecord ForEachFrame [f remaining results]
  IContinuation
  (-resume [_ res context stack]
    (if (failure? res)
      [nil res context stack]
      (let [new-results (conj results res)]
        (if (empty? remaining)
          [nil new-results context stack]
          (let [[next-item & rest-items] remaining
                next-eff (f next-item)]
            (if (effect? next-eff)
              [next-eff nil context (conj stack (->ForEachFrame f rest-items new-results))]
              (if (failure? next-eff)
                [nil next-eff context stack]
                [nil (conj new-results next-eff) context stack]))))))))

(defrecord ZipWithBFrame [res-a f]
  IContinuation
  (-resume [_ res-b context stack]
    (if (failure? res-b)
      [nil res-b context stack]
      [nil (f res-a res-b) context stack])))

(defrecord ZipWithAFrame [eff-b value f]
  IContinuation
  (-resume [_ res-a context stack]
    (if (failure? res-a)
      [nil res-a context stack]
      (let [b (if (and (some? value) (effect? eff-b) (nil? (:prev-effect eff-b)))
                (chain> (succeed> value) eff-b)
                eff-b)]
        (if (effect? b)
          [b nil context (conj stack (->ZipWithBFrame res-a f))]
          [nil (f res-a b) context stack])))))

(defrecord IfFrame [value then else]
  IContinuation
  (-resume [_ cond-res context stack]
    (if (failure? cond-res)
      [nil cond-res context stack]
      (let [branch (if cond-res then else)
            branch-eff (if (nil? (:prev-effect branch))
                         (chain> (succeed> value) branch)
                         branch)]
        [branch-eff nil context stack]))))

(defrecord CondFrame [value expr-eff remaining]
  IContinuation
  (-resume [_ test-res context stack]
    (if (failure? test-res)
      [nil test-res context stack]
      (if test-res
        (let [e-eff (if (nil? (:prev-effect expr-eff))
                      (chain> (succeed> value) expr-eff)
                      expr-eff)]
          [e-eff nil context stack])
        (if (empty? remaining)
          [nil (make-failure :cond :no-conditions) context stack]
          (let [[[next-test next-expr] & rest-conds] remaining
                t-eff (if (nil? (:prev-effect next-test))
                        (chain> (succeed> value) next-test)
                        next-test)]
            [t-eff nil context (conj stack (->CondFrame value next-expr rest-conds))]))))))

;; ---------------------------------------------------------------------------
;; Concrete Effect Records with Polymorphic -step Dispatch
;; ---------------------------------------------------------------------------

(defrecord Effect [tag prev-effect data]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      [nil val context stack])))

(defrecord SucceedEffect [tag prev-effect data value]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      [nil value context stack])))

(defrecord FailEffect [tag prev-effect data failure]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      [nil failure context stack])))

(defrecord MapEffect [tag prev-effect data f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        [nil (f val) context stack]))))

(defrecord MapCtxEffect [tag prev-effect data f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        [nil (f val context) context stack]))))

(defrecord TapEffect [tag prev-effect data f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (do
          (f val)
          [nil val context stack])))))

(defrecord TapErrorEffect [tag prev-effect data f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        (do
          (f val)
          [nil val context stack])
        [nil val context stack]))))

(defrecord DoCtxEffect [tag prev-effect data f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (do
          (f val context)
          [nil val context stack])))))

(defrecord ContextEffect [tag prev-effect data key default]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (some? key)
        [nil (get context key default) context stack]
        [nil context context stack]))))

(defrecord ProvideEffect [tag prev-effect data body context-map]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (nil? body)
        [nil val context stack]
        (let [child-ctx (merge context context-map)
              eff (if (some? val)
                    (chain> (succeed> val) body)
                    body)]
          [eff nil child-ctx (conj stack (->RestoreContextFrame context))])))))

(defrecord EnsureEffect [tag prev-effect data finalizer]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [fin-input (if (failure? val) nil val)
            fin-eff (if (nil? (:prev-effect finalizer))
                      (chain> (succeed> fin-input) finalizer)
                      finalizer)]
        [fin-eff nil context (conj stack (->EnsureFrame val context))]))))

(defrecord AcquireReleaseEffect [tag prev-effect data acquire use release]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (some? acquire)
        (let [acq-eff (if (and (some? val) (effect? acquire) (nil? (:prev-effect acquire)))
                        (chain> (succeed> val) acquire)
                        acquire)]
          [acq-eff nil context (conj stack (->AcquireResourceFrame use release context))])
        (if (nil? val)
          [nil nil context stack]
          (if (failure? val)
            [nil val context stack]
            (let [use-res (try
                            (if (fn? use) (use val) use)
                            (catch #?(:clj Throwable :cljs :default) e
                              (try (if (fn? release) (release val) release)
                                   (catch #?(:clj Throwable :cljs :default) _ nil))
                              (throw e)))]
              (if (effect? use-res)
                (let [eff (if (nil? (:prev-effect use-res)) (chain> (succeed> val) use-res) use-res)]
                  [eff nil context (conj stack (->AcquireReleaseUseFrame val release context))])
                (let [rel-res (try
                                (if (fn? release) (release val) release)
                                (catch #?(:clj Throwable :cljs :default) e
                                  (make-failure :release-error e)))]
                  (if (effect? rel-res)
                    [rel-res nil context (conj stack (->AcquireReleaseFinishFrame use-res))]
                    (cond
                      (failure? use-res) [nil use-res context stack]
                      (failure? rel-res) [nil rel-res context stack]
                      :else [nil use-res context stack])))))))))))

(defrecord DieEffect [tag prev-effect data err msg die-data]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (cond
        (some? msg)
        (throw (ex-info msg (or die-data {})))

        (instance? #?(:clj Throwable :cljs js/Error) err)
        (throw err)

        (string? err)
        (throw (ex-info err {:defect err}))

        (map? err)
        (throw (ex-info "Effect defect encountered" err))

        :else
        (throw (ex-info "Effect defect encountered" {:defect err}))))))

(defrecord OrDieEffect [tag prev-effect data msg-or-fn]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        (let [val-map (failure->value val)
              err-payload (error-data val)]
          (cond
            (fn? msg-or-fn)
            (let [res (msg-or-fn val)]
              (if (instance? #?(:clj Throwable :cljs js/Error) res)
                (throw res)
                (throw (ex-info (if (string? res) res "Effect terminated with fatal defect")
                                (if (map? res) res val-map)))))

            (string? msg-or-fn)
            (throw (ex-info msg-or-fn val-map))

            (instance? #?(:clj Throwable :cljs js/Error) err-payload)
            (throw (ex-info "Effect terminated with fatal defect" val-map err-payload))

            :else
            (throw (ex-info "Effect terminated with fatal defect" val-map))))
        [nil val context stack]))))

(defrecord MatchEffect [tag prev-effect data on-failure on-success]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        (let [res (if (fn? on-failure) (on-failure val) on-failure)]
          (if (effect? res)
            [res nil context stack]
            [nil res context stack]))
        (let [res (if (fn? on-success) (on-success val) on-success)]
          (if (effect? res)
            [res nil context stack]
            [nil res context stack]))))))

(defrecord OrElseEffect [tag prev-effect data fallback]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        (let [res (if (fn? fallback) (fallback val) fallback)]
          (if (effect? res)
            [(if (nil? (:prev-effect res)) (chain> (succeed> val) res) res) nil context stack]
            [nil res context stack]))
        [nil val context stack]))))

(defrecord OrElseFailEffect [tag prev-effect data fail-tag error-data fallback-failure]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        (if (some? fallback-failure)
          [nil (if (failure? fallback-failure) fallback-failure (make-failure :or-else-fail fallback-failure)) context stack]
          (let [res-data (if (fn? error-data) (error-data val) error-data)]
            [nil (make-failure fail-tag res-data) context stack]))
        [nil val context stack]))))

(defrecord RetryEffect [tag prev-effect data target policy]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (if (and (some? val) (effect? target) (nil? (:prev-effect target)))
                  (chain> (succeed> val) target)
                  (or target (succeed> val)))]
        [eff nil context (conj stack (->RetryFrame eff val (or policy {}) 1 (long (get policy :delay-ms 0))))]))))

(defrecord TryEffect [tag prev-effect data body catch]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (let [eff (if (nil? (:prev-effect body))
                    (chain> (succeed> val) body)
                    body)]
          [eff nil context (conj stack (->TryFrame catch context))])))))

(defrecord TryFnEffect [tag prev-effect data f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      [nil (f) context stack])))

(defrecord AllEffect [tag prev-effect data effects]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (empty? effects)
        [nil [] context stack]
        (let [[eff1 & rest-effs] effects]
          [eff1 nil context (conj stack (->AllFrame rest-effs []))])))))

(defrecord ForEachEffect [tag prev-effect data coll f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (let [items (or coll val [])]
          (if (empty? items)
            [nil [] context stack]
            (let [[item1 & rest-items] items
                next-eff (f item1)]
              (cond
                (effect? next-eff)
                [next-eff nil context (conj stack (->ForEachFrame f rest-items []))]

                (failure? next-eff)
                [nil next-eff context stack]

                :else
                (loop [acc [next-eff]
                       rem-items rest-items]
                  (if (empty? rem-items)
                    [nil acc context stack]
                    (let [res (f (first rem-items))]
                      (if (failure? res)
                        [nil res context stack]
                        (recur (conj acc res) (rest rem-items))))))))))))))

(defrecord ZipWithEffect [tag prev-effect data eff-a eff-b f]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (let [a (if (and (some? val) (effect? eff-a) (nil? (:prev-effect eff-a)))
                  (chain> (succeed> val) eff-a)
                  eff-a)]
          (if (effect? a)
            [a nil context (conj stack (->ZipWithAFrame eff-b val f))]
            (let [b (if (and (some? val) (effect? eff-b) (nil? (:prev-effect eff-b)))
                      (chain> (succeed> val) eff-b)
                      eff-b)]
              (if (effect? b)
                [b nil context (conj stack (->ZipWithBFrame a f))]
                [nil (f a b) context stack]))))))))

(defrecord MapcatEffect [tag prev-effect data inner-effect]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (if (effect? inner-effect)
          [(chain> (succeed> val) inner-effect) nil context stack]
          (if (fn? inner-effect)
            (let [res (inner-effect val)]
              (if (effect? res)
                [(chain> (succeed> val) res) nil context stack]
                [nil (make-failure :mapcat>-result-not-an-effect {:result res}) context stack]))
            [nil (make-failure :mapcat>-result-not-an-effect {:result inner-effect}) context stack]))))))

(defrecord IfEffect [tag prev-effect data cond then else]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (let [cond-eff (if (nil? (:prev-effect cond))
                         (chain> (succeed> val) cond)
                         cond)]
          [cond-eff nil context (conj stack (->IfFrame val then else))])))))

(defrecord CondEffect [tag prev-effect data conditions]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (if (empty? conditions)
          [nil (make-failure :cond :no-conditions) context stack]
          (let [[[test-eff expr-eff] & rest-conds] conditions
                t-eff (if (nil? (:prev-effect test-eff))
                        (chain> (succeed> val) test-eff)
                        test-eff)]
            [t-eff nil context (conj stack (->CondFrame val expr-eff rest-conds))]))))))

(defrecord CatchEffect [tag prev-effect data handlers]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        (let [val-tag (:tag val)
              err-payload (error-data val)
              f-effect (get handlers val-tag)]
          (if f-effect
            [(chain> (succeed> err-payload) f-effect) nil context stack]
            [nil val context stack]))
        [nil val context stack]))))

(defrecord CatchallEffect [tag prev-effect data inner-effect]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        (let [eff (chain> (succeed> (failure->value val)) inner-effect)]
          [eff nil context stack])
        [nil val context stack]))))

(defrecord SleepEffect [tag prev-effect data ms]
  ITagged
  (tag [_] tag)
  IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (failure? val)
        [nil val context stack]
        (do
          #?(:clj  (when (pos? ms) (Thread/sleep ms))
             :cljs nil)
          [nil val context stack])))))

;; ---------------------------------------------------------------------------
;; Effect Constructor / Factory Functions
;; ---------------------------------------------------------------------------

(defn make-effect
  "Creates a new generic Effect record given a `tag` keyword, an upstream `prev-effect` (or nil),
   and a transparent `data` map."
  [tag prev-effect data]
  (Effect. tag prev-effect (or data {})))

(defn succeed>
  "Creates a successful effect yielding `value`.

   Example:
     (fx/succeed> 42)"
  [value]
  (->SucceedEffect :succeed nil {:value value} value))

(defn fail>
  "Creates a failed effect. Accepts either an existing IFailure instance, or a `tag` keyword
   and an `error-data` payload.

   Examples:
     (fx/fail> (fx/make-failure :not-found {:id 10}))
     (fx/fail> :not-found {:id 10})"
  ([failure]
   (->FailEffect :fail nil {:failure failure} failure))
  ([tag error-data]
   (let [f (make-failure tag error-data)]
     (->FailEffect :fail nil {:failure f} f))))

(defn map>
  "Maps a pure function `f` over the successful value of an effect.
   Short-circuits if the upstream effect yielded a failure.

   Supports point-free pipeline usage:
     (-> (fx/succeed> 10)
         (fx/map> inc))"
  ([f]
   (map> nil f))
  ([prev-effect f]
   (->MapEffect :map prev-effect {:f f} f)))

(defn map-ctx>
  "Maps a binary function `(f value context)` over the successful value and active context.
   Short-circuits if the upstream effect yielded a failure.

   Supports point-free pipeline usage:
     (-> (fx/succeed> {:amount 100})
         (fx/map-ctx> (fn [order {:keys [tax-rate]}]
                        (assoc order :total (* (:amount order) (inc (or tax-rate 0)))))))"
  ([f]
   (map-ctx> nil f))
  ([prev-effect f]
   (->MapCtxEffect :map-ctx prev-effect {:f f} f)))

(defn tap>
  "Executes a side-effecting function `f` on the successful value (e.g. logging or metrics)
   and propagates the original value unchanged to the next effect in the pipeline.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/tap> #(println \"Current value:\" %))
         (fx/map> inc))"
  ([f]
   (tap> nil f))
  ([prev-effect f]
   (->TapEffect :tap prev-effect {:f f} f)))

(defn tap-error>
  "Executes a side-effecting function `f` on the failure (e.g. logging or metrics)
   when a failure occurs, and propagates the original failure unchanged to the next
   effect in the pipeline. Short-circuits (does not run `f`) if the upstream effect succeeded.

   Example:
     (-> (fx/fail> :not-found {:id 10})
         (fx/tap-error> (fn [err] (println \"Failed with tag:\" (fx/tag err))))
         (fx/catch> ...))"
  ([f]
   (tap-error> nil f))
  ([prev-effect f]
   (->TapErrorEffect :tap-error prev-effect {:f f} f)))

(defn do-ctx>
  "Executes a side-effecting binary function `(f value context)` on the successful value and
   active context, and propagates the original value unchanged.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/do-ctx> (fn [v {:keys [logger]}] (when logger (logger v))))
         (fx/map> inc))"
  ([f]
   (do-ctx> nil f))
  ([prev-effect f]
   (->DoCtxEffect :do-ctx prev-effect {:f f} f)))

(defn context>
  "Creates an effect yielding the active execution context map or a value at `key`.

   Examples:
     (fx/context>) ;; yields entire context map
     (fx/context> :db) ;; yields (:db context)
     (fx/context> :db default-db) ;; yields default-db if :db is absent in context"
  ([]
   (->ContextEffect :context nil {} nil nil))
  ([key]
   (->ContextEffect :context nil {:key key} key nil))
  ([key default-val]
   (->ContextEffect :context nil {:key key :default default-val} key default-val)))

(defn service>
  "Creates an effect extracting service `key` from the active execution context.

   Examples:
     (fx/service> :db)
     (fx/service> :timeout 5000)"
  ([key]
   (context> key))
  ([key default-val]
   (context> key default-val)))

(defn provide>
  "Executes `effect` within a dynamic context merged with `context-map`.

   Supports point-free pipeline threading and standalone wrapping:
     (-> (fx/service> :db)
         (fx/provide> {:db mock-db}))

     (fx/provide> {:db mock-db} (fx/service> :db))

     (-> (fx/succeed> 10)
         (fx/provide> (fx/map-ctx> (fn [v ctx] (* v (:multiplier ctx)))) {:multiplier 4}))"
  ([context-map]
   (provide> nil context-map))
  ([a b]
   (let [[target-effect context-map] (cond
                                       (effect? a) [a b]
                                       (effect? b) [b a]
                                       (and (map? a) (not (effect? a))) [b a]
                                       :else [a b])]
     (->ProvideEffect :provide nil {:body target-effect :context-map context-map} target-effect context-map)))
  ([prev-effect body-effect context-map]
   (->ProvideEffect :provide prev-effect {:body body-effect :context-map context-map} body-effect context-map)))

(defn provide-service>
  "Executes `effect` within a dynamic context where `key` is bound to `service-impl`.

   Supports point-free pipeline threading and standalone wrapping:
     (-> (fx/service> :db)
         (fx/provide-service> :db mock-db))

     (fx/provide-service> :db mock-db (fx/service> :db))"
  ([key service-impl]
   (provide> {key service-impl}))
  ([a b c]
   (if (keyword? a)
     (provide> c {a b})
     (provide> a {b c}))))

(defn ensure>
  "Executes a `finalizer-effect` guaranteed after the previous effect completes,
   regardless of whether the previous effect succeeded or failed.

   If the upstream effect succeeds, the finalizer runs and the original successful
   value is returned (unless the finalizer itself fails).
   If the upstream effect fails, the finalizer runs and the original failure
   is returned (unless the finalizer itself fails).

   Supports point-free pipeline usage:
     (-> (fx/succeed> 10)
         (fx/ensure> (fx/tap> (fn [_] (println \"Cleanup\")))))

     (-> (fx/fail> :not-found {:id 42})
         (fx/ensure> (fx/tap> (fn [_] (println \"Cleanup\")))))"
  ([finalizer-effect]
   (ensure> nil finalizer-effect))
  ([prev-effect finalizer-effect]
   (->EnsureEffect :ensure prev-effect {:finalizer finalizer-effect} finalizer-effect)))

(defn acquire-release>
  "Acquires a resource, runs `use-eff-fn`, and guarantees `release-eff-fn` executes
   if acquisition succeeded, regardless of whether usage succeeded, failed, or threw an exception.

   Supports standalone execution and point-free pipeline threading:
     (fx/acquire-release>
       (open-db-connection> config)
       (fn [conn] (run-queries> conn))
       (fn [conn] (close-conn> conn)))

     (-> (open-db-connection> config)
         (fx/acquire-release>
           (fn [conn] (run-queries> conn))
           (fn [conn] (close-conn> conn))))"
  ([use-eff-fn release-eff-fn]
   (->AcquireReleaseEffect :acquire-release nil {:acquire nil :use use-eff-fn :release release-eff-fn} nil use-eff-fn release-eff-fn))
  ([acquire-eff use-eff-fn release-eff-fn]
   (->AcquireReleaseEffect :acquire-release nil {:acquire acquire-eff :use use-eff-fn :release release-eff-fn} acquire-eff use-eff-fn release-eff-fn))
  ([prev-effect acquire-eff use-eff-fn release-eff-fn]
   (let [acq (if (effect? acquire-eff)
               (chain> prev-effect acquire-eff)
               acquire-eff)]
     (->AcquireReleaseEffect :acquire-release nil {:acquire acq :use use-eff-fn :release release-eff-fn} acq use-eff-fn release-eff-fn))))

(defn die>
  "Yields an effect that halts execution by throwing a fatal defect exception."
  ([]
   (die> "Effect defect encountered"))
  ([err]
   (->DieEffect :die nil {:err err} err nil nil))
  ([msg data]
   (->DieEffect :die nil {:msg msg :data data} nil msg data)))

(defn or-die>
  "Converts any upstream domain failure into a fatal defect exception.
   If upstream succeeded, propagates value unmodified.

   Supports point-free pipeline usage and custom error messages or formatters:
     (-> (fx/fail> :not-found {:id 42})
         (fx/or-die>))

     (-> (fx/fail> :not-found {:id 42})
         (fx/or-die> \"Failed to load entity\"))"
  ([]
   (or-die> nil))
  ([prev-effect]
   (or-die> prev-effect nil))
  ([prev-effect msg-or-fn]
   (->OrDieEffect :or-die prev-effect {:msg-or-fn msg-or-fn} msg-or-fn)))

(defn match>
  "Converges failure and success channels by applying `on-failure` or `on-success`.
   `on-failure` receives the Failure record and `on-success` receives the success value.
   Both can be functions returning values or effects, or standalone effects.

   Supports point-free pipeline usage:
     (-> (fetch-user> id)
         (fx/match>
           (fn [err] (render-error-view err))
           (fn [user] (render-profile-view user))))"
  ([on-failure on-success]
   (match> nil on-failure on-success))
  ([prev-effect on-failure on-success]
   (->MatchEffect :match prev-effect {:on-failure on-failure :on-success on-success} on-failure on-success)))

(defn or-else>
  "Executes `fallback-eff` if upstream evaluates to a failure.
   Propagates success value unmodified.

   Supports point-free pipeline usage:
     (-> (fetch-from-cache> id)
         (fx/or-else> (fetch-from-remote-db> id)))"
  ([fallback-eff]
   (or-else> nil fallback-eff))
  ([prev-effect fallback-eff]
   (->OrElseEffect :or-else prev-effect {:fallback fallback-eff} fallback-eff)))

(defn or-else-fail>
  "Replaces any upstream failure with a new Failure having `tag` and `error-data`
   (or the provided failure / function result). Propagates success value unmodified.

   Example:
     (-> (fetch-user> id)
         (fx/or-else-fail> :user-not-found {:id id}))"
  ([fallback-failure]
   (or-else-fail> nil fallback-failure))
  ([a b]
   (if (keyword? a)
     (or-else-fail> nil a b)
     (->OrElseFailEffect :or-else-fail a {:fallback-failure b} nil nil b)))
  ([prev-effect tag error-data]
   (->OrElseFailEffect :or-else-fail prev-effect {:tag tag :error-data error-data} tag error-data nil)))

(defn retry>
  "Retries an effect according to policy `{:max-attempts n :delay-ms ms :backoff-factor f :retry-if pred}`.

   Supports standalone execution and pipeline usage:
     (fx/retry> (flaky-effect) {:max-attempts 3 :delay-ms 100})
     (-> (flaky-effect) (fx/retry> {:max-attempts 3 :delay-ms 100 :backoff-factor 2.0}))"
  ([policy]
   (retry> nil nil policy))
  ([target-or-policy policy-or-nil]
   (if (and (map? target-or-policy) (not (effect? target-or-policy)) (nil? policy-or-nil))
     (retry> nil nil target-or-policy)
     (->RetryEffect :retry nil {:target target-or-policy :policy (or policy-or-nil {})} target-or-policy (or policy-or-nil {}))))
  ([prev-effect target-effect policy]
   (->RetryEffect :retry prev-effect {:target target-effect :policy (or policy {})} target-effect (or policy {}))))

(defn try>
  "Creates an effect that executes an inner effect combinator, catching any thrown
   exceptions (Throwable on Clojure, :default on ClojureScript) and converting them into typed failures.

   Supports point-free pipeline usage, standalone evaluation, custom keyword failure types,
   custom catch effect/function handlers, and options maps:
     (-> (succeed> \"10\") (try> (map> parse-long)))
     (-> (succeed> \"abc\") (try> (map> parse-long) :parse-error))
     (-> (succeed> \"abc\") (try> (map> parse-long) (map> (fn [e] (make-failure :parse-error (.getMessage e))))))
     (try> (map> #(slurp \"file.txt\")))
     (try> (map> #(slurp \"file.txt\")) :io-error)
     (try> {:try (map> #(slurp \"file.txt\")) :catch :io-error})"
  ([effect-or-opts]
   (if (and (map? effect-or-opts) (contains? effect-or-opts :try))
     (try> nil (:try effect-or-opts) (:catch effect-or-opts))
     (try> nil effect-or-opts nil)))
  ([a b]
   (cond
     (and (effect? a) (effect? b))
     (try> a b nil)

     (and (effect? a) (map? b) (contains? b :try))
     (try> a (:try b) (:catch b))

     (effect? a)
     (try> nil a b)

     :else
     (try> nil a b)))
  ([prev-effect body-effect catch-handler]
   (let [eff (if (effect? body-effect)
               body-effect
               (if (fn? body-effect)
                 (->TryFnEffect :try-fn nil {:f body-effect} body-effect)
                 body-effect))]
     (->TryEffect :try prev-effect {:body eff :catch catch-handler} eff catch-handler))))

(defn all>
  "Combines a vector or sequence of independent effects into a single effect that returns
   a vector of their evaluated results.

   Example:
     (fx/all> [(fx/succeed> 1)
               (fx/succeed> 2)])
     ;; => [1 2]"
  [effects]
  (->AllEffect :all nil {:effects (vec effects)} (vec effects)))

(defn for-each>
  "Maps a collection through an effect-producing function `f-eff` and collects results into a vector.
   Short-circuits on the first failure.

   Supports standalone execution and point-free pipeline threading:
     (fx/for-each> [1 2 3] (fn [id] (fetch-user-by-id> id)))
     (-> (fx/succeed> [1 2 3])
         (fx/for-each> (fn [id] (fetch-user-by-id> id))))"
  ([f-eff]
   (for-each> nil f-eff))
  ([a b]
   (if (and (or (sequential? a) (set? a) (nil? a)) (not (effect? a)))
     (->ForEachEffect :for-each nil {:coll (vec a) :f b} (vec a) b)
     (->ForEachEffect :for-each a {:coll nil :f b} nil b)))
  ([prev-effect coll f-eff]
   (->ForEachEffect :for-each prev-effect {:coll (when (some? coll) (vec coll)) :f f-eff} (when (some? coll) (vec coll)) f-eff)))

(defn zip-with>
  "Evaluates two effects and combines their successful results using binary function `f`.
   Short-circuits if either effect fails.

   Example:
     (fx/zip-with> (fetch-user> id) (fetch-perms> id)
                   (fn [u p] (assoc u :permissions p)))"
  ([eff-a eff-b f]
   (->ZipWithEffect :zip-with nil {:eff-a eff-a :eff-b eff-b :f f} eff-a eff-b f))
  ([prev-effect eff-a eff-b f]
   (->ZipWithEffect :zip-with prev-effect {:eff-a eff-a :eff-b eff-b :f f} eff-a eff-b f)))

(defn zip>
  "Evaluates two effects and pairs their results into a 2-element vector `[res-a res-b]`.
   Short-circuits if either effect fails.

   Example:
     (fx/zip> (fetch-user> id) (fetch-profile> id))"
  ([eff-a eff-b]
   (zip-with> eff-a eff-b vector))
  ([prev-effect eff-a eff-b]
   (zip-with> prev-effect eff-a eff-b vector)))

(defn mapcat>
  "Flat-maps over an effect by applying an inner effect combinator to the successful value.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/mapcat> (fx/map> inc)))"
  ([inner-effect]
   (mapcat> nil inner-effect))
  ([prev-effect inner-effect]
   (->MapcatEffect :mapcat prev-effect {:inner-effect inner-effect} inner-effect)))

(defn if>
  "Conditional branching combinator. Evaluates `cond-eff` with the current value:
   if truthy, evaluates `then-effect`; otherwise evaluates `else-effect`.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/if> (fx/map> even?)
                 (fx/map> inc)
                 (fx/map> dec)))"
  ([cond-eff then-effect else-effect]
   (if> nil cond-eff then-effect else-effect))
  ([prev-effect cond-eff then-effect else-effect]
   (->IfEffect :if prev-effect {:cond cond-eff :then then-effect :else else-effect} cond-eff then-effect else-effect)))

(defn cond>
  "Multi-branch conditional combinator. Takes paired test and expression effects:
   `[prev-effect test-1 expr-1 test-2 expr-2 ...]`.
   Evaluates tests in order with the current value; when a test evaluates to a truthy
   value, executes the corresponding expression effect.

   If no test matches, returns a Failure with tag `:cond` and error-data `:no-conditions`.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/cond>
           (fx/map> odd?)  (fx/map> inc)
           (fx/map> even?) (fx/map> dec)))"
  [prev-effect & conditions]
  (let [pairs (vec (partition 2 conditions))]
    (->CondEffect :cond prev-effect {:conditions pairs} pairs)))

(defn catch>
  "Catches specific failure tags using a handler map `f-map` of `{failure-tag handler-effect}`.
   When a failure matches a key in `f-map`, executes the corresponding handler effect passing the
   failure's `error-data` as input. Unmatched failures and successful values pass through untouched.

   Example:
     (-> (fx/fail> :not-found {:id 42})
         (fx/catch> {:not-found (fx/map> (fn [{:keys [id]}] (str \"User \" id \" missing\")))}))"
  ([f-map]
   (catch> nil f-map))
  ([prev-effect f-map]
   (->CatchEffect :catch prev-effect {:handlers f-map} f-map)))

(defn catchall>
  "Catches any failure and routes its map representation `{:tag ... :error-data ...}` as input
   to `inner-effect`. Successful values pass through untouched.

   Example:
     (-> (fx/fail> :error {:code 500})
         (fx/catchall> (fx/map> (fn [{:keys [tag error-data]}] (str \"Caught: \" tag))))))"
  ([inner-effect]
   (catchall> nil inner-effect))
  ([prev-effect inner-effect]
   (->CatchallEffect :catch-all prev-effect {:inner-effect inner-effect} inner-effect)))

(defn sleep>
  "Creates an effect that pauses evaluation for `ms` milliseconds and propagates upstream value."
  ([ms]
   (sleep> nil ms))
  ([prev-effect ms]
   (->SleepEffect :sleep prev-effect {:ms ms} ms)))

;; ---------------------------------------------------------------------------
;; Exception Unwinding Helper
;; ---------------------------------------------------------------------------

(defn- unwind-for-exception [stack e]
  (loop [s stack]
    (if (empty? s)
      nil
      (let [[frame & rest-s] s]
        (if #?(:clj  (instance? com.lambdaseq.fx.core.IUnwindable frame)
               :cljs (satisfies? IUnwindable frame))
          (if-let [res (-unwind frame e rest-s)]
            res
            (recur rest-s))
          (recur rest-s))))))

;; ---------------------------------------------------------------------------
;; Runtime Evaluation Engine
;; ---------------------------------------------------------------------------

(defn run-sync!
  "Synchronously evaluates an effect pipeline from root to leaf and returns the final value
   or failure. Binds execution with optional initial `context` map."
  ([effect]
   (run-sync! effect {}))
  ([root-effect initial-context]
   (let [ctx (assoc (or initial-context {}) :runner run-sync!)]
     (loop [curr-eff root-effect
            curr-val nil
            curr-ctx ctx
            stack    ()]
       (if (nil? curr-eff)
         (if (empty? stack)
           curr-val
           (let [[frame & rest-stack] stack
                 [next-eff next-val next-ctx next-stack]
                 (try
                   (-resume frame curr-val curr-ctx rest-stack)
                   (catch #?(:clj Throwable :cljs :default) e
                     (if-let [unwind-info (unwind-for-exception stack e)]
                       (if-let [eff (:effect unwind-info)]
                         [eff nil (:context unwind-info) (:rest-stack unwind-info)]
                         [nil (:value unwind-info) (:context unwind-info) (:rest-stack unwind-info)])
                       (throw e))))]
             (recur next-eff next-val next-ctx next-stack)))
         (let [[next-eff next-val next-ctx next-stack]
               (try
                 (-step curr-eff curr-val curr-ctx stack)
                 (catch #?(:clj Throwable :cljs :default) e
                   (if-let [unwind-info (unwind-for-exception stack e)]
                     (if-let [eff (:effect unwind-info)]
                       [eff nil (:context unwind-info) (:rest-stack unwind-info)]
                       [nil (:value unwind-info) (:context unwind-info) (:rest-stack unwind-info)])
                     (throw e))))]
           (recur next-eff next-val next-ctx next-stack)))))))

(defn run-async!
  "Asynchronously evaluates an effect pipeline returning a java.util.concurrent.CompletableFuture (Clojure)
   or a js/Promise (ClojureScript).

   Examples:
     @(fx/run-async! (fx/succeed> 42))
     ;; => 42"
  ([effect]
   (run-async! effect {}))
  ([effect context]
   #?(:clj
      (CompletableFuture/supplyAsync
        (reify Supplier
          (get [_]
            (run-sync! effect context))))
      :cljs
      (js/Promise.
        (fn [resolve reject]
          (try
            (resolve (run-sync! effect context))
            (catch :default e
              (reject e))))))))
