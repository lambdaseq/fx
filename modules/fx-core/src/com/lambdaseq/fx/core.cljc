(ns com.lambdaseq.fx.core)

(defprotocol IEffect
  "Protocol representing an executable effect in a computation pipeline."
  (-eval! [this v]
    "Evaluates the run function of the effect with input value `v` and returns the result.")
  (effect-type [this]
    "Returns the keyword identifying the type of the effect (e.g. :succeed, :fail, :map, :try).")
  (prev-effect [this]
    "Returns the upstream (previous) effect in the chain, or nil if this is the root."))

(defrecord Effect [effect-type prev-effect run]
  IEffect
  (-eval! [_ v] (run v))
  (prev-effect [_] prev-effect)
  (effect-type [_] effect-type))

(defprotocol IFailure
  "Protocol representing a typed failure in the effect system."
  (failure-type [this]
    "Returns the keyword type of the failure.")
  (error-data [this]
    "Returns the error payload/data of the failure."))

(defrecord Failure
  [type error-data]
  IFailure
  (failure-type [_] type)
  (error-data [_] error-data))

(defn chain>
  "Chains `current-effect` onto `prev-effect` by setting `prev-effect` as its upstream dependency."
  [prev-effect current-effect]
  (assoc current-effect :prev-effect prev-effect))

(def ^:dynamic *context*
  "Dynamic map holding execution context and runtime state (e.g. :runner)."
  {})

(defn- -run!
  "Helper function for running an effect using the current runner in *context*."
  [effect]
  (let [{:keys [runner]} *context*]
    (runner effect)))

(defn effect?
  "Returns true if `x` implements IEffect or is an instance of Effect."
  [x]
  (instance? Effect x))

(defn failure?
  "Returns true if `x` implements IFailure or is an instance of Failure."
  [x]
  (instance? Failure x))

(defn make-effect
  "Creates a new Effect record given an `effect-type` keyword, an upstream `prev-effect` (or nil),
   and a single-argument execution function `run`."
  [type prev-effect run]
  (Effect. type prev-effect run))

(defn make-failure
  "Creates a new Failure record given a `type` keyword and an `err` payload map or value."
  [type err]
  (Failure. type err))

(defmacro maybe-propagate-failure
  "If `v` is a failure, short-circuits and returns `v` directly. Otherwise evaluates `body`."
  [v & body]
  `(let [v# ~v]
     (if (failure? v#)
       v#
       (do ~@body))))

(defmacro maybe-propagate-effect
  "If `v` is an effect, returns `v` directly. Otherwise evaluates `body`."
  [v & body]
  `(let [v# ~v]
     (if (effect? v#)
       v#
       (do ~@body))))

(defn succeed>
  "Creates a successful effect yielding `value`.

   Example:
     (fx/succeed> 42)"
  [value]
  (make-effect :succeed nil (fn [_] value)))

(defn fail>
  "Creates a failed effect. Accepts either an existing IFailure instance, or a `type` keyword
   and an `error-data` payload.

   Examples:
     (fx/fail> (fx/make-failure :not-found {:id 10}))
     (fx/fail> :not-found {:id 10})"
  ([failure]
   (make-effect :fail nil (constantly failure)))
  ([type error-data]
   (make-effect :fail nil (constantly (make-failure type error-data)))))

(defn map>
  "Maps a pure function `f` over the successful value of an effect.
   Short-circuits if the upstream effect yielded a failure.

   Supports point-free pipeline usage:
     (-> (fx/succeed> 10)
         (fx/map> inc))"
  ([f]
   (map> nil f))
  ([prev-effect f]
   (make-effect :map
     prev-effect
     (fn [value]
       (maybe-propagate-failure value
         (f value))))))

(defn do>
  "Executes a side-effecting function `f` on the successful value (e.g. logging or metrics)
   and propagates the original value unchanged to the next effect in the pipeline.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/do> #(println \"Current value:\" %))
         (fx/map> inc))"
  ([f]
   (do> nil f))
  ([prev-effect f]
   (make-effect :do
     prev-effect
     (fn [value]
       (maybe-propagate-failure value
         (f value)
         value)))))

(defn ensure>
  "Executes a `finalizer-effect` guaranteed after the previous effect completes,
   regardless of whether the previous effect succeeded or failed.

   If the upstream effect succeeds, the finalizer runs and the original successful
   value is returned (unless the finalizer itself fails).
   If the upstream effect fails, the finalizer runs and the original failure
   is returned (unless the finalizer itself fails).

   Supports point-free pipeline usage:
     (-> (fx/succeed> 10)
         (fx/ensure> (fx/do> (fn [_] (println \"Cleanup\")))))

     (-> (fx/fail> :not-found {:id 42})
         (fx/ensure> (fx/do> (fn [_] (println \"Cleanup\")))))"
  ([finalizer-effect]
   (ensure> nil finalizer-effect))
  ([prev-effect finalizer-effect]
   (let [eff (if (effect? finalizer-effect)
               finalizer-effect
               (do> finalizer-effect))]
     (make-effect :ensure
       prev-effect
       (fn [value]
         (let [fin-input (if (failure? value) nil value)
               fin-eff (if (nil? (:prev-effect eff))
                         (chain> (succeed> fin-input) eff)
                         eff)
               fin-res (try
                         (-run! fin-eff)
                         (catch #?(:clj Throwable :cljs :default) e
                           (make-failure :ensure e)))]
           (cond
             (failure? fin-res) fin-res
             (failure? value)   value
             :else              value)))))))

(defn- handle-try-exception [catch-handler e]
  (cond
    (nil? catch-handler)
    (make-failure :try e)

    (keyword? catch-handler)
    (make-failure catch-handler e)

    (effect? catch-handler)
    (-run! (chain> (succeed> e) catch-handler))

    (ifn? catch-handler)
    (let [res (catch-handler e)]
      (if (failure? res)
        res
        (make-failure :try res)))

    :else
    (make-failure :try e)))

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
   (let [eff (if (effect? body-effect) body-effect (map> body-effect))]
     (make-effect :try
       prev-effect
       (fn [value]
         (maybe-propagate-failure value
           (try
             (-run! (chain> (succeed> value) eff))
             (catch #?(:clj Throwable :cljs :default) e
               (handle-try-exception catch-handler e)))))))))

(defn all>
  "Combines a vector or sequence of independent effects into a single effect that returns
   a vector of their evaluated results.

   Example:
     (fx/all> [(fx/succeed> 1)
               (fx/succeed> 2)])
     ;; => [1 2]"
  [effects]
  (make-effect
    :all
    nil
    (constantly
      (->> effects
           (mapv (fn [eff] (-eval! eff nil)))))))

(defn mapcat>
  "Flat-maps over an effect by applying an inner effect combinator to the successful value.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/mapcat> (fx/map> inc)))"
  ([inner-effect]
   (mapcat> nil inner-effect))
  ([prev-effect inner-effect]
   (make-effect :mapcat
     prev-effect
     (fn [value]
       (maybe-propagate-failure value
         (if (effect? inner-effect)
           ; Maybe should eval with nil here?
           (-run! (chain> (succeed> value)
                          inner-effect))
           (make-failure :mapcat>-result-not-an-effect
                         {:result inner-effect})))))))

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
   (make-effect :if
     prev-effect
     (fn [value]
       (maybe-propagate-failure value
         (let [branch-eff (if (-run! (chain> (succeed> value)
                                             cond-eff))
                            then-effect
                            else-effect)]
           (-run! (chain> (succeed> value)
                          branch-eff))))))))

(defn cond>
  "Multi-branch conditional combinator. Takes paired test and expression effects:
   `[prev-effect test-1 expr-1 test-2 expr-2 ...]`.
   Evaluates tests in order with the current value; when a test evaluates to a truthy
   value, executes the corresponding expression effect.

   If no test matches, returns a Failure with type `:cond` and error-data `:no-conditions`.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/cond>
           (fx/map> odd?)  (fx/map> inc)
           (fx/map> even?) (fx/map> dec)))"
  [prev-effect & conditions]
  (let [conditions (partition 2 conditions)]
    (make-effect :cond
      prev-effect
      (fn [value]
        (maybe-propagate-failure value
          (let [res-eff (loop [conditions conditions]
                          (if-let [[test-eff expr-effect] (first conditions)]
                            (if (-run! (chain> (succeed> value)
                                               test-eff))
                              expr-effect
                              (recur (rest conditions)))
                            (fail> :cond :no-conditions)))]
            (-run! (chain> (succeed> value)
                           res-eff))))))))

(defn catch>
  "Catches specific failure types using a handler map `f-map` of `{failure-type-keyword handler-effect}`.
   When a failure matches a key in `f-map`, executes the corresponding handler effect passing the
   failure's `error-data` as input. Unmatched failures and successful values pass through untouched.

   Example:
     (-> (fx/fail> :not-found {:id 42})
         (fx/catch> {:not-found (fx/map> (fn [{:keys [id]}] (str \"User \" id \" missing\")))}))"
  ([f-map]
   (catch> nil f-map))
  ([prev-effect f-map]
   (make-effect :catch
     prev-effect
     (fn [value]
       (if (failure? value)
         (let [failure-type (failure-type value)
               error-data (error-data value)
               f-effect (get f-map failure-type)]
           (if f-effect
             ; maybe it should be (f value)
             (-run! (chain> (succeed> error-data)
                            f-effect))
             value))
         value)))))

(defn failure->value
  "Converts an IFailure instance into a plain map `{:type ... :error-data ...}`."
  [failure]
  {:type       (failure-type failure)
   :error-data (error-data failure)})

(defn catchall>
  "Catches any failure and routes its map representation `{:type ... :error-data ...}` as input
   to `inner-effect`. Successful values pass through untouched.

   Example:
     (-> (fx/fail> :error {:code 500})
         (fx/catchall> (fx/map> (fn [{:keys [type error-data]}] (str \"Caught: \" type))))))"
  ([inner-effect]
   (catchall> nil inner-effect))
  ([prev-effect inner-effect]
   (make-effect :catch-all
     prev-effect
     (fn [value]
       (if (failure? value)
         (-run! (chain> (succeed> (failure->value value))
                        inner-effect))
         value)))))

(defn run-sync!
  "Synchronously evaluates an effect pipeline from root to leaf and returns the final value
   or failure. Binds `*context*` with `:runner run-sync!` during execution.

   Example:
     (fx/run-sync! (fx/succeed> 42))
     ;; => 42"
  [effect]
  (binding [*context* (assoc *context* :runner run-sync!)]
    (->> effect
         (iterate prev-effect)
         (take-while some?)
         (reverse)
         (reduce (fn [acc effect]
                   (-eval! effect acc))
                 nil))))