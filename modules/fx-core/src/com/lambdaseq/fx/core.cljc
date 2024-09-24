(ns com.lambdaseq.fx.core)

(defprotocol IEffect
  (-eval! [this v]
    "Evaluates the run function of the effect and returns the result.")
  (effect-type [this]
    "Returns the type of the effect.")
  (prev-effect [this]
    "Returns the next effect in the chain."))

(defrecord Effect [effect-type prev-effect run]
  IEffect
  (-eval! [_ v] (run v))
  (prev-effect [_] prev-effect)
  (effect-type [_] effect-type))

(defprotocol IFailure
  (failure-type [this]
    "Returns the type of the failure.")
  (error-data [this]
    "Returns the error data of the failure."))

(defrecord Failure
  [type error-data]
  IFailure
  (failure-type [_] type)
  (error-data [_] error-data))

(def ^:dynamic *runner*)

(def ^:dynamic *input*)

(defn run-sync!
  "Evaluates the effect and returns the result."
  [effect]
  (binding [*runner* run-sync!]
    (->> effect
         (iterate prev-effect)
         (take-while some?)
         (reverse)
         (reduce (fn [acc effect]
                   (-eval! effect acc))
                 nil))))

(defn- -run!
  "Helper function for running using the current runner"
  ([effect]
   (*runner* effect))
  ([effect input]
   (binding [*input* input]
     (*runner* effect))))

(defn effect?
  "Returns true if the value is an effect."
  [x]
  (instance? Effect x))

(defn failure?
  "Returns true if the value is a failure."
  [x]
  (instance? Failure x))

(defn make-effect
  "Given a type keyword, a next effect, and a run function, returns a new effect."
  [type prev-effect run]
  (Effect. type prev-effect run))

(defn make-failure
  "Given a type keyword and an error map, returns a new failure."
  [type err]
  (Failure. type err))

(defmacro maybe-propagate-failure
  "If the value is an effect, then returns it, otherwise evaluates the body.
  Useful for propagating failures in effects that handle only values."
  [v & body]
  `(if (failure? ~v)
     ~v
     (do ~@body)))

(defmacro maybe-propagate-effect
  "If the value is an effect, then returns it, otherwise evaluates the body.
  Useful for propagating effects, in effects that handle only failures."
  [v & body]
  `(if (effect? ~v)
     ~v
     (do ~@body)))

(defn succeed> [value]
  "Creates a successful effect that just returns the value."
  (make-effect :succeed nil (fn [_] value)))

(defn input>
  "Creates an effect that returns the value of *input* when evaluated
  Useful for higher-order effects (conditional effects, mapcat etc.)"
  []
  (make-effect :input nil (fn [_] *input*)))

(defn fail>
  "Creates a failed effect. Takes a failure or a type and an error to be wrapped in a failure,
   and returns a new effect."
  ([failure]
   (make-effect :fail nil (constantly failure)))
  ([type error-data]
   (make-effect :fail nil (constantly (make-failure type error-data)))))

(defn map>
  "Maps over the effect. Takes a function f and/or an effect,
   and returns a new effect."
  [f prev-effect]
  (make-effect :map
    prev-effect
    (fn [value]
      (maybe-propagate-failure value
        (f value)))))

(defn do>
  "Runs the effect and propagates the value to the next effect.
  Useful for running side effects, and making sure the input is passed to the next effect,
  like logging, or updating a database."
  [f prev-effect]
  (make-effect :do
    prev-effect
    (fn [value]
      (maybe-propagate-failure value
        (do
          (f value)
          value)))))

(defn all>
  "Combines the effects into a single effect that returns a vector of the results of the effects."
  [effects]
  (make-effect
    :all
    nil
    (constantly
      (->> effects
           (mapv (fn [eff] (-eval! eff nil)))))))

(defn mapcat>
  "Flat maps over the effect. Takes a function f that returns an effect and/or an effect,
   and returns a new effect."
  [inner-effect prev-effect]
  (make-effect :mapcat
    prev-effect
    (fn [value]
      (maybe-propagate-failure value
        (let [res (-eval! inner-effect value)]
          (if (effect? res)
            ; Maybe should eval with nil here?
            (-run! res value)
            (make-failure :mapcat>-result-not-an-effect
                          {:result res})))))))

(defn if>
  "If the condition is true,
     then returns the `then` effect,
     otherwise returns the `else` effect."
  [cond then-effect else-effect prev-effect]
  (make-effect :if
    prev-effect
    (fn [value]
      (maybe-propagate-failure value
        (-run! (if (cond value)
                 then-effect
                 else-effect)
               value)))))

(defn cond>
  "Evaluates the conditions in order until one of them returns true,
   then returns the effect associated with that condition.
   Conditions are test and expr pairs. tests are functions that take the result of the previous effect,
   and exprs are functions that take the result of the previous effect and return effects.

   If no conditions are met, then returns a failure."
  [& conditions]
  (let [effect (last conditions)
        conditions (->> conditions
                        (butlast)
                        (partition 2))]
    (make-effect :cond
      effect
      (fn [value]
        (maybe-propagate-failure value
          (-run! (loop [conditions conditions]
                   (if-let [[test expr-effect] (first conditions)]
                     (if (test value)
                       expr-effect
                       (recur (rest conditions)))
                     (fail> :cond :no-conditions)))
                 value))))))

(defn catch>
  "Dispatches the failure to the provided handler based on the failure type.
   The functions should receive the failure `data` and return an effect."
  [f-map prev-effect]
  (make-effect :catch
    prev-effect
    (fn [value]
      (if (failure? value)
        (let [failure-type (failure-type value)
              error-data (error-data value)
              f-effect (get f-map failure-type)]
          (if f-effect
            ; maybe it should be (f value)
            (-run! f-effect error-data)
            value))
        value))))

(defn failure->value
  "Converts a IFailure object to a plain map"
  [failure]
  {:type (failure-type failure)
   :error-data (error-data failure)})

(defn catchall>
  "Catches all failures and runs the provided function.
  The function should receive the failure and return an effect."
  [inner-effect prev-effect]
  (make-effect :catch-all
    prev-effect
    (fn [value]
      (if (failure? value)
        (-run! inner-effect
               (failure->value value))
        value))))