(ns com.lambdaseq.fx.core)

(defprotocol IEffect
  (-next-effect [this]
    "Returns the next effect in the chain.")
  (-eval! [this v]
    "Evaluates the run function of the effect and returns the result."))

(defrecord Effect [effect-type next-effect run]
  IEffect
  (-next-effect [_] next-effect)
  (-eval! [_ v] (run v)))

(defprotocol IFailure)

(defrecord Failure
  [type data]
  IFailure)

(defn effect?
  "Returns true if the value is an effect."
  [x]
  (instance? Effect x))

(defn failure?
  "Returns true if the value is a failure."
  [x]
  (instance? Failure x))

(defn make-effect [type next-effect run]
  (Effect. type next-effect run))

(defn make-failure [type err]
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
  (make-effect :succeed nil (constantly value)))

(defn fail>
  "Creates a failed effect. Takes a failure or a type and an error to be wrapped in a failure,
   and returns a new effect."
  ([failure]
   (make-effect :fail nil (constantly failure)))
  ([type error]
   (make-effect :fail nil (constantly (make-failure type error)))))

(defn map>
  "Maps over the effect. Takes a function f and/or an effect,
   and returns a new effect."
  [f next-effect]
  (make-effect :map
    next-effect
    (fn [value]
      (maybe-propagate-failure value
        (f value)))))

(defn do>
  "Runs the effect and propagates the value to the next effect."
  [f next-effect]
  (make-effect :do
    next-effect
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
  [f next-effect]
  (make-effect :mapcat
    next-effect
    (fn [value]
      (maybe-propagate-failure value
        (let [res (f value)]
          (if (effect? res)
            ; Maybe should eval with nil here?
            (-eval! res value)
            (make-failure :mapcat>-result-not-an-effect
                          {:result res})))))))

(defn if>
  "If the condition is true,
     then returns the `then` effect,
     otherwise returns the `else` effect."
  [cond then> else> next-effect]
  (make-effect :if
    next-effect
    (fn [value]
      (maybe-propagate-failure value
        (-eval! (if (cond value)
                  (then> value)
                  (else> value))
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
      (fn [v]
        (maybe-propagate-failure v
          (-eval! (loop [conditions conditions]
                    (if-let [[test expr>] (first conditions)]
                      (if (test v)
                        (expr> v)
                        (recur (rest conditions)))
                      (fail> :cond :no-conditions)))
                  v))))))

(defn catch>
  "Dispatches the failure to the provided handler based on the failure type.
   The functions should receive the failure `data` and return an effect."
  [f-map next-effect]
  (make-effect :catch
    next-effect
    (fn [value]
      (if (failure? value)
        (let [{:keys [type data]} value
              f (get f-map type)]
          (if f
            ; maybe it should be (f value)
            (-eval! (f data) value)
            value))
        value))))

(defn catch-all>
  "Catches all failures and runs the provided function.
  The function should receive the failure and return an effect."
  [f next-effect]
  (make-effect :catch-all
    next-effect
    (fn [value]
      (if (failure? value)
        (-eval! (f value) value)
        value))))

(defmacro pipeline>>
  "Creates a pipeline of effects."
  [& body]
  `(fn [effect#]
     (->> effect#
          ~@body)))

(defn run-sync!
  "Evaluates the effect and returns the result."
  [effect]
  (->> effect
       (iterate -next-effect)
       (take-while some?)
       (reverse)
       (reduce (fn [acc effect]
                 (-eval! effect acc))
               nil)))