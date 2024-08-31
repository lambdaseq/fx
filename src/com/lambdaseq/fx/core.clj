(ns com.lambdaseq.fx.core)

(defprotocol IEffect
  (-eval! [this]
    "Evaluates the effect and returns the result."))

(defrecord Effect [effect-type run]
  IEffect
  (-eval! [_] (run)))

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

(defn make-effect [type run]
  (Effect. type run))

(defn make-failure [type err]
  (Failure. type err))

(defmacro maybe-propagate-failure
  "If the value is an effect, then returns it, otherwise evaluates the body."
  [v & body]
  `(if (failure? ~v)
     ~v
     (do ~@body)))

(defn succeed> [value]
  "Creates a successful effect that just returns the value."
  (make-effect :succeed (constantly value)))

(defn fail>
  "Creates a failed effect. Takes a failure or a type and an error to be wrapped in a failure,
   and returns a new effect."
  ([failure]
   (make-effect :fail (constantly failure)))
  ([type error]
   (make-effect :fail (constantly (make-failure type error)))))

(defn map>
  "Maps over the effect. Takes a function f and/or an effect,
   and returns a new effect."
  [f effect]
  (make-effect :map
    (constantly
      (let [res# (-eval! effect)]
        (maybe-propagate-failure res#
          (f res#))))))

(defn mapcat>
  "Flat maps over the effect. Takes a function f that returns an effect and/or an effect,
   and returns a new effect."
  [f effect]
  (make-effect :mapcat
    (constantly
      (let [res# (-eval! effect)]
        (maybe-propagate-failure res#
          (let [ret# (f res#)]
            (if (effect? ret#)
              (-eval! ret#)
              (make-failure :mapcat>-result-not-an-effect
                {:result ret#}))))))))

(defn if>
  "If the condition is true,
     then returns the `then` effect,
     otherwise returns the `else` effect."
  [cond then> else> effect]
  (make-effect :if
    (constantly
      (let [res (-eval! effect)]
        (maybe-propagate-failure res
          (-eval!
            (if (cond res)
              (then> res)
              (else> res))))))))

(defn cond>
  "Evaluates the conditions in order until one of them returns true,
   then returns the effect associated with that condition.
   Conditions are test and expr pairs. tests are functions that take the result of the previous effect,
   and exprs are functions that take the result of the previous effect and return effects.

   If no conditions are met, then returns a failure."
  [& conditions]
  (make-effect :cond
    (constantly
      (let [effect (last conditions)
            conditions (->> conditions
                            (butlast)
                            (partition 2))]
        (let [res (-eval! effect)]
          (maybe-propagate-failure res
            (-eval!
              (loop [conditions conditions]
                (if-let [[test expr>] (first conditions)]
                  (if (test res)
                    (expr> res)
                    (recur (rest conditions)))
                  (fail> :cond :no-conditions))))))))))

(defmacro pipeline>>
  "Creates a pipeline of effects."
  [& body]
  `(fn [effect#]
     (->> effect#
          ~@body)))