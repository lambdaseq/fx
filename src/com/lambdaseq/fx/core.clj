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
  "Maps over the effect. Takes a function f and/or an effect, and returns a new effect."
  [f effect]
  (make-effect :map
    (constantly
      (let [res# (-eval! effect)]
        (maybe-propagate-failure res#
          (f res#))))))

(defn mapcat>
  "Flat maps over the effect. Takes a function f that returns an effect and/or an effect, and returns a new effect."
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

(defmacro pipeline>>
  "Creates a pipeline of effects."
  [& body]
  `(fn [effect#]
     (->> effect#
          ~@body)))


(comment

  (->>
    (fail> :foo {})
    (map> (fn [v] (println "Executed")
            (inc v)))
    (-eval!))

  (->>
    #_(fail> :foo {})
    (succeed> 42)
    (mapcat> (fn [v]
               (println "Executed")
               (inc v)))
    (-eval!))

  (def inc-twice>
    (pipeline>>
      (map> inc)
      (map> inc)))

  (->>
    (succeed> 1)
    (inc-twice>)
    (-eval!))

  (->>
    (succeed> 1)
    (mapcat> (fn [v] (succeed> (inc v))))
    (-eval!))

  (def cond-pipeline>
    (pipeline>>
      (if> true?
        (constantly (do
                      (print "Then branch")
                      (succeed> 1)))
        (constantly (do
                      (print "Else branch")
                      (fail> :foo "error"))))
      (map> inc)))

  (->>
    (succeed> false)
    (cond-pipeline>)
    (-eval!))

  (->>
    (succeed> true)
    (cond-pipeline>)
    (-eval!))

  (->>
    (fail> :foo {})
    (cond-pipeline>)
    (-eval!))

  )
