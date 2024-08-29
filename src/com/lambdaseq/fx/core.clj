(ns com.lambdaseq.fx.core)

(defprotocol IEffect
  (eval! [this]
    "Evaluates the effect and returns the result."))

(defrecord Effect [run]
  IEffect
  (eval! [_] (run)))

(defn <succeed [value]
  "Creates a successful effect that just returns the value."
  (Effect. (constantly value)))

(defn <fail [error]
  "Creates a failed effect that throws an ex-info error."
  (Effect. (constantly (throw (ex-info error {})))))

(defn <map
  "Maps over the effect. Takes a function f and/or an effect, and returns a new effect."
  ([f]
   (fn [effect]
     (<map f effect)))
  ([f effect]
   (Effect. (constantly (f (eval! effect))))))

(defn <mapcat
  "Flat maps over the effect. Takes a function f that returns an effect and/or an effect, and returns a new effect."
  ([f]
   (fn [effect]
     (<mapcat f effect)))
  ([f effect]
   (Effect. (constantly (eval! (f (eval! effect)))))))


(comment

  (->
    (<succeed 1)
    (<map inc)
    (<map inc)
    (eval!))

  )