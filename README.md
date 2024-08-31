# com.lambdaseq/fx

A Clojure library that implements a functional effect system, akin to Effect.ts, Scala's ZIO, or Haskell's IO monad.
Using Clojure's elegant syntax and macros provides a simple and powerful way to model side effects in a pure functional way.
For example composing effects can be simply done using the `->>` macro.

## Usage
Functions in `fx` that return effects are suffixed with `>`, and functions that evaluate effects are suffixed with `!`.
You can `require` the core namespace and use the following functions:

```clojure
(ns my-ns
  (:require [com.lambdaseq.fx :as fx]))

(->> (fx/succeed> 42)
     ; maps the value of the previous effect
     (fx/map> inc)
     ; maps the value of the previous effect, but returns a new effect.
     (fx/mapcat> (fn [x] (fx/succeed> (* x 2))))
     (eval!))
;; => 86
```

`fx` effects short circuit on the first error, and can be used to model side effects in a pure functional way.

```clojure
(->> (fx/fail! :error {})
     (fx/map> (fn [_] (println "This does not print" 42)))
     (eval!))

;; => #com.lambdaseq.fx.core.Failure {:data {}, :type :error}
```
An elegant way to do conditionals is also provided by the following functions:

```clojure
; Using the `fx/if>` function
(->> (fx/succeed> 42)
     (fx/if> even?
             (constantly (fx/succeed> "even"))
             (constantly (fx/fail> :odd {})))
     (eval!))
;; => "even"
```

You can also declare reusable effects using the `pipeline>>` macro:

```clojure
(def inc-twice>
  (fx/pipeline>>
    (fx/map> inc)
    (fx/map> inc)))
```
which is the same as writing:

```clojure
(defn inc-twice> [x]
  (->> x
       (fx/map> inc)
       (fx/map> inc)))
```


## License

Copyright © 2024 lambdaseq.com

Distributed under the Eclipse Public License version 1.0.
