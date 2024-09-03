# com.lambdaseq/fx

A Clojure library that implements a functional effect system, akin to Effect.ts, Scala's ZIO, or Haskell's IO monad.
Using Clojure's elegant syntax and macros provides a simple and powerful way to model side effects in a pure functional
way.
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
     (run-sync!))
;; => 86
```

`fx` effects short circuit on the first error, and can be used to model side effects in a pure functional way.

```clojure
(->> (fx/fail! :error {})
     (fx/map> (fn [_] (println "This does not print" 42)))
     (run-sync!))

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

### Handling Errors

You can handle errors using the `fx/catch>` function:

```clojure
(->> (fx/fail! :error {:value 42})
     (fx/catch>
       {:error (fn [{:keys [value]}] (fx/succeed> value))})
     (run-sync!))
;; => "error"
```

or catch all errors using the `fx/catchall>` function:

```clojure
(->> (fx/fail! :error {:value 42})
     (fx/catchall>
       (fn [e] (fx/succeed> (str "Caught error: " e))))
     (run-sync!))
;; => "Caught error: #com.lambdaseq.fx.core.Failure {:data {:value 42}, :type :error}"
```

## Future Goals

We intend to add the following features in the future:

- [ ] Different runtimes and run strategies (similar to Effect.ts or ZIO's `Runtime`).
- [ ] Build a suite of concurrency and parallelism primitives on top of `fx`.
- [ ] Build helpers for common Clojure libraries that use side
  effects ([ring](https://github.com/ring-clojure/ring)/[pedestal](https://github.com/pedestal/pedestal), [next.jdbc](https://github.com/seancorfield/next-jdbc),
  etc), and extend them potentially into a full webstack.
- [ ] Build a suite of monitoring and tracing tools using `fx` primitives.
- [ ] Integrate with [typedclojure](https://typedclojure.org/), for a complete type safe effect system.

## License

Copyright © 2024 [LambdaSeq Works LTD.](lambdaseq.com),

Distributed under the Eclipse Public License version 1.0.
