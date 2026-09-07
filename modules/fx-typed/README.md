# fx/typed

Typed Clojure annotations and static type-checking integration for the `fx` effect system.

`fx-typed` provides complete type signatures and channel tracking for Typed Clojure. It enables static type checking across effect pipelines, verifying input requirements, success payload types, failure channel contracts, and required environmental dependencies.

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {io.github.conjurernix/fx.typed {:mvn/version "0.0.1-alpha"}}}
;; or local module coordinate
{:deps {fx/typed {:mvn/version "0.0.1-alpha"}}}
```

Requires `fx/core` and Typed Clojure (`org.typedclojure/typed.clj.checker`).

---

## Mental Model & Type Structure

In `fx-typed`, an effect is represented as a 4-parameter type constructor:

```clojure
(fx/IEffect In Out Failure Context)
```

### Type Parameters and Variance

1. **`In` (Contravariant)**: The input type expected by the effect node when evaluating an upstream value in the pipeline.
2. **`Out` (Covariant)**: The output type produced on the success channel upon successful evaluation.
3. **`Failure` (Covariant)**: The error type yielded on the failure channel, bounded by `(t/Option (fx/IFailure Tag Error))`. If an effect cannot fail, this is `nil` or `t/Nothing`.
4. **`Context` (Covariant)**: The required environmental context map structure needed to evaluate the effect, bounded by `Context` (an open HMap / map of keywords).

### Typed Protocols

`fx-typed` annotates the fundamental protocols of the `fx` effect system:

- **`fx/IFailure`**:
  ```clojure
  (t/ann-protocol [[failure-tag :< t/Keyword :variance :covariant]
                   [error :variance :covariant]] fx/IFailure
    -error [(fx/IFailure failure-tag error) -> error])
  ```
- **`fx/ITagged`**:
  ```clojure
  (t/ann-protocol [[tag :< t/Keyword :variance :covariant]] fx/ITagged
    -tag [(fx/ITagged tag) -> tag])
  ```
- **`fx/IEffect`**:
  ```clojure
  (t/ann-protocol [[in :variance :contravariant]
                   [out :variance :covariant]
                   [failure :< (t/Option (fx/IFailure t/Keyword t/Any)) :variance :covariant]
                   [context :< Context :variance :covariant]] fx/IEffect
    -prev-effect [(fx/IEffect in out failure context) -> (t/Option (fx/IEffect t/Any in failure Context))]
    -step [(fx/IEffect in out failure context) in context (t/List t/Any) -> (t/Vec t/Any)])
  ```

---

## Quickstart

```clojure
(ns my-app.typed-example
  (:require [fx.core :as fx]
            [fx.typed]
            [typed.clojure :as t]))

;; Annotate an effect definition:
;; (fx/IEffect In Out Failure Context)
(t/ann compute-discount
       (fx/IEffect t/Any Double (t/Option (fx/IFailure (t/Val :invalid-amount) String)) '{}))

(def compute-discount
  (-> (fx/succeed> 150.0)
      (fx/mapcat> (fn [amount]
                    (if (pos? amount)
                      (fx/succeed> (* amount 0.9))
                      (fx/fail> :invalid-amount "Amount must be positive"))))))
```

---

## Key Type Signatures

### Constructors

- **`fx/succeed>`**:
  ```clojure
  (t/ann fx/succeed> (t/All [x] [x -> (fx/IEffect t/Any x nil Context)]))
  ```
- **`fx/fail>`**:
  ```clojure
  (t/ann fx/fail>
         (t/All [[key :< t/Keyword] x]
                (t/IFn
                  [x -> (fx/IEffect t/Any t/Nothing (fx/IFailure (t/Val :fail) x) Context)]
                  [key x -> (fx/IEffect t/Any t/Nothing (fx/IFailure key x) Context)])))
  ```

### Combinators

- **`fx/map>`**:
  ```clojure
  (t/ann fx/map>
         (t/All [in out
                 [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
                 [context :< Context]]
                (t/IFn
                  [[in -> out]
                   -> (fx/IEffect in out nil Context)]
                  [(fx/IEffect t/Any in failure context)
                   [in -> out]
                   -> (fx/IEffect t/Any out failure context)])))
  ```
- **`fx/mapcat>`**:
  ```clojure
  (t/ann fx/mapcat>
         (t/All [in out
                 [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
                 [next-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
                 [context :< Context]]
                (t/IFn
                  [[in -> (fx/IEffect t/Any out next-failure context)]
                   -> (fx/IEffect in out next-failure context)]
                  [(fx/IEffect t/Any in prev-failure context)
                   [in -> (fx/IEffect t/Any out next-failure context)]
                   -> (fx/IEffect t/Any out (t/U prev-failure next-failure) context)])))
  ```
- **`fx/tap>` / `fx/tap-error>`**:
  Preserves output types and propagates failure channels without mutation.
- **`fx/catch>`**:
  Unifies remaining failure tags when handlers recover from specific failure subsets.
- **`fx/acquire-release>`**:
  Ensures resource acquisition, usage, and release bracket types match safely.

### Runners

- **`fx/run-sync!`**:
  ```clojure
  (t/ann fx/run-sync!
         (t/All [out [failure :< (t/Option (fx/IFailure t/Keyword t/Any))] [context :< Context]]
                (t/IFn
                  [(fx/IEffect t/Any out failure Context) -> (t/U out failure)]
                  [(fx/IEffect t/Any out failure context) context -> (t/U out failure)])))
  ```
- **`fx/run-async!`**:
  ```clojure
  (t/ann fx/run-async!
         (t/All [out [failure :< (t/Option (fx/IFailure t/Keyword t/Any))] [context :< Context]]
                (t/IFn
                  [(fx/IEffect t/Any out failure Context) -> (CompletableFuture (t/U out failure))]
                  [(fx/IEffect t/Any out failure context) context -> (CompletableFuture (t/U out failure))])))
  ```

---

## Type Checking with Typed Clojure

To run type checking on your project:

```clojure
(require '[typed.clojure :as t])

;; Check an entire namespace
(t/check-ns 'my-app.typed-example)
```

Or run the test suite across `modules/fx-typed`:

```bash
clojure -T:build test
```
