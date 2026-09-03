# Architecture and Design of com.lambdaseq/fx

A comprehensive guide to the internal architecture, protocol mechanics, stack-safe interpreter, and AST manipulation capabilities of `fx`.

---

## 1. Core Philosophy & Design Principles

Traditional functional effect libraries often compile pipelines into nested, opaque closures `(fn [env] ...)` that capture state and parameters internally. While easy to construct, closures become black boxes at runtime: they cannot be inspected, rewritten in tests, or serialized.

`fx` is built around four foundational principles:

1. **100% Transparent Data AST:** Every effect stage is a first-class Clojure `defrecord` implementing `ITagged`, `IEffect`, and standard map interfaces. Configuration parameters (functions, retry policies, service keys) are stored as explicit record fields rather than closed over inside executable functions.
2. **Deterministic Monomorphic Contracts:** Every combinator strictly separates pure functions from `Effect` instances. There is zero dynamic coercion or runtime reflection overhead.
3. **Stack-Safe Continuation Interpreter:** Pipelines evaluate within a symmetric protocol-driven interpreter loop in constant $O(1)$ JVM stack space using heap-allocated continuation frames (`IContinuation`), completely preventing `StackOverflowError`.
4. **Pure Context Threading:** Environmental dependencies and services are passed as pure immutable maps across synchronous and asynchronous execution boundaries (`run-sync!` and `run-async!`) without Clojure dynamic vars (`*context*`).

---

## 2. Core Protocols and AST Records

The runtime is structured around five core protocols in `com.lambdaseq.fx.core`:

### Protocols

```clojure
(defprotocol ITagged
  (tag [this] "Returns the tag keyword identifying the structure."))

(defprotocol IFailure
  (error-data [this] "Returns the structured error data payload."))

(defprotocol IEffect
  (prev-effect [this] "Returns upstream effect or nil.")
  (-step [this value context stack]
    "Performs one forward execution step, returning:
     [next-effect next-value next-context next-stack]."))

(defprotocol IContinuation
  (-resume [this value context stack]
    "Resumes execution when a sub-computation completes, returning:
     [next-effect next-value next-context next-stack]."))

(defprotocol IUnwindable
  (-unwind [this exception rest-stack]
    "Handles exception unwinding during runtime exceptions, returning:
     {:effect ... :value ... :context ... :rest-stack ...} or nil."))
```

### AST Record Model

Every effect constructor produces a dedicated `defrecord` storing its upstream dependency (`:prev-effect`) and parameters:

```clojure
;; Example: MapEffect
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
```

Because effects are standard Clojure records, their fields can be directly inspected:

```clojure
(def r (fx/retry> (fx/map> inc) {:max-attempts 3 :delay-ms 100}))

(:tag r)    ;; => :retry
(:policy r) ;; => {:max-attempts 3, :delay-ms 100}
(:target r) ;; => #com.lambdaseq.fx.core.MapEffect{...}
```

---

## 3. The Stack-Safe Interpreter Engine

The execution engine evaluates pipelines by stepping forward through effects and resuming backward through continuation stack frames.

```
       Step forward: (-step effect val ctx stack)
     ──────────────────────────────────────────────>
 AST Nodes                                            Result / Failure
     <──────────────────────────────────────────────
       Resume backward: (-resume frame val ctx stack)
```

### Symmetric Dispatch Loop

The interpreter loop in `run-sync!` contains zero hardcoded `case` switch tables on effect tags:

```clojure
(defn run-sync!
  ([effect] (run-sync! effect {}))
  ([root-effect initial-context]
   (let [ctx (assoc (or initial-context {}) :runner run-sync!)]
     (loop [curr-eff root-effect
            curr-val nil
            curr-ctx ctx
            stack    ()]
       (if (nil? curr-eff)
         ;; Branch finished: pop and resume top continuation frame
         (if (empty? stack)
           curr-val
           (let [[frame & rest-stack] stack
                 [next-eff next-val next-ctx next-stack]
                 (-resume frame curr-val curr-ctx rest-stack)]
             (recur next-eff next-val next-ctx next-stack)))

         ;; Step next effect node polymorphically
         (let [[next-eff next-val next-ctx next-stack]
               (-step curr-eff curr-val curr-ctx stack)]
           (recur next-eff next-val next-ctx next-stack)))))))
```

### Continuation Frames

Continuation frames represent suspended computation states on the heap:

- `StepEffectFrame`: Resumes downstream pipeline execution after an upstream stage completes.
- `RestoreContextFrame`: Restores parent context maps after a child `provide>` scope completes.
- `EnsureFrame`: Runs finalizer effects while preserving the original upstream value or failure.
- `AcquireResourceFrame` / `AcquireReleaseUseFrame`: Manages resource acquisition, usage, and guaranteed release cleanup.
- `RetryFrame`: Manages retry iteration counts, backoff delays, and retry predicates.
- `AllFrame`: Collects batch results across vectors of effects.
- `IfFrame` / `CondFrame`: Evaluates conditional branches.

---

## 4. Context Propagation & Dependency Injection

Context is threaded as an explicit parameter through every step and continuation frame.

1. **No Dynamic Var Overhead:** Context does not use `^:dynamic *context*`, eliminating var lookups and re-binding penalties.
2. **Thread-Safe Async Propagation:** In `run-async!`, the immutable context map passes directly into Java `CompletableFuture` suppliers or JavaScript `Promise` callbacks without thread-local bleeding.
3. **Scoped Overrides:** `provide>` and `provide-service>` create localized sub-contexts for child effect trees, restoring the parent context when the child finishes via `RestoreContextFrame`.

---

## 5. Pipeline Metaprogramming with `com.lambdaseq.fx.utils`

Because effects are transparent data trees, `com.lambdaseq.fx.utils` provides pure functions to inspect, modify, and compose pipelines without executing them:

### Linear Inspection & Traversal

```clojure
(require '[com.lambdaseq.fx.utils :as fxu])

(def pipeline
  (-> (fx/service> :db)
      (fx/map> (fn [db] (str "connected:" db)))
      (fx/map> clojure.string/upper-case)))

(fxu/chain-length pipeline)     ;; => 3
(fxu/effect-tags pipeline)       ;; => [:context :map :map]
(fxu/root-effect pipeline)       ;; => #com.lambdaseq.fx.core.ContextEffect{...}
```

### Splicing & Pipeline Rewriting

```clojure
;; Replace root service lookup with static mock in tests
(def test-pipeline
  (fxu/replace-by-tag pipeline :context (fx/succeed> "mock-db")))

(fx/run-sync! test-pipeline)
;; => "CONNECTED:MOCK-DB"

;; Inject telemetry after specific stages
(def logged-pipeline
  (fxu/insert-after-tag pipeline :context (fx/tap> (fn [db] (println "DB initialized:" db)))))
```

### Higher-Order Combinators

- `pipe>`: Composes effect transformers `(Effect -> Effect)` in forward execution order.
- `comp>`: Composes effect transformers in standard right-to-left order.
- `pipe-fx-fn>`: Composes Kleisli arrows `(a -> Effect[b])` into a single effect-producing function.
- `around>`: Decorates an effect stage with before/after actions while preserving its value.
- `compose-ast-passes`: Composes multiple pipeline rewrite passes into an optimizing AST compiler.

---

## 6. Static Typing with `fx-typed`

The `modules/fx-typed` module provides Typed Clojure annotations for all effect constructors, combinators, and protocols.

```clojure
(ns my-app.typed-example
  (:require [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.typed]
            [typed.clojure :as t]))

(t/ann fetch-data (fx/IEffect t/Any String (t/Option (fx/IFailure (t/Val :not-found) t/Any)) '{:db t/Any}))
```
