# Architecture and Design of fx

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

The runtime is structured around five core protocols in `fx.core`:

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
(:target r) ;; => #fx.core.MapEffect{...}
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

## 5. Pipeline Metaprogramming with `fx.utils`

Because effects are transparent data trees, `fx.utils` provides pure functions to inspect, modify, and compose pipelines without executing them:

### Linear Inspection & Traversal

```clojure
(require '[fx.utils :as fxu])

(def pipeline
  (-> (fx/service> :db)
      (fx/map> (fn [db] (str "connected:" db)))
      (fx/map> clojure.string/upper-case)))

(fxu/chain-length pipeline)     ;; => 3
(fxu/effect-tags pipeline)       ;; => [:context :map :map]
(fxu/root-effect pipeline)       ;; => #fx.core.ContextEffect{...}
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
  (:require [fx.core :as fx]
            [fx.typed]
            [typed.clojure :as t]))

(t/ann fetch-data (fx/IEffect t/Any String (t/Option (fx/IFailure (t/Val :not-found) t/Any)) '{:db t/Any}))
```

---

## 7. Defining Custom Effects & Continuation Frames

Because `fx` is protocol-driven rather than interpreter-table-driven, extending the system with custom effects does not require patching the core runner.

### Implementing `IEffect`

Every custom effect record must implement:
1. `ITagged`: `(tag [this])` returning a descriptive keyword.
2. `IEffect`:
   - `(prev-effect [this])`: returns `:prev-effect` or `nil`.
   - `(-step [this val context stack])`: returns `[next-effect next-val next-context next-stack]`.

```clojure
(defrecord MultiplyEffect [tag prev-effect data factor]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      ;; Push StepEffectFrame to evaluate upstream effect first
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      ;; Evaluate step: short-circuit on failure or compute result
      (if (fx/failure? val)
        [nil val context stack]
        [nil (* val factor) context stack]))))

(defn multiply>
  ([factor] (multiply> nil factor))
  ([prev-eff factor]
   (->MultiplyEffect :multiply prev-eff {:factor factor} factor)))
```

### Implementing `IContinuation` and `IUnwindable`

When an effect needs to suspend its own computation and wait for an inner effect or sub-pipeline to complete:

```clojure
(defrecord MultiplierFrame [factor]
  fx/IContinuation
  (-resume [_ val context stack]
    [nil (* val factor) context stack]))
```

Continuation frames that manage resources can also implement `IUnwindable` to ensure cleanup during unhandled exceptions:

```clojure
(defrecord ResourceCleanupFrame [resource release-fn]
  fx/IContinuation
  (-resume [_ val context stack]
    (release-fn resource)
    [nil val context stack])
  fx/IUnwindable
  (-unwind [_ exception rest-stack]
    (release-fn resource)
    nil))
```

---

## 8. Composable Dependency Injection & Resource Lifecycles (`fx.layer`)

The `fx.layer` module introduces declarative dependency injection and scoped lifecycle management inspired by Effect-ts and ZIO.

### Core Protocols

1. **`IScope`:**
   - `(add-finalizer! [this finalizer-eff])`: Registers a cleanup effect in the active lifecycle scope.
   - `(close-scope!> [this])`: Produces an effect executing all registered finalizers in reverse registration order, guaranteeing that each finalizer runs even if preceding finalizers fail or throw exceptions.
2. **`ILayer`:**
   - `(-build-eff [this scope])`: Produces an effect that instantiates the layer's services within `scope` and returns a context map.

### AST Layer Types

- `ValueLayer`: Unmanaged static context map.
- `EffectLayer`: Unmanaged single service computed by evaluating an effect.
- `ResourceLayer`: Managed single resource acquiring via an effect and registering a release effect in `IScope`.
- `MapResourceLayer`: Managed multi-service map acquiring via an effect and registering a release effect in `IScope`.
- `MergeLayer`: Horizontal composition of independent layers.
- `ComposeLayer`: Vertical composition feeding upstream context to downstream layer acquisition.

### Deterministic Lifecycle Guarantees

When a layer is evaluated via `provide-layer>`, `with-layer>`, or managed via `start-layer!`/`stop-layer!`:
1. Resources are acquired in sequence.
2. If any stage during layer acquisition fails, already-acquired resources in the scope are immediately finalized in reverse acquisition order before the failure returns.
3. Upon pipeline completion, normal exit, or unhandled exceptions, all registered finalizers are executed deterministically.

---

## 9. Observability, Distributed Tracing & Diagnostics (`fx.observability`)

The `modules/fx-observability` module introduces zero-dependency contextual logging, distributed tracing, high-concurrency in-memory metrics, typed failure diagnostics, and telemetry taps built directly on top of `fx`'s immutable context map.

### Context-Driven Observability State

Because `fx` passes context immutably across continuation steps (`-step` and `-resume`), observability state does not rely on global singletons or JVM `ThreadLocal` storage:

```
┌─────────────────────────────────────────────────────────────────────────┐
│                          fx Execution Context                           │
│  {:fx.observability/log-annotations {:user-id "123", :request-id "abc"} │
│   :fx.observability/trace-spans     [{:name "http.req", :span-id "00f1"}]│
│   :fx.observability/metrics-registry (ConcurrentMetricsRegistry)        │
│   :fx.observability/logger          console-logger-sink                 │
│   :fx.observability/span-reporter   (fn [span] ...) }                   │
└─────────────────────────────────────────────────────────────────────────┘
```

1. **Contextual Logging (`fx.observability.log`):** `annotate-logs>` injects key-value pairs into `:fx.observability/log-annotations` which downstream steps automatically inherit.
2. **Distributed Tracing (`fx.observability.trace`):** `with-span>` pushes span frames onto `:fx.observability/trace-spans` and records nanosecond elapsed durations and parent-child hierarchies with W3C `traceparent` codec support (`extract-trace-context>`, `inject-trace-context>`).
3. **In-Memory Metrics (`fx.observability.metrics`):** Lock-free atomic counters (`LongAdder`), gauges, and timers track latency distributions and request throughput with `metrics-snapshot!` queries.
4. **Diagnostics & Cause Algebra (`fx.observability.diagnostics`):** `sandbox>` wraps domain failures (`Fail`) and unexpected JVM defects (`Die`) into a typed `Cause` tree on the success channel, enabling full programmatic inspection and visual ASCII rendering (`render-cause`, `render-execution-trace`).
