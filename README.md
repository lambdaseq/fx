# com.lambdaseq/fx

A lightweight, purely functional effect system for Clojure and ClojureScript inspired by Effect-ts and ZIO.

`fx` models synchronous and asynchronous computations, resource lifecycles, dependency injection, and failure channels
as pure, transparent data structures composed with Clojure's standard thread-first (`->`) macro. Pipelines short-circuit
automatically on failure, manage resources safely, execute in constant stack space, and evaluate only when explicitly
triggered.

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {com.lambdaseq/fx-core {:mvn/version "0.2.0"}}}
```

For optional Typed Clojure type annotations:

```clojure
{:deps {com.lambdaseq/fx-typed {:mvn/version "0.2.0"}}}
```

## Quickstart

Functions that create or transform effects end in `>`, and functions that execute effects end in `!`.

```clojure
(require '[com.lambdaseq.fx.core :as fx])

(-> (fx/succeed> 20)
    (fx/map> inc)
    (fx/tap> (fn [v] (println "Current value:" v)))
    (fx/mapcat> (fn [v] (fx/succeed> (* v 2))))
    (fx/run-sync!))
;; Prints: "Current value: 21"
;; => 42
```

### Naming Conventions

- `>` suffix: Effect constructors, combinators, and pipeline transforms (e.g., `succeed>`, `fail>`, `map>`, `catch>`).
- `!` suffix: Execution runners that trigger evaluation and side effects (e.g., `run-sync!`, `run-async!`).
- `?` suffix: Standard boolean predicates (e.g., `effect?`, `failure?`).

## Philosophy

`fx` is built around a few guiding principles:

- **Programs as Data**: Workflows are represented as transparent, immutable data records rather than opaque closures.
  Pipelines can be inspected, queried, transformed, and mocked with pure functions before execution.
- **Explicitness over Magic**: No hidden dynamic vars, implicit coercions, or surprise runtime behavior. Dependencies,
  failure paths, and evaluation boundaries are declared explicitly.
- **Idiomatic Clojure**: Bring the reliability and safety of modern effect systems (like Effect-ts and ZIO) into Clojure
  without alien DSLs, composing naturally with standard threading (`->`) and data structures.
- **Pure Descriptions at the Core, Execution at the Edge**: Business logic remains completely pure and inert until
  evaluated at the application boundary.

## Mental Model

If you are new to effect systems, think of `fx` as a way to build robust, predictable programs by separating the
description of what your program does from its actual execution:

- **Descriptions over actions (Programs as Data)**: When you write `(fx/succeed> 10)` or `(fx/map> inc)`, you are not
  running operations immediately. Instead, you are building an immutable, data-driven blueprint of your workflow. This
  means you can pass workflows around, inspect them, or replace steps in tests before running anything.
- **Execution happens at the edge**: Pipelines remain completely inert until you explicitly run them with `fx/run-sync!`
  or `fx/run-async!`. This keeps your business logic pure and predictable.
- **Standard Clojure threading (`->`)**: Effects chain sequentially using Clojure's familiar thread-first macro (`->`),
  transforming values step by step.
- **Two distinct channels (Success vs. Failure)**: Computations result in either a successful value or a structured
  `Failure` record (with a `:tag` and error payload). You handle business errors as explicit data rather than throwing
  and catching exceptions.
- **Automatic short-circuiting**: If any step in a pipeline fails, downstream transformations (`map>`, `mapcat>`) are
  automatically skipped, and the failure flows straight to your error handlers (`catch>`, `or-else>`) or caller.
- **Pure dependency injection (Context)**: Steps declare what environment dependencies they need (such as a database
  pool or config map) via `service>` or `context>`. You provide these dependencies at run time via `provide>` or
  `run-sync!`, eliminating global state and dynamic vars.
- **Guaranteed resource safety**: Built-in primitives manage resource lifecycles (`acquire-release>`), guaranteed
  cleanup (`ensure>`), exception trapping (`try>`), and retrying transient errors (`retry>`).
- **Stack safe by design**: Pipelines execute using a heap-allocated loop rather than consuming the JVM or JS call
  stack. Deeply nested or long-running chains will never trigger a `StackOverflowError`.

## API Overview

### Core Constructors & Combinators (`com.lambdaseq.fx.core`)

| Function           | Signature                                               | Description                                                                                            |
|--------------------|---------------------------------------------------------|--------------------------------------------------------------------------------------------------------|
| `succeed>`         | `[val]`                                                 | Creates a successful effect yielding `val`.                                                            |
| `fail>`            | `([failure] [tag data])`                                | Creates a failed effect holding a typed `Failure`.                                                     |
| `map>`             | `([f] [eff f])`                                         | Transforms successful values with unary function `f`.                                                  |
| `map-ctx>`         | `([f] [eff f])`                                         | Transforms successful values with binary function `(f val context)`.                                   |
| `mapcat>`          | `([f] [eff f])`                                         | Flat-maps over an effect with an effect-producing function `(f val) -> Effect`.                        |
| `tap>`             | `([f] [eff f])`                                         | Executes side-effect `f` on success and passes the value through unchanged.                            |
| `tap-error>`       | `([f] [eff f])`                                         | Executes side-effect `f` on failure and passes the failure through unchanged.                          |
| `do-ctx>`          | `([f] [eff f])`                                         | Executes side-effect `(f val context)` and passes value unchanged.                                     |
| `context>`         | `([] [key] [key default])`                              | Yields the active execution context map or a specific key value.                                       |
| `service>`         | `([key] [key default])`                                 | Extracts a service dependency from the active context.                                                 |
| `provide>`         | `([ctx-map] [eff ctx-map] [eff body ctx-map])`          | Executes effects within a scoped context map override.                                                 |
| `provide-service>` | `([k v] [eff k v] [k v eff])`                           | Injects a single service key-value pair into context.                                                  |
| `ensure>`          | `([finalizer-eff] [eff finalizer-eff])`                 | Guaranteed finalizer that executes on success or failure, preserving upstream value.                   |
| `acquire-release>` | `([acquire use release] [use release])`                 | Safe resource bracket: acquires a resource, executes `use`, and guarantees `release`.                  |
| `retry>`           | `([policy] [eff policy])`                               | Retries a failed effect according to a retry policy map.                                               |
| `try>`             | `([eff] [eff catch] [prev eff catch])`                  | Catches thrown exceptions and converts them to typed failures.                                         |
| `if>`              | `([cond-eff then-eff else-eff] ...)`                    | Branches execution based on a predicate effect.                                                        |
| `cond>`            | `([eff & test-expr-pairs])`                             | Multi-branch conditional evaluating test-expression pairs.                                             |
| `match>`           | `([on-failure on-success] [eff on-failure on-success])` | Pattern-matches on both failure and success channels.                                                  |
| `or-else>`         | `([fallback-eff] [eff fallback-eff])`                   | Recovers from any failure by executing a fallback effect.                                              |
| `or-else-fail>`    | `([failure] [eff failure])`                             | Replaces any upstream failure with a custom failure descriptor.                                        |
| `all>`             | `[effects]`                                             | Evaluates a vector of independent effects and collects results into a vector.                          |
| `zip>`             | `[eff-a eff-b]`                                         | Combines two effects into a pair `[val-a val-b]`.                                                      |
| `zip-with>`        | `[eff-a eff-b f]`                                       | Combines two effects using a binary combining function `(f a b)`.                                      |
| `for-each>`        | `([f] [coll f])`                                        | Iterates over items applying an effect-producing function `f`.                                         |
| `catch>`           | `([handler-map] [eff handler-map])`                     | Recovers from specific failures using a `{tag-key handler-eff}` map.                                   |
| `catchall>`        | `([inner-eff] [eff inner-eff])`                         | Recovers from any failure by passing failure map to `inner-eff`.                                       |
| `die>`             | `([] [msg] [msg data] [eff msg data])`                  | Terminates execution by throwing an uncatchable exception.                                             |
| `or-die>`          | `([] [on-failure] [eff on-failure])`                    | Converts failures into fatal uncatchable runtime exceptions.                                           |
| `sleep>`           | `([ms] [eff ms])`                                       | Pauses execution for `ms` milliseconds.                                                                |
| `chain>`           | `[prev-eff current-eff]`                                | Links two effects into a sequential chain.                                                             |
| `run-sync!`        | `([eff] [eff context])`                                 | Evaluates an effect pipeline synchronously with optional context.                                      |
| `run-async!`       | `([eff] [eff context])`                                 | Evaluates an effect pipeline asynchronously (returns `CompletableFuture` on JVM / `js/Promise` on JS). |

---

### Pipeline Utilities & Metaprogramming (`com.lambdaseq.fx.utils`)

| Function               | Signature                    | Description                                                                    |
|------------------------|------------------------------|--------------------------------------------------------------------------------|
| `chain-length`         | `[leaf-eff]`                 | Returns total number of effect stages in the linear pipeline.                  |
| `effect-tags`          | `[leaf-eff]`                 | Returns vector of tags in forward execution order `[:succeed :map ...]`.       |
| `effect-seq`           | `[leaf-eff]`                 | Returns vector of effect nodes in forward execution order.                     |
| `root-effect`          | `[leaf-eff]`                 | Traverses upstream links to return the root effect.                            |
| `find-first-by-tag`    | `[leaf-eff tag]`             | Finds first effect node matching `tag`.                                        |
| `find-all-by-tag`      | `[leaf-eff tag]`             | Returns all effect nodes matching `tag`.                                       |
| `map-effects`          | `[leaf-eff f]`               | Applies pure transform `(f eff)` to every node in the pipeline.                |
| `replace-by-tag`       | `[leaf-eff tag replacement]` | Replaces first node matching `tag` with `replacement`.                         |
| `remove-by-tag`        | `[leaf-eff tag]`             | Removes all nodes matching `tag` and reconnects surrounding nodes.             |
| `insert-after-tag`     | `[leaf-eff tag new-eff]`     | Slices `new-eff` immediately after the first node matching `tag`.              |
| `insert-before-tag`    | `[leaf-eff tag new-eff]`     | Slices `new-eff` immediately before the first node matching `tag`.             |
| `concat-chains`        | `[chain-a chain-b]`          | Concatenates two effect pipelines into a single continuous chain.              |
| `slice-effects`        | `[leaf-eff start end]`       | Extracts a sub-pipeline between stage indices `start` and `end`.               |
| `ast-seq`              | `[root-eff]`                 | Traverses nested composite and branching AST nodes into a flat sequence.       |
| `pipe>`                | `[& combinators]`            | Composes pipeline combinators `(Effect -> Effect)` in forward execution order. |
| `comp>`                | `[& combinators]`            | Composes pipeline combinators in standard mathematical right-to-left order.    |
| `pipe-fx-fn>`          | `[& fx-fns]`                 | Composes effect-producing functions (Kleisli arrows: `a -> Effect[b]`).        |
| `around>`              | `[before-fn after-fn]`       | Wraps an effect stage with before/after actions while preserving its value.    |
| `with-scoped-service>` | `[key val combinator]`       | Wraps a combinator to execute inside a scoped service binding.                 |
| `compose-ast-passes`   | `[& passes]`                 | Composes multiple AST rewrite passes into an optimizing compiler pipeline.     |

## Guide & Examples

### Creating Effects & Error Propagation

Effects are constructed using `succeed>` or `fail>`. Upstream failures short-circuit downstream transformations without
executing them:

```clojure
;; Success
(-> (fx/succeed> 10)
    (fx/map> inc)
    (fx/run-sync!))
;; => 11

;; Short-circuiting failure
(-> (fx/fail> :not-found {:user-id 123})
    (fx/map> inc)
    (fx/run-sync!))
;; => #com.lambdaseq.fx.core.Failure{:tag :not-found, :error-data {:user-id 123}}
```

### Side Effects with `tap>` and `tap-error>`

Use `tap>` for logging, metric collection, or instrumentation on the success channel. The original value is preserved:

```clojure
(-> (fx/succeed> {:status 200})
    (fx/tap> (fn [res] (println "Response received:" res)))
    (fx/map> :status)
    (fx/run-sync!))
;; Prints: "Response received: {:status 200}"
;; => 200
```

Use `tap-error>` to inspect failures without recovering from them:

```clojure
(-> (fx/fail> :not-found {:user-id 123})
    (fx/tap-error> (fn [err] (println "Failed with tag:" (fx/tag err))))
    (fx/run-sync!))
;; Prints: "Failed with tag: :not-found"
;; => #com.lambdaseq.fx.core.Failure{:tag :not-found, :error-data {:user-id 123}}
```

### Guaranteed Finalization with `ensure>`

`ensure>` executes a finalizer effect (or function) after the upstream pipeline completes, guaranteed to run whether the
preceding steps succeeded or failed:

```clojure
;; Finalizer runs on success; original value is preserved
(-> (fx/succeed> "data")
    (fx/ensure> (fx/tap> (fn [_] (println "Cleaning up resources..."))))
    (fx/run-sync!))
;; Prints: "Cleaning up resources..."
;; => "data"

;; Finalizer runs on failure; original failure is preserved
(-> (fx/fail> :network-error {:url "http://example.com"})
    (fx/ensure> (fx/tap> (fn [_] (println "Closing connection..."))))
    (fx/run-sync!))
;; Prints: "Closing connection..."
;; => #com.lambdaseq.fx.core.Failure{:tag :network-error, :error-data {:url "http://example.com"}}
```

### Exception Safety with `try>`

`try>` wraps operations that may throw host exceptions (JVM `Throwable` or JS error) and converts them into structured
`Failure` records:

```clojure
;; Default failure tag (:try)
(-> (fx/succeed> "invalid-number")
    (fx/try> (fx/map> parse-long))
    (fx/run-sync!))
;; => #com.lambdaseq.fx.core.Failure{:tag :try, :error-data #error ...}

;; Custom failure keyword
(-> (fx/succeed> "invalid-number")
    (fx/try> (fx/map> parse-long) :parse-error)
    (fx/run-sync!))
;; => #com.lambdaseq.fx.core.Failure{:tag :parse-error, :error-data #error ...}

;; Fallback recovery effect
(-> (fx/succeed> "invalid-number")
    (fx/try> (fx/map> parse-long) (fx/succeed> 0))
    (fx/run-sync!))
;; => 0

;; Standalone or options map form
(fx/run-sync! (fx/try> {:try (fx/map> #(slurp "non-existent.txt")) :catch :io-error}))
;; => #com.lambdaseq.fx.core.Failure{:tag :io-error, :error-data #error ...}
```

### Branching: `if>` and `cond>`

Conditional combinators take effect combinators for predicates and branches:

```clojure
;; Binary branching with if>
(-> (fx/succeed> 10)
    (fx/if> (fx/map> even?)
            (fx/succeed> "even number")
            (fx/succeed> "odd number"))
    (fx/run-sync!))
;; => "even number"

;; Multi-branch with cond>
(-> (fx/succeed> 15)
    (fx/cond>
      (fx/map> #(zero? (mod % 15))) (fx/succeed> "FizzBuzz")
      (fx/map> #(zero? (mod % 3))) (fx/succeed> "Fizz")
      (fx/map> #(zero? (mod % 5))) (fx/succeed> "Buzz"))
    (fx/run-sync!))
;; => "FizzBuzz"
```

### Batching with `all>`

`all>` runs a vector of independent effects and aggregates their values into a vector:

```clojure
(-> (fx/all> [(fx/succeed> "a")
              (fx/succeed> "b")
              (fx/succeed> "c")])
    (fx/run-sync!))
;; => ["a" "b" "c"]
```

### Error Handling & Recovery

Recover from failures using `catch>` for specific error tags or `catchall>` for any error:

```clojure
;; Pattern match on specific failure tags (passes error-data to handler)
(-> (fx/fail> :user-not-found {:id 42})
    (fx/catch>
      {:user-not-found (fx/map> (fn [{:keys [id]}] {:id id :name "Guest"}))
       :db-timeout     (fx/succeed> {:id 0 :name "Fallback"})})
    (fx/run-sync!))
;; => {:id 42, :name "Guest"}

;; Catch any failure (passes {:tag ... :error-data ...} map to handler)
(-> (fx/fail> :service-unavailable {:retry-after 30})
    (fx/catchall>
      (fx/map> (fn [{:keys [tag error-data]}]
                 (str "Handled error " tag " (retry in " (:retry-after error-data) "s)"))))
    (fx/run-sync!))
;; => "Handled error :service-unavailable (retry in 30s)"
```

### Dependency Injection & Context Provision

`fx` provides first-class dependency injection and environmental context propagation without mutable singletons or
global state.

#### 1. Consuming Dependencies with `service>` and `context>`

Effects can access dependencies (database connections, HTTP clients, configuration maps) directly from the execution
context:

```clojure
(defn fetch-user [user-id]
  (-> (fx/service> :db)
      (fx/mapcat> (fn [db]
                    (fx/try> (fx/map> (fn [_] ((:query db) "SELECT * FROM users WHERE id = ?" user-id))))))))
```

#### 2. Context-Aware Transformations with `map-ctx>` and `do-ctx>`

When an operation needs both the pipeline's current value and active context simultaneously, use `map-ctx>` or `do-ctx>`
with a 2-argument callback `(f value context)`:

```clojure
(-> (fx/succeed> {:amount 100})
    (fx/map-ctx> (fn [order {:keys [tax-rate]}]
                   (assoc order :total (* (:amount order) (inc (or tax-rate 0))))))
    (fx/do-ctx> (fn [order {:keys [logger]}]
                  (when logger (logger (str "Calculated order total: " (:total order))))))
    (fx/run-sync! {:tax-rate 0.15 :logger println}))
;; Prints: "Calculated order total: 115.0"
;; => {:amount 100, :total 115.0}
```

#### 3. Scoped Dependency Injection with `provide>` and `provide-service>`

`provide>` and `provide-service>` scope context maps or individual services to an effect tree, ideal for test mocking
and isolation:

```clojure
(def mock-db
  {:query (fn [_sql id] {:id id :name "Test User"})})

;; Pipeline executes with injected mock dependency
(-> (fetch-user 42)
    (fx/provide> {:db mock-db})
    (fx/run-sync!))
;; => {:id 42, :name "Test User"}

;; Single-service helper
(-> (fetch-user 42)
    (fx/provide-service> :db mock-db)
    (fx/run-sync!))
;; => {:id 42, :name "Test User"}
```

#### 4. Running with Production Environment Context

Supply production dependencies at the application boundary using `run-sync!`:

```clojure
(def prod-context
  {:db     (create-connection-pool db-config)
   :logger (create-logger)
   :env    :prod})

(fx/run-sync! (fetch-user 42) prod-context)
```

### Resource Management with `acquire-release>`

`acquire-release>` ensures resources (file handles, sockets, database transactions) are properly acquired, used, and
guaranteed to be released even if exceptions or failures occur:

```clojure
(def read-file-safely
  (fx/acquire-release>
    ;; Acquire
    (fx/map> (fn [path] (clojure.java.io/reader path)))
    ;; Use
    (fn [reader] (fx/succeed> (line-seq reader)))
    ;; Release (guaranteed to run)
    (fn [reader] (.close reader))))

(-> (fx/succeed> "data.txt")
    read-file-safely
    (fx/run-sync!))
```

### Automatic Retries with `retry>`

`retry>` re-evaluates a target effect with customizable backoff and predicate policies:

```clojure
(def resilient-fetch
  (-> (fetch-user 42)
      (fx/retry> {:max-attempts   3
                  :delay-ms       100
                  :backoff-factor 2.0
                  :retry-if       (fn [failure] (= (fx/tag failure) :timeout))})))

(fx/run-sync! resilient-fetch prod-context)
```

### Asynchronous Execution with `run-async!`

`run-async!` executes effect pipelines asynchronously on background thread pools (JVM `CompletableFuture` / JS
`Promise`) with zero dynamic var binding cost:

```clojure
;; Returns CompletableFuture on JVM / js/Promise on JS
(def future-res
  (-> (fx/succeed> 100)
      (fx/map> inc)
      (fx/run-async! prod-context)))

@future-res
;; => 101
```

## Pipeline Metaprogramming & Utilities (`com.lambdaseq.fx.utils`)

Because FX effects are 100% transparent data records, `com.lambdaseq.fx.utils` allows you to inspect, query, transform,
optimize, and compose effect pipelines as pure data without executing them.

```clojure
(require '[com.lambdaseq.fx.utils :as fxu])
```

### 1. Inspection & Querying

```clojure
(def pipeline
  (-> (fx/service> :db)
      (fx/map> (fn [db] (str "db:" db)))
      (fx/tap> println)
      (fx/map> clojure.string/upper-case)))

(fxu/chain-length pipeline)
;; => 4

(fxu/effect-tags pipeline)
;; => [:context :map :tap :map]

(fx/tag (fxu/root-effect pipeline))
;; => :context

;; Find specific stages
(fxu/find-first-by-tag pipeline :tap)
(fxu/find-all-by-tag pipeline :map)
```

### 2. Splicing, Mocking, and AST Rewriting

```clojure
;; Replace root dependency lookup with a static mock in tests
(def test-pipeline
  (fxu/replace-by-tag pipeline :context (fx/succeed> "mock-postgres")))

(fx/run-sync! test-pipeline)
;; => "DB:MOCK-POSTGRES"

;; Strip out logging/telemetry stages for fast unit tests
(def silent-pipeline
  (fxu/remove-by-tag pipeline :tap))

;; Inject telemetry after the root step
(def instrumented
  (fxu/insert-after-tag pipeline :context (fx/tap> (fn [db] (println "Connected to" db)))))
```

### 3. Higher-Order Combinator Composition

Compose reusable pipeline transformers and effect functions cleanly:

```clojure
;; Compose combinators in forward execution order
(def audit-and-retry
  (fxu/pipe>
    (fn [eff] (fx/tap> eff (fn [v] (println "Auditing:" v))))
    (fn [eff] (fx/retry> eff {:max-attempts 3 :delay-ms 100}))))

(def secure-pipeline
  (-> (fx/succeed> 10)
      audit-and-retry
      (fx/map> inc)))

;; Compose effect-producing functions (Kleisli arrows: a -> Effect[b])
(defn fetch-user-fx [id] (fx/succeed> {:id id :name "Alice"}))
(defn load-profile-fx [user] (fx/succeed> (str "Profile of " (:name user))))

(def get-user-profile (fxu/pipe-fx-fn> fetch-user-fx load-profile-fx))

(fx/run-sync! (get-user-profile 42))
;; => "Profile of Alice"
```

## Typed Clojure Integration

`fx-typed` provides complete type signatures for Typed Clojure. Effect channels and variance are fully tracked:

```clojure
(ns my-app.core
  (:require [com.lambdaseq.fx.core :as fx]
            [com.lambdaseq.fx.typed]
            [typed.clojure :as t]))

;; (fx/IEffect In Out Failure Context)
(t/ann calculate-total (fx/IEffect t/Any Long (t/Option (fx/IFailure (t/Val :calc-err) t/Any)) '{}))
(def calculate-total
  (-> (fx/succeed> 10)
      (fx/try> (fx/map> inc) :calc-err)))
```

## License

Copyright © 2024 LambdaSeq Works LTD.

Distributed under the Eclipse Public License version 1.0.
