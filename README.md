# com.lambdaseq/fx

A lightweight, purely functional effect system for Clojure and ClojureScript inspired by Effect-ts and ZIO.

`fx` models synchronous computations, side effects, and failure channels as pure data structures composed with Clojure's thread-first (`->`) macro. Pipelines short-circuit automatically on failure, catch exceptions safely, and execute only when explicitly triggered.

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {com.lambdaseq/fx-core {:mvn/version "0.1.0"}}}
```

For optional Typed Clojure type annotations:

```clojure
{:deps {com.lambdaseq/fx-typed {:mvn/version "0.1.0"}}}
```

## Quickstart

Functions that create or transform effects end in `>`, and functions that execute effects end in `!`.

```clojure
(require '[com.lambdaseq.fx.core :as fx])

(-> (fx/succeed> 20)
    (fx/map> inc)
    (fx/tap> (fn [v] (println "Current value:" v)))
    (fx/mapcat> (fx/map> (fn [v] (* v 2))))
    (fx/run-sync!))
;; Prints: "Current value: 21"
;; => 42
```

## Mental Model

- **Effects as data**: Combinators return lazy, inspectable `Effect` records describing steps in a computation.
- **Thread-first composition**: Effects chain naturally with standard Clojure `->` threading.
- **Two output channels**: Pipelines yield either a success value or a typed `Failure` record.
- **Automatic short-circuiting**: On failure, downstream `map>`, `mapcat>`, and `tap>` steps are skipped automatically.
- **Explicit execution**: Pipelines stay inert until evaluated with `run-sync!`.

## API Overview

| Function | Signature | Description |
| --- | --- | --- |
| `succeed>` | `[val]` | Creates a successful effect yielding `val`. |
| `fail>` | `([failure] [tag data])` | Creates a failed effect holding a typed `Failure`. |
| `map>` | `([f] [eff f])` | Transforms successful values with unary function `f`. |
| `map-ctx>` | `([f] [eff f])` | Transforms successful values with binary function `(f val context)`. |
| `mapcat>` | `([inner-eff] [eff inner-eff])` | Flat-maps over an effect with an effect combinator. |
| `tap>` | `([f] [eff f])` | Executes side-effect `f` on success and passes the value through unchanged. |
| `tap-error>` | `([f] [eff f])` | Executes side-effect `f` on failure and passes the failure through unchanged. |
| `do-ctx>` | `([f] [eff f])` | Executes side-effect `(f val context)` and passes value unchanged. |
| `context>` | `([] [key] [key default])` | Yields the active execution context map or a key value. |
| `service>` | `([key] [key default])` | Extracts a service dependency from the active context. |
| `provide>` | `([ctx] [eff ctx] [eff body ctx])` | Executes effects within a scoped context map. |
| `provide-service>` | `([k v] [eff k v] [k v eff])` | Injects a single service key-value pair into context. |
| `ensure>` | `([finalizer-eff] [eff finalizer-eff])` | Guaranteed finalizer that executes on success or failure. |
| `try>` | `([eff] [eff catch] [prev eff catch])` | Catches thrown exceptions and converts them to typed failures. |
| `if>` | `([cond-eff then-eff else-eff] ...)` | Branches execution based on a predicate effect. |
| `cond>` | `([eff & test-expr-pairs])` | Multi-branch conditional evaluating test-expression pairs. |
| `all>` | `[effects]` | Evaluates a vector of independent effects and collects results. |
| `catch>` | `([f-map] [eff f-map])` | Recovers from specific failures using a `{tag-key handler}` map. |
| `catchall>` | `([inner-eff] [eff inner-eff])` | Recovers from any failure by passing failure map to `inner-eff`. |
| `chain>` | `[prev-eff current-eff]` | Links two effects into a sequential chain. |
| `run-sync!` | `([eff] [eff context])` | Evaluates an effect pipeline with optional initial context. |

## Guide & Examples

### Creating Effects & Error Propagation

Effects are constructed using `succeed>` or `fail>`. Upstream failures short-circuit downstream transformations without executing them:

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

`ensure>` executes a finalizer effect (or function) after the upstream pipeline completes, guaranteed to run whether the preceding steps succeeded or failed:

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

`try>` wraps operations that may throw host exceptions (JVM `Throwable` or JS error) and converts them into structured `Failure` records:

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
      (fx/map> #(zero? (mod % 3)))  (fx/succeed> "Fizz")
      (fx/map> #(zero? (mod % 5)))  (fx/succeed> "Buzz"))
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

`fx` provides first-class dependency injection and environmental context propagation without mutable singletons or global state.

#### 1. Consuming Dependencies with `service>` and `context>`

Effects can access dependencies (database connections, HTTP clients, configuration maps) directly from the execution context:

```clojure
(defn fetch-user [user-id]
  (-> (fx/service> :db)
      (fx/mapcat> (fn [db]
                    (fx/try> (fx/map> (fn [_] ((:query db) "SELECT * FROM users WHERE id = ?" user-id))))))))
```

#### 2. Context-Aware Transformations with `map-ctx>` and `do-ctx>`

When an operation needs both the pipeline's current value and active context simultaneously, use `map-ctx>` or `do-ctx>` with a 2-argument callback `(f value context)`:

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

`provide>` and `provide-service>` scope context maps or individual services to an effect tree, ideal for test mocking and isolation:

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
  {:db (create-connection-pool db-config)
   :logger (create-logger)
   :env :prod})

(fx/run-sync! (fetch-user 42) prod-context)
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
