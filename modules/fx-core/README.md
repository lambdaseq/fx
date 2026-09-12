# fx/core

Declarative, stack-safe, and purely functional effect system for Clojure and ClojureScript.

`fx.core` models synchronous and asynchronous computations, resource lifecycles, dependency injection, and failure channels as pure, transparent data structures composed with Clojure's standard thread-first (`->`) macro. Pipelines short-circuit automatically on failure, manage resources safely, execute in constant stack space, and evaluate only when explicitly triggered at application boundaries.

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {io.github.conjurernix/fx.core {:mvn/version "0.1.0"}}}
;; or local module coordinate
{:deps {fx/core {:mvn/version "0.1.0"}}}
```

---

## Philosophy

- **Programs as Data**: Workflows are represented as transparent, immutable data records rather than opaque closures. Pipelines can be inspected, queried, transformed, and mocked with pure functions before execution.
- **Explicitness over Magic**: No hidden dynamic vars, implicit coercions, or surprise runtime behavior. Dependencies, failure paths, and evaluation boundaries are declared explicitly.
- **Idiomatic Clojure**: Brings the reliability and safety of modern effect systems (such as Effect-ts and ZIO) into Clojure without alien DSLs, composing naturally with standard threading (`->`) and data structures.
- **Pure Descriptions at the Core, Execution at the Edge**: Business logic remains completely pure and inert until evaluated at the application boundary via `run-sync!` or `run-async!`.

---

## Mental Model

- **Descriptions over actions**: Writing `(fx/succeed> 10)` or `(fx/map> inc)` builds an immutable blueprint of your workflow. No side-effects occur until execution.
- **Execution happens at the edge**: Pipelines evaluate via `fx/run-sync!` or `fx/run-async!`, preserving purity across business logic.
- **Standard Clojure threading (`->`)**: Effects chain sequentially using Clojure's familiar thread-first macro (`->`), transforming values step by step.
- **Two distinct channels (Success vs. Failure)**: Computations produce either a successful value or a structured `Failure` record (with a `:tag` and error payload).
- **Automatic short-circuiting**: If any step fails, downstream transformations (`map>`, `mapcat>`) are skipped, and the failure flows straight to error handlers (`catch>`, `or-else>`) or caller.
- **Pure dependency injection (Context)**: Steps declare environmental dependencies via `service>` or `context>`. You provide these dependencies at run time via `provide>` or `run-sync!`.
- **Guaranteed resource safety**: Built-in primitives manage resource lifecycles (`acquire-release>`), guaranteed cleanup (`ensure>`), exception trapping (`try>`), and retries (`retry>`).
- **Stack safe by design**: Pipelines execute using a heap-allocated loop rather than consuming the JVM or JS call stack, avoiding `StackOverflowError`.

---

## Quickstart

```clojure
(ns example.core
  (:require [fx.core :as fx]))

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

---

## API Reference

### Core Constructors & Combinators (`fx.core`)

| Function | Signature | Description |
|---|---|---|
| `succeed>` | `[val]` | Creates a successful effect yielding `val`. |
| `fail>` | `([failure] [tag data])` | Creates a failed effect holding a typed `Failure`. |
| `map>` | `([f] [eff f])` | Transforms successful values with unary function `f`. |
| `map-ctx>` | `([f] [eff f])` | Transforms successful values with binary function `(f val context)`. |
| `mapcat>` | `([f] [eff f])` | Flat-maps over an effect with a required effect-producing function `(f val) -> Effect`. |
| `tap>` | `([f] [eff f])` | Executes side-effect `f` on success and passes the value through unchanged. |
| `tap-error>` | `([f] [eff f])` | Executes side-effect `f` on failure and passes the failure through unchanged. |
| `do-ctx>` | `([f] [eff f])` | Executes side-effect `(f val context)` and passes value unchanged. |
| `context>` | `([] [key] [key default])` | Yields the active execution context map or a specific key value. |
| `service>` | `([key] [key default])` | Extracts a service dependency from the active context. |
| `provide>` | `([ctx-map] [eff ctx-map] [eff body ctx-map])` | Executes effects within a scoped context map override. |
| `provide-service>` | `([k v] [eff k v] [k v eff])` | Injects a single service key-value pair into context. |
| `ensure>` | `([finalizer-eff] [eff finalizer-eff])` | Guaranteed finalizer that executes on success or failure, preserving upstream value. |
| `acquire-release>` | `([acquire use release] [use release])` | Safe resource bracket: acquires a resource, executes `use`, and guarantees `release`. |
| `retry>` | `([policy] [eff policy])` | Retries a failed effect according to a retry policy map. |
| `try>` | `([eff] [eff catch] [prev eff catch])` | Catches thrown exceptions and converts them to typed failures. |
| `if>` | `([cond-eff then-eff else-eff] ...)` | Branches execution based on a predicate effect. |
| `cond>` | `([eff & test-expr-pairs])` | Multi-branch conditional evaluating test-expression pairs. |
| `match>` | `([on-failure on-success] [eff on-failure on-success])` | Pattern-matches on both failure and success channels. |
| `or-else>` | `([fallback-eff] [eff fallback-eff])` | Recovers from any failure by executing a fallback effect. |
| `or-else-fail>` | `([failure] [eff failure])` | Replaces any upstream failure with a custom failure descriptor. |
| `all>` | `[effects]` | Evaluates a vector of independent effects and collects results into a vector. |
| `zip>` | `[eff-a eff-b]` | Combines two effects into a pair `[val-a val-b]`. |
| `zip-with>` | `[eff-a eff-b f]` | Combines two effects using a binary combining function `(f a b)`. |
| `for-each>` | `([f] [coll f])` | Iterates over items applying an effect-producing function `f`. |
| `catch>` | `([handler-map] [eff handler-map])` | Recovers from specific failures using a `{tag-key handler-eff}` map. |
| `catchall>` | `([inner-eff] [eff inner-eff])` | Recovers from any failure by passing failure map to `inner-eff`. |
| `die>` | `([] [msg] [msg data] [eff msg data])` | Terminates execution by throwing an uncatchable exception. |
| `or-die>` | `([] [on-failure] [eff on-failure])` | Converts failures into fatal uncatchable runtime exceptions. |
| `sleep>` | `([ms] [eff ms])` | Pauses execution for `ms` milliseconds. |
| `chain>` | `[prev-eff current-eff]` | Links two effects into a sequential chain. |
| `run-sync!` | `([eff] [eff context])` | Evaluates an effect pipeline synchronously with optional context. |
| `run-async!` | `([eff] [eff context])` | Evaluates an effect pipeline asynchronously (returns `CompletableFuture` on JVM / `js/Promise` on JS). |

---

### Composable Dependency & Lifecycle Layers (`fx.layer`)

| Function | Signature | Description |
|---|---|---|
| `from-value>` | `[key val]` | Creates an unmanaged layer providing a single `{key val}` entry. |
| `from-values>` | `[context-map]` | Creates an unmanaged layer providing a static `context-map`. |
| `from-effect>` | `[key effect]` | Creates an unmanaged layer providing `{key val}` by evaluating `effect`. |
| `make>` | `[key acquire-eff release-fn]` | Creates a managed layer acquiring a resource and registering `(release-fn resource) -> Effect` for cleanup. |
| `make-map>` | `[acquire-eff release-fn]` | Creates a managed layer acquiring a context map and registering `(release-fn ctx-map) -> Effect` for cleanup. |
| `merge>` | `([] [layer] [l1 l2] [l1 l2 & more])` | Horizontally combines independent layers, evaluating them and merging their context maps. |
| `compose>` | `([] [layer] [l1 l2] [l1 l2 & more])` | Vertically chains layers, providing accumulated upstream context to downstream layer acquisition. |
| `provide-layer>` | `([layer] [effect layer])` | Executes an effect pipeline within the context of `layer`, guaranteeing teardown of all acquired resources. |
| `with-layer>` | `[layer use-eff-fn]` | Executes `(use-eff-fn context)` within the context of `layer`, guaranteeing complete teardown upon completion. |
| `layer?` | `[x]` | Predicate returning true if `x` implements the `ILayer` protocol. |
| `start-layer!` | `([layer] [layer initial-context])` | Synchronously starts a layer into an active `LayerSystem` record (`Closeable`, `ILookup`, `IDeref`). |
| `stop-layer!` | `[system]` | Shuts down an active system, deterministically executing all layer finalizers in reverse acquisition order. |
| `launch-sync!` | `([layer] [layer opts])` | Starts a layer, registers JVM shutdown hooks for graceful termination, and optionally joins/blocks calling thread. |

---

### Pipeline Utilities & Metaprogramming (`fx.utils`)

| Function | Signature | Description |
|---|---|---|
| `chain-length` | `[leaf-eff]` | Returns total number of effect stages in the linear pipeline. |
| `effect-tags` | `[leaf-eff]` | Returns vector of tags in forward execution order `[:succeed :map ...]`. |
| `effect-seq` | `[leaf-eff]` | Returns vector of effect nodes in forward execution order. |
| `root-effect` | `[leaf-eff]` | Traverses upstream links to return the root effect. |
| `find-first-by-tag` | `[leaf-eff tag]` | Finds first effect node matching `tag`. |
| `find-all-by-tag` | `[leaf-eff tag]` | Returns all effect nodes matching `tag`. |
| `map-effects` | `[leaf-eff f]` | Applies pure transform `(f eff)` to every node in the pipeline. |
| `replace-by-tag` | `[leaf-eff tag replacement]` | Replaces first node matching `tag` with `replacement`. |
| `remove-by-tag` | `[leaf-eff tag]` | Removes all nodes matching `tag` and reconnects surrounding nodes. |
| `insert-after-tag` | `[leaf-eff tag new-eff]` | Slices `new-eff` immediately after the first node matching `tag`. |
| `insert-before-tag` | `[leaf-eff tag new-eff]` | Slices `new-eff` immediately before the first node matching `tag`. |
| `concat-chains` | `[chain-a chain-b]` | Concatenates two effect pipelines into a single continuous chain. |
| `slice-effects` | `[leaf-eff start end]` | Extracts a sub-pipeline between stage indices `start` and `end`. |
| `ast-seq` | `[root-eff]` | Traverses nested composite and branching AST nodes into a flat sequence. |
| `pipe>` | `[& combinators]` | Composes pipeline combinators `(Effect -> Effect)` in forward execution order. |
| `comp>` | `[& combinators]` | Composes pipeline combinators in standard mathematical right-to-left order. |
| `pipe-fx-fn>` | `[& fx-fns]` | Composes effect-producing functions (Kleisli arrows: `a -> Effect[b]`). |
| `around>` | `[before-fn after-fn]` | Wraps an effect stage with before/after actions while preserving its value. |
| `with-scoped-service>` | `[key val combinator]` | Wraps a combinator to execute inside a scoped service binding. |
| `compose-ast-passes` | `[& passes]` | Composes multiple AST rewrite passes into an optimizing compiler pipeline. |

---

## Detailed Guide & Examples

### 1. Creating Effects & Error Propagation

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
;; => #fx.core.Failure{:tag :not-found, :error-data {:user-id 123}}
```

### 2. Side Effects with `tap>` and `tap-error>`

Use `tap>` for logging, metric collection, or instrumentation on the success channel without altering the value:

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
;; => #fx.core.Failure{:tag :not-found, :error-data {:user-id 123}}
```

### 3. Guaranteed Finalization with `ensure>`

`ensure>` executes a finalizer effect after the upstream pipeline completes, guaranteed to run whether preceding steps succeeded or failed:

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
;; => #fx.core.Failure{:tag :network-error, :error-data {:url "http://example.com"}}
```

### 4. Exception Safety with `try>`

`try>` wraps operations that may throw host exceptions (JVM `Throwable` or JS error) and converts them into structured `Failure` records:

```clojure
;; Default failure tag (:try)
(-> (fx/succeed> "invalid-number")
    (fx/try> (fx/map> parse-long))
    (fx/run-sync!))
;; => #fx.core.Failure{:tag :try, :error-data #error ...}

;; Custom failure keyword
(-> (fx/succeed> "invalid-number")
    (fx/try> (fx/map> parse-long) :parse-error)
    (fx/run-sync!))
;; => #fx.core.Failure{:tag :parse-error, :error-data #error ...}

;; Fallback recovery effect
(-> (fx/succeed> "invalid-number")
    (fx/try> (fx/map> parse-long) (fx/succeed> 0))
    (fx/run-sync!))
;; => 0
```

### 5. Branching: `if>` and `cond>`

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

### 6. Batching with `all>`

`all>` runs a vector of independent effects and aggregates their values into a vector:

```clojure
(-> (fx/all> [(fx/succeed> "a")
              (fx/succeed> "b")
              (fx/succeed> "c")])
    (fx/run-sync!))
;; => ["a" "b" "c"]
```

### 7. Error Handling & Recovery

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

### 8. Dependency Injection & Context Provision

`fx` provides first-class dependency injection and environmental context propagation without mutable singletons or global state:

#### Consuming Dependencies with `service>` and `context>`

```clojure
(defn fetch-user [user-id]
  (-> (fx/service> :db)
      (fx/mapcat> (fn [db]
                    (fx/try> (fx/map> (fn [_] ((:query db) "SELECT * FROM users WHERE id = ?" user-id))))))))
```

#### Context-Aware Transformations with `map-ctx>` and `do-ctx>`

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

#### Scoped Dependency Injection with `provide>` and `provide-service>`

```clojure
(def mock-db
  {:query (fn [_sql id] {:id id :name "Test User"})})

;; Pipeline executes with injected mock dependency
(-> (fetch-user 42)
    (fx/provide> {:db mock-db})
    (fx/run-sync!))
;; => {:id 42, :name "Test User"}
```

### 9. Resource Management with `acquire-release>`

`acquire-release>` ensures resources (file handles, sockets, database transactions) are properly acquired, used, and guaranteed to be released even if exceptions or failures occur:

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

### 10. Automatic Retries with `retry>`

`retry>` re-evaluates a target effect with customizable backoff and predicate policies:

```clojure
(def resilient-fetch
  (-> (fetch-user 42)
      (fx/retry> {:max-attempts   3
                  :delay-ms       100
                  :backoff-factor 2.0
                  :retry-if       (fn [failure] (= (fx/tag failure) :timeout))})))

(fx/run-sync! resilient-fetch {:db prod-db})
```

### 11. Asynchronous Execution with `run-async!`

`run-async!` executes effect pipelines asynchronously on background thread pools (JVM `CompletableFuture` / JS `Promise`):

```clojure
(def future-res
  (-> (fx/succeed> 100)
      (fx/map> inc)
      (fx/run-async!)))

@future-res
;; => 101
```

### 12. Composable Dependency & Lifecycle Layers (`fx.layer`)

`fx.layer` provides declarative dependency injection and resource lifecycle management inspired by Effect-ts and ZIO. Layers represent blueprints for creating and tearing down environmental services (e.g. database connection pools, HTTP servers, queue listeners) with guaranteed reverse-order finalization.

#### 1. Defining Layers

Use `from-value>`, `from-values>`, or `from-effect>` for simple unmanaged services, and `make>` or `make-map>` for lifecycle-managed resources:

```clojure
(require '[fx.layer :as fx-layer])

;; Unmanaged configuration layer
(def config-layer
  (fx-layer/from-values> {:port 8080 :db-spec {:dbtype "sqlite" :dbname "app.db"}}))

;; Managed database layer: acquires connection pool, closes on teardown
(def db-layer
  (fx-layer/make> :db/pool
    (fx/try> (fn [] (create-connection-pool)) :db/pool-creation-failed)
    (fn [pool]
      (fx/try> (fn [] (.close pool)) :db/pool-close-failed))))

;; Managed HTTP server layer requiring :db/pool and :port from upstream context
(def http-server-layer
  (fx-layer/make> :http/server
    (-> (fx/service> :db/pool)
        (fx/mapcat> (fn [pool]
                      (fx/try> (fn [] (start-http-server pool 8080))
                               :http/server-start-failed))))
    (fn [server]
      (fx/try> (fn [] (.stop server)) :http/server-stop-failed))))
```

#### 2. Composing Layers (Horizontal & Vertical)

- **Horizontal Composition (`merge>`)**: Evaluates independent layers in parallel/sequence and combines their context maps.
- **Vertical Composition (`compose>`)**: Chains layers sequentially, feeding the context provided by parent layers into downstream layer constructors.

```clojure
;; App layer composed vertically: config -> db -> http-server
(def app-layer
  (fx-layer/compose>
    config-layer
    db-layer
    http-server-layer))
```

#### 3. Scoped Execution (`provide-layer>` and `with-layer>`)

`provide-layer>` evaluates an effect pipeline within the layer's environment, automatically releasing all acquired resources in reverse order upon completion (or failure/exception):

```clojure
(-> (fx/service> :http/server)
    (fx/map> (fn [srv] (str "Server running on port: " (:port srv))))
    (fx-layer/provide-layer> app-layer)
    (fx/run-sync!))
;; Automatically acquires config, db, http-server -> executes effect -> closes http-server, db
```

#### 4. Managing Application Lifecycles (`start-layer!`, `stop-layer!`, `launch-sync!`)

For long-running servers and integration test suites, manage active systems directly:

```clojure
;; Start system synchronously
(def system (fx-layer/start-layer! app-layer))

;; Access services via ILookup / deref
(:db/pool system)
(:http/server system)

;; Explicit teardown (Closeable)
(fx-layer/stop-layer! system)
;; or (.close system)

;; Entrypoint with JVM shutdown hooks and optional blocking
(defn -main [& _args]
  (fx-layer/launch-sync! app-layer {:join? true}))
```

---

## Defining Custom Effects

### 1. Composing Built-in Primitives (High-Level)

The simplest approach is composing existing constructors and combinators:

```clojure
(defn log-stage>
  ([stage-name]
   (log-stage> nil stage-name))
  ([prev-eff stage-name]
   (-> prev-eff
       (fx/tap> (fn [v] (println (str "[" stage-name "] Success:") v)))
       (fx/tap-error> (fn [err] (println (str "[" stage-name "] Failure:") (fx/tag err)))))))

(-> (fx/succeed> 100)
    (log-stage> "calculation")
    (fx/map> inc)
    (fx/run-sync!))
;; Prints: "[calculation] Success: 100"
;; => 101
```

### 2. Implementing Custom AST Effect Records (Low-Level)

For custom execution semantics, first-class AST transparency, or dedicated continuation frames in the stack-safe interpreter loop:

```clojure
(defrecord MultiplyEffect [tag prev-effect data factor]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (fx/failure? val)
        [nil val context stack]
        [nil (* val factor) context stack]))))

(defn multiply>
  ([factor]
   (multiply> nil factor))
  ([prev-eff factor]
   (->MultiplyEffect :multiply prev-eff {:factor factor} factor)))

(-> (fx/succeed> 10)
    (multiply> 5)
    (fx/map> inc)
    (fx/run-sync!))
;; => 51
```

---

## Pipeline Metaprogramming (`fx.utils`)

Because `fx` effects are 100% transparent data records, `fx.utils` allows you to inspect, query, transform, optimize, and compose effect pipelines as pure data without executing them:

```clojure
(require '[fx.utils :as fxu])

(def pipeline
  (-> (fx/service> :db)
      (fx/map> (fn [db] (str "db:" db)))
      (fx/tap> println)
      (fx/map> clojure.string/upper-case)))

(fxu/chain-length pipeline)
;; => 4

(fxu/effect-tags pipeline)
;; => [:context :map :tap :map]

;; Replace dependency lookup with a static mock in tests
(def test-pipeline
  (fxu/replace-by-tag pipeline :context (fx/succeed> "mock-postgres")))

(fx/run-sync! test-pipeline)
;; => "DB:MOCK-POSTGRES"

;; Strip out logging/telemetry stages for fast unit tests
(def silent-pipeline
  (fxu/remove-by-tag pipeline :tap))
```
