# fx

A lightweight, purely functional, and modular effect system for Clojure and ClojureScript inspired by Effect-ts and ZIO.

`fx` models synchronous and asynchronous computations, resource lifecycles, dependency injection, and failure channels as pure, transparent data structures composed with Clojure's standard thread-first (`->`) macro. Pipelines short-circuit automatically on failure, manage resources safely, execute in constant stack space, and evaluate only when explicitly triggered at application boundaries.

---

## Modular Architecture

`fx` is engineered as a cohesive suite of modular libraries. Each module addresses a specific layer of application development while sharing the same underlying data-driven runtime and execution semantics:

| Module | Coordinate | Description | Documentation |
|---|---|---|---|
| **[`fx.core`](modules/fx-core)** | `io.github.conjurernix/fx.core` | Foundational effect runtime, AST records, standard combinators, failure channels, context DI, and metaprogramming (`fx.utils`). | [Read Core Docs →](modules/fx-core/README.md) |
| **[`fx.typed`](modules/fx-typed)** | `io.github.conjurernix/fx.typed` | Typed Clojure annotations with full variance tracking across input, output, failure, and context channels. | [Read Typed Docs →](modules/fx-typed/README.md) |
| **[`fx.jdbc`](modules/fx-jdbc)** | `io.github.conjurernix/fx.jdbc` | Functional JDBC database access and connection pooling with automatic dual-failure transaction rollback semantics. | [Read JDBC Docs →](modules/fx-jdbc/README.md) |
| **[`fx.ring`](modules/fx-ring)** | `io.github.conjurernix/fx.ring` | Ring HTTP middleware and response combinators for 1-arity synchronous and 3-arity asynchronous web handlers. | [Read Ring Docs →](modules/fx-ring/README.md) |

---

## Core Philosophy & Mental Model

- **Programs as Data**: Workflows are represented as transparent, immutable data records rather than opaque closures. Pipelines can be inspected, queried, transformed, and mocked with pure functions before execution.
- **Execution at the Edge**: Pipelines remain completely inert until explicitly evaluated with `fx/run-sync!` or `fx/run-async!`.
- **Standard Clojure Threading (`->`)**: Effects chain sequentially using Clojure's familiar thread-first macro (`->`), transforming values step by step.
- **Explicit Success and Failure Channels**: Computations yield either a successful value or a structured `Failure` record (with a `:tag` and error payload). Unhandled failures short-circuit downstream steps automatically.
- **Pure Dependency Injection**: Effects declare environmental dependencies via `service>` or `context>`. Dependencies are injected at runtime via `provide>` or `run-sync!`, eliminating global state and mutable singletons.
- **Deterministic Resource Safety**: Primitives like `acquire-release>` and `ensure>` guarantee resource cleanup under both success and failure conditions.
- **Stack Safe Execution**: The runtime executes chains iteratively via a heap-allocated continuation stack, preventing `StackOverflowError` regardless of pipeline depth.

---

## Quickstart

Effect constructors and pipeline combinators end in `>`, runtime execution runners end in `!`, and predicates end in `?`.

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

---

## Module Overview

### 1. `fx.core` — The Foundational Runtime

The core module provides the primitives to build, compose, and execute pure effect workflows.

```clojure
(ns example.workflow
  (:require [fx.core :as fx]))

(defn fetch-user [user-id]
  (-> (fx/service> :user-db)
      (fx/mapcat> (fn [db]
                    (if-let [user (get db user-id)]
                      (fx/succeed> user)
                      (fx/fail> :user/not-found {:id user-id}))))
      (fx/catch>
        {:user/not-found (fx/map> (fn [{:keys [id]}] {:id id :name "Guest"}))})))

(fx/run-sync! (fetch-user 42) {:user-db {1 {:id 1 :name "Alice"}}})
;; => {:id 42, :name "Guest"}
```

👉 **[Detailed `fx.core` Documentation & API Reference →](modules/fx-core/README.md)**

---

### 2. `fx.typed` — Static Typing with Typed Clojure

`fx.typed` provides full type annotations for Typed Clojure, modeling effects as a 4-parameter type constructor `(fx/IEffect In Out Failure Context)` with rigorous variance guarantees.

```clojure
(ns example.typed
  (:require [fx.core :as fx]
            [fx.typed]
            [typed.clojure :as t]))

(t/ann calculate-tax
       (fx/IEffect t/Any Double (t/Option (fx/IFailure (t/Val :tax/invalid-amount) String)) '{}))

(def calculate-tax
  (-> (fx/succeed> 100.0)
      (fx/mapcat> (fn [amt]
                    (if (pos? amt)
                      (fx/succeed> (* amt 0.2))
                      (fx/fail> :tax/invalid-amount "Amount must be positive"))))))
```

👉 **[Detailed `fx.typed` Documentation & Type Contracts →](modules/fx-typed/README.md)**

---

### 3. `fx.jdbc` — Purely Functional Database Access

`fx.jdbc` bridges relational database operations into effect pipelines using `next.jdbc`. It provides automatic connection pooling, statement caching, streaming reductions with `plan!>`, and atomic transactions that automatically roll back on exceptions or functional `IFailure` values.

```clojure
(ns example.db
  (:require [fx.core :as fx]
            [fx.jdbc :as fx-jdbc]
            [fx.jdbc.sql :as sql]))

(defn transfer-balance [from-id to-id amount]
  (fx-jdbc/with-transaction>
    (fn [_conn]
      (-> (sql/get-by-id!> :accounts from-id)
          (fx/mapcat> (fn [sender]
                        (if (< (:balance sender) amount)
                          (fx/fail> :transfer/insufficient-funds {:balance (:balance sender)})
                          (-> (sql/update!> :accounts {:balance (- (:balance sender) amount)} {:id from-id})
                              (fx/mapcat> (fn [_] (sql/get-by-id!> :accounts to-id)))
                              (fx/mapcat> (fn [receiver]
                                            (sql/update!> :accounts {:balance (+ (:balance receiver) amount)} {:id to-id})))))))))))
```

👉 **[Detailed `fx.jdbc` Documentation & SQL Combinators →](modules/fx-jdbc/README.md)**

---

### 4. `fx.ring` — Declarative Ring Web Services

`fx.ring` integrates pure effect pipelines into Ring HTTP servers. It translates effect handlers (`req -> effect`) into 1-arity synchronous or 3-arity asynchronous Ring handlers, binds incoming requests to ambient context, and automatically maps failure channels into appropriate HTTP status responses.

```clojure
(ns example.web
  (:require [fx.core :as fx]
            [fx.ring :as fx-ring]
            [fx.ring.response :as fx-resp]))

(defn greet-handler [req]
  (if-let [name (get-in req [:params :name])]
    (fx-resp/ok> (str "Hello, " name "!"))
    (fx/fail> :bad-request {:message "Missing name parameter"})))

(def app
  (fx-ring/wrap-fx greet-handler
    {:failure-map {:bad-request (fn [err req] {:status 400 :body (:message err)})}}))
```

👉 **[Detailed `fx.ring` Documentation & Response Combinators →](modules/fx-ring/README.md)**

---

## Installation

Add the necessary modules to your `deps.edn`:

```clojure
{:deps
 {;; Foundational effect system
  io.github.conjurernix/fx.core  {:mvn/version "0.0.1-alpha"}

  ;; Optional Typed Clojure support
  io.github.conjurernix/fx.typed {:mvn/version "0.0.1-alpha"}

  ;; Optional JDBC database support
  io.github.conjurernix/fx.jdbc  {:mvn/version "0.0.1-alpha"}

  ;; Optional Ring HTTP support
  io.github.conjurernix/fx.ring  {:mvn/version "0.0.1-alpha"}}}
```

---

## Development & Build Orchestration

The project includes build and verification tools configured in `build.clj`:

```bash
# Run test suite across all modules
clojure -T:build test

# Build JARs for all modules
clojure -T:build jar

# Install all modules to local Maven cache (~/.m2)
clojure -T:build install

# Run full CI pipeline (clean, build JARs, run tests)
clojure -T:build ci
```

---

## License

Copyright © 2024 LambdaSeq Works LTD.

Distributed under the Eclipse Public License version 1.0.
