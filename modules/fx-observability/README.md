# fx/observability

Zero-dependency observability subsystem for the `fx` effect system, providing contextual structured logging, distributed
tracing, high-concurrency in-memory metrics, typed failure diagnostics, and telemetry taps.

---

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {io.github.conjurernix/fx.observability {:mvn/version "0.0.1-alpha6"}}}
;; or local module coordinate
{:deps {fx/observability {:mvn/version "0.0.1-alpha6"}}}
```

---

## Namespaces

`fx-observability` is organized into focused, single-purpose namespaces:

| Namespace                                                                                      | Responsibility                                            | Key Primitives                                                                           |
|------------------------------------------------------------------------------------------------|-----------------------------------------------------------|------------------------------------------------------------------------------------------|
| **[`fx.observability.log`](#1-structured--contextual-logging-fxobservabilitylog)**             | Structured, hierarchical logging with context propagation | `log>`, `log-info>`, `log-error>`, `annotate-logs>`, `with-logger>`                      |
| **[`fx.observability.trace`](#2-distributed-tracing--spans-fxobservabilitytrace)**             | Hierarchical tracing spans and W3C `traceparent` codec    | `with-span>`, `with-span-attributes>`, `extract-trace-context>`, `inject-trace-context>` |
| **[`fx.observability.metrics`](#3-in-memory-metrics-fxobservabilitymetrics)**                  | Lock-free concurrent counters, gauges, and latency timers | `metric-counter`, `metric-timer`, `track-duration>`, `metrics-snapshot!`                 |
| **[`fx.observability.diagnostics`](#4-diagnostics--cause-algebra-fxobservabilitydiagnostics)** | Typed Cause algebra, sandboxing, and AST trace renderers  | `Cause`, `sandbox>`, `unsandbox>`, `render-cause`, `render-execution-trace`              |
| **[`fx.observability.telemetry`](#5-telemetry-taps-fxobservabilitytelemetry)**                 | Non-intrusive event taps and telemetry sink routing       | `tap-telemetry>`, `with-telemetry-sink>`, `tap-telemetry-event>`                         |

---

## 1. Structured & Contextual Logging (`fx.observability.log`)

Logging in `fx` is purely contextual: log metadata and annotations propagate immutably through the execution context
across asynchronous boundaries and nested pipelines without `ThreadLocal` leaks.

```clojure
(ns example.logging
  (:require [fx.core :as fx]
            [fx.observability.log :as log]))

(defn process-payment [user-id amount]
  (-> (log/annotate-logs> {:user-id user-id :operation "process-payment"})
      (fx/mapcat> (fn [_] (log/log-info> "Initiating payment" {:amount amount})))
      (fx/mapcat> (fn [_] (charge-card> user-id amount)))
      (fx/tap-error> (fn [err] (log/log-error> "Payment failed" {:error err})))))
```

### Log Annotations

`annotate-logs>` merges structured key-value pairs into `:fx.observability/log-annotations` in the execution context.
All downstream steps and nested pipelines automatically inherit these annotations in their log output.

### Pluggable Sinks

- `console-logger-sink`: Default standard output/error sink.
- `system-logger-sink`: Delegates to standard `java.lang.System.Logger` (JVM) or console (JS).
- Custom Sinks: `(with-logger> custom-sink-fn eff)`.

---

## 2. Distributed Tracing & Spans (`fx.observability.trace`)

Tracks execution spans with nanosecond precision, nested parent-child hierarchies, and W3C Trace Context (`traceparent`)
interoperability for distributed microservices.

```clojure
(ns example.tracing
  (:require [fx.core :as fx]
            [fx.observability.trace :as trace]))

(defn handle-order-request [order-payload]
  (-> (trace/with-span> "orders.create-order" {:env "prod"}
                        (-> (validate-order> order-payload)
                            (fx/mapcat> (fn [order]
                                          (trace/with-span> "orders.persist"
                                                            (save-order-to-db> order))))
                            (fx/mapcat> (fn [order]
                                          (trace/with-span> "orders.notify"
                                                            (notify-user> order))))))
      (trace/with-span-reporter> (fn [span] (println "Span finished:" (:name span) (:duration-ms span) "ms")))
      (fx/run-sync!)))
```

### W3C Trace Context Codec

- `(extract-trace-context carrier-map)`: Parses inbound W3C `traceparent` headers.
- `(inject-trace-context carrier-map trace-ctx)`: Formats and injects active `traceparent` into outbound carriers.
- `extract-trace-context>` & `inject-trace-context>`: Pure effect combinators.

---

## 3. In-Memory Metrics (`fx.observability.metrics`)

High-performance, lock-free concurrent counters, gauges, and latency timers powered by JVM atomics (`LongAdder`,
`DoubleAdder`, `AtomicReference`).

```clojure
(ns example.metrics
  (:require [fx.core :as fx]
            [fx.observability.metrics :as metrics]))

(def http-requests-counter (metrics/metric-counter "http.requests.total" {:route "/api/orders"}))
(def db-query-timer (metrics/metric-timer "db.query.duration" {:table "orders"}))
(def db-pool-gauge (metrics/metric-gauge "db.pool.active" {:pool "main"}))

(defn handle-request [req]
  (-> (metrics/track-duration> db-query-timer
                               (metrics/track-success-count> http-requests-counter
                                                             (query-database> req)))
      (fx/run-sync!)))

;; Query metrics at runtime
(metrics/metrics-snapshot!)
;; =>
;; {:counters {"http.requests.total" {:tags {:route "/api/orders"} :value 1420}}
;;  :gauges   {"db.pool.active"       {:tags {:pool "main"} :value 8}}
;;  :timers   {"db.query.duration"    {:tags {:table "orders"} :count 1420 :sum-ms 3550.0 :min-ms 1.2 :max-ms 45.0 :avg-ms 2.5}}}
```

---

## 4. Diagnostics & Cause Algebra (`fx.observability.diagnostics`)

Distinguishes between typed domain failures (`Fail`) and unhandled unexpected defects (`Die`/exceptions). Sandboxes
failures onto the success channel for compositional inspection and provides visual ASCII tree formatters.

```clojure
(ns example.diagnostics
  (:require [fx.core :as fx]
            [fx.observability.diagnostics :as diag]))

;; Sandboxing both domain errors and JVM runtime defects
(-> (risky-db-query>)
    (diag/sandbox>)
    (fx/map> (fn [cause-or-val]
               (if (diag/cause? cause-or-val)
                 (do
                   (println (diag/render-cause cause-or-val))
                   (fallback-value))
                 cause-or-val)))
    (fx/run-sync!))
```

### AST Execution Tracing

Reconstructs the linear pipeline steps leading to a failure using `render-execution-trace`:

```
[fx AST Execution Trace]
  1. (succeed> {:user-id 42})
  2. (annotate-logs> {:req-id "req-1"})
  3. (with-span> {:name "fetch-user"})
  4. (mapcat> fetch-user-data)
  └── FAILED: [:user/not-found] {:user-id 42}
```

---

## 5. Telemetry Taps (`fx.observability.telemetry`)

Non-intrusive event interceptors for auditing, logging, and routing pipeline metrics to event listeners.

```clojure
(ns example.telemetry
  (:require [fx.core :as fx]
            [fx.observability.telemetry :as telemetry]))

(-> (telemetry/with-telemetry-sink> (fn [evt] (println "Observed event:" evt))
                                    (telemetry/tap-telemetry> :payment/execute {:order-id "ord-1"}
                                                              (charge-card> "ord-1" 100)))
    (fx/run-sync!))
```

---

## Observability Layers (`fx.layer`)

All observability components can be managed and composed as declarative `fx.layer` recipes:

```clojure
(ns example.system
  (:require [fx.core :as fx]
            [fx.layer :as fx-layer]
            [fx.observability.log :as log]
            [fx.observability.metrics :as metrics]
            [fx.observability.telemetry :as telemetry]
            [fx.observability.trace :as trace]))

(def observability-layers
  (fx-layer/merge>
    (metrics/metrics-layer>)
    (log/logger-layer> {:logger log/console-logger-sink :level :info})
    (trace/span-reporter-layer> (fn [span] (println "Span finished:" (:name span))))
    (telemetry/telemetry-sink-layer> (fn [evt] (println "Telemetry event:" (:event evt))))))

(defn -main []
  (fx-layer/launch-sync!
    (fx-layer/compose>
      observability-layers
      (my-application-layer>))))
```
