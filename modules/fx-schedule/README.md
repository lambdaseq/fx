# fx/schedule

Purely functional, composable schedule and resilience algebra for the `fx` effect system. Inspired by `zio.Schedule` and `effect/Schedule`, `fx.schedule` models recurrence policies, retry strategies, and resilience patterns as transparent, immutable data structures composed via Clojure's thread-first (`->`) macro.

---

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {io.github.conjurernix/fx.schedule {:mvn/version "0.1.0"}}}
;; or local module coordinate
{:deps {fx/schedule {:mvn/version "0.1.0"}}}
```

---

## Core Mental Model

A schedule in `fx.schedule` is an immutable state machine implementing the `ISchedule` protocol:
1. **Initial State**: `(-initial-state schedule)` returns the initial state.
2. **Step Evaluation**: `(-step schedule state now input)` transitions the state machine given current timestamp `now` and `input` (such as a failure or value).
3. **Transition Result**: Returns a map:
   ```clojure
   {:decision :recur | :halt
    :delay-ms <long>
    :state    <any>
    :out      <any>}
   ```

Because state transitions are purely functional and accept an explicit `now` timestamp, schedules are deterministic, easily composed, and completely testable without sleeping.

---

## Quickstart

```clojure
(ns example.resilient-service
  (:require [fx.core :as fx]
            [fx.schedule :as sched]))

;; Define a composable retry policy with exponential backoff and jitter capped at 5 retries
(def retry-policy
  (-> (sched/exponential-backoff> {:initial-ms 100 :factor 2.0 :max-ms 5000})
      (sched/jitter> 0.1)
      (sched/intersect> (sched/recur-n> 5))
      (sched/while-tag> :http/connection-error)))

;; Apply to an effect
(defn resilient-fetch> [url]
  (-> (http-get> url)
      (sched/retry-schedule> retry-policy)))
```

---

## Constructors

| Constructor | Description | Default Output |
|---|---|---|
| `(forever>)` | Recurs indefinitely with 0 delay | Completed recurrence count |
| `(once>)` | Recurs exactly once with 0 delay | Completed recurrence count |
| `(recur-n> n)` | Recurs at most `n` times with 0 delay | Completed recurrence count |
| `(fixed> delay-ms)` | Recurs indefinitely with constant `delay-ms` interval | Completed recurrence count |
| `(linear-backoff> opts)` | Linearly increasing delay (`(* initial-ms attempt)`) | Computed `delay-ms` |
| `(exponential-backoff> opts)` | Exponential backoff (`(* initial-ms (pow factor attempt))`) | Computed `delay-ms` |
| `(fibonacci-backoff> opts)` | Fibonacci-spaced delay backoff | Computed `delay-ms` |
| `(elapsed> max-duration-ms)` | Recurs while total elapsed time is under `max-duration-ms` | Elapsed milliseconds |

---

## Combinators

### Composition
- **`intersect>`**: `(intersect> sched-a sched-b)` - Runs both schedules in lockstep. Recurs while **both** continue; uses the maximum delay of both (`(max delay-a delay-b)`).
- **`union>`**: `(union> sched-a sched-b)` - Runs both schedules in lockstep. Recurs while **either** continues; uses the minimum delay of active schedules (`(min delay-a delay-b)`).
- **`and-then>`**: `(and-then> sched-a sched-b)` - Evaluates `sched-a` to exhaustion, then seamlessly continues with `sched-b`.

### Timing & Delay Modifiers
- **`jitter>`**: `(jitter> schedule factor)` - Applies randomized jitter `[(- 1.0 factor), (+ 1.0 factor)]` to delays.
- **`modify-delay>`**: `(modify-delay> schedule f)` - Purely transforms computed delay using `(f delay-ms state)`.

### Conditional Filtering
- **`while-input>`**: `(while-input> schedule pred)` - Recurs only while `(pred input)` is truthy.
- **`until-input>`**: `(until-input> schedule pred)` - Recurs until `(pred input)` is truthy.
- **`while-output>`**: `(while-output> schedule pred)` - Recurs only while `(pred out)` is truthy.
- **`until-output>`**: `(until-output> schedule pred)` - Recurs until `(pred out)` is truthy.
- **`while-tag>`**: `(while-tag> schedule tag-key)` - Convenience filter to retry only on matching failure `:tag` (keyword or set).
- **`until-tag>`**: `(until-tag> schedule tag-key)` - Convenience filter to stop retries on matching failure `:tag`.

### Inspection & Output Transformation
- **`map-output>`**: `(map-output> schedule f)` - Transforms step output value with `(f out)`.
- **`tap-step>`**: `(tap-step> schedule tap-fn)` - Side-effect callback `(tap-fn {:decision d :delay-ms ms :input in :out out :state st})`.

---

## Effect Integrations

### 1. `retry-schedule>`
Retries an effect on failure according to an `ISchedule`. Evaluates the upstream effect, inspects any `IFailure`, steps the schedule, pauses for the computed delay, and re-executes until the schedule halts or the effect succeeds.

```clojure
(-> (fetch-data> url)
    (sched/retry-schedule> (-> (sched/exponential-backoff> {:initial-ms 50})
                               (sched/intersect> (sched/recur-n> 3)))))
```

### 2. `repeat-schedule>`
Repeats an effect on success according to an `ISchedule`. Evaluates the upstream effect, inspects the successful value, steps the schedule, pauses for the computed delay, and re-executes until the schedule halts or the effect fails.

```clojure
(-> (poll-job-status> job-id)
    (sched/repeat-schedule> (-> (sched/fixed> 1000)
                                (sched/until-input> (fn [status] (= (:state status) :completed))))))
```

---

## Resilience Primitives

### Circuit Breaker (`circuit-breaker>`)
Protects downstream dependencies from cascading failures by tracking consecutive errors and failing fast when open.

- **States**: `:closed` (normal), `:open` (fast-fail), `:half-open` (canary probe).
- **Options**:
  - `:failure-threshold`: consecutive failure count before opening (default: `3`).
  - `:reset-timeout-ms`: cooldown period in milliseconds before attempting half-open probe (default: `5000`).
  - `:trip-on?`: predicate on `IFailure` deciding if it counts toward tripping.
  - `:on-state-change`: callback `(fn [old-state new-state])`.

```clojure
(def payment-breaker (sched/make-circuit-breaker {:failure-threshold 5 :reset-timeout-ms 10000}))

(defn charge-card> [user-id amount]
  (-> (call-payment-gateway> user-id amount)
      (sched/circuit-breaker> payment-breaker)))
```

### Rate Limiter (`rate-limiter>`)
Token-bucket rate limiter for controlling request throughput.

- **Options**:
  - `:limit`: token capacity / burst limit (default: `10`).
  - `:interval-ms`: replenishment window in milliseconds (default: `1000`).

```clojure
(def api-limiter (sched/make-rate-limiter {:limit 50 :interval-ms 1000}))

(defn call-external-api> [payload]
  (-> (post-json> payload)
      (sched/rate-limiter> api-limiter)))
```
