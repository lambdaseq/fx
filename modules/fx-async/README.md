# fx.async

A first-class structured concurrency and async coordination module for the `fx` effect system.

## Features

- **Lightweight Fibers & Structured Concurrency**: Fork, join, interrupt, and inspect fibers with cascading cancellation and resource unwinding safety.
- **Structured Parallel Combinators**: `race>`, `all-par>`, and `map-par>` with configurable bounded concurrency (`{:concurrency n}`) and fail-fast options.
- **Pluggable Execution Engines**: Scope pipelines to Java Virtual Threads (`with-virtual-threads>`), platform thread pools (`with-thread-pool>`), or `core.async` go-routines (`with-go>`).
- **`core.async` Channel Bridging**: `chan>`, `chan-put>`, `chan-take>`, `chan-alts>`, `chan-drain>`, `chan-pipe>`, `chan-close>`, `chan-pub>`, `chan-sub>`.
- **Coordination & In-Memory Messaging**: Bounded/sliding/dropping Queues (`queue-bounded>`, `queue-sliding>`, `queue-dropping>`), broadcast Hubs (`hub-bounded>`, `hub-publish>`, `hub-subscribe>`), Deferreds (`deferred>`), Semaphores (`semaphore>`, `with-permit>`), Countdown Latches (`countdown-latch>`), and atomic state cells (`ref>`).

## Usage

```clojure
(require '[fx.core :as fx]
         '[fx.async :as fxa])

;; Parallel computation with bounded concurrency
(fx/run-sync!
  (fxa/map-par>
    (fn [id] (fetch-data> id))
    item-ids
    {:concurrency 4}))

;; Structured racing with automatic cancellation of slower branches
(fx/run-sync!
  (fxa/race>
    [(fetch-from-primary-cache>)
     (fetch-from-fallback-service>)]))

;; Resource-safe fiber fork & join
(fx/run-sync!
  (-> (expensive-computation>)
      (fxa/fork>)
      (fx/mapcat> (fn [fiber] (fxa/join> fiber)))))
```
