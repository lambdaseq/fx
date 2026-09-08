# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]
### Added
- **Zero-Dependency Observability Subsystem (`fx.observability`)**:
  - Contextual structured logging (`fx.observability.log`) with log levels (`log-debug>`, `log-info>`, `log-warn>`, `log-error>`), log annotation propagation (`annotate-logs>`), and pluggable sinks (`console-logger-sink`, `system-logger-sink`, `with-logger>`).
  - Distributed tracing & spans (`fx.observability.trace`) with hierarchical span lifecycles (`with-span>`), dynamic span attributes (`with-span-attributes>`), and W3C Trace Context codec (`extract-trace-context>`, `inject-trace-context>`, `format-traceparent`, `parse-traceparent`).
  - Concurrent in-memory metrics (`fx.observability.metrics`) with lock-free atomic counters, gauges, latency timers (`LongAdder`, `DoubleAdder`), execution trackers (`track-duration>`, `track-success-count>`, `track-failure-count>`), and snapshot queries (`metrics-snapshot!`).
  - Typed failure diagnostics & Cause algebra (`fx.observability.diagnostics`) with `Cause` hierarchy (`Fail`, `Die`, `Interrupt`, `Sequential`, `Parallel`), sandboxing (`sandbox>`, `unsandbox>`), and ASCII visual tree renderers (`render-cause`, `render-execution-trace`).
  - Telemetry taps (`fx.observability.telemetry`) for non-intrusive event observation and sink routing.
  - Unified module facade (`fx.observability`) re-exporting canonical combinators.
- **Todo Example Application (`example/`)**:
  - Full-stack REST API showcasing `fx.core`, `fx.jdbc`, `fx.ring`, and `fx.observability` with Reitit, Muuntaja, HoneySQL v2, and shared in-memory SQLite.
  - Interactive HTTP request suite (`test.http`), automated integration tests, and `/api/metrics` JSON snapshot endpoint.

## [0.2.0] - 2026-09-04
### Added
- **100% Transparent Data AST**: Refactored all effect constructors in `fx.core` into dedicated `defrecord` types implementing `ITagged` and `IEffect`.
- **Protocol-Driven Continuation Interpreter**: Implemented symmetric, stack-safe interpreter loop in `run-sync!` and `run-async!` with heap-allocated continuation frames (`IContinuation`, `IUnwindable`).
- **Resource Management**: Added `acquire-release>` combinator for safe acquisition and guaranteed resource cleanup.
- **Resilience**: Added `retry>` combinator with customizable retry attempts, backoff factors, and predicate policies.
- **Branching & Matching**: Added `match>`, `or-else>`, `or-else-fail>`, `zip>`, `zip-with>`, `for-each>`, `die>`, and `or-die>`.
- **Async Execution**: Added `run-async!` returning `CompletableFuture` (JVM) / `js/Promise` (JS) with explicit context propagation.
- **Pipeline Utilities & Metaprogramming (`fx.utils`)**: Added traversal, querying, AST rewriting, splicing, and combinator composition (`pipe>`, `comp>`, `pipe-fx-fn>`, `around>`, `with-scoped-service>`, `compose-ast-passes`).

### Changed
- **Monomorphic Combinators**: Enforced strict parameter separation between pure functions and `Effect` instances across all combinators.
- **Pure Context Threading**: Replaced dynamic var `*context*` with pure, explicit context threading across runners and execution steps.

### Removed
- Removed polymorphic runtime coercion helper `eval-eff-or-fn` and dynamic `ArityException` catching.

## [0.1.1] - 2024-08-30
### Changed
- Documentation on how to make the widgets.

### Removed
- `make-widget-sync` - we're all async, all the time.

### Fixed
- Fixed widget maker to keep working when daylight savings switches over.

## 0.1.0 - 2024-08-30
### Added
- Files from the new template.
- Widget maker public API - `make-widget-sync`.

[Unreleased]: https://github.com/fx/fx/compare/0.1.1...HEAD
[0.1.1]: https://github.com/fx/fx/compare/0.1.0...0.1.1
