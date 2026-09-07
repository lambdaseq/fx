# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [Unreleased]

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
