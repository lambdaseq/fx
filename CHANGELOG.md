# Change Log
All notable changes to this project will be documented in this file. This change log follows the conventions of [keepachangelog.com](http://keepachangelog.com/).

## [0.1.0] - 2026-09-12
### Added
- Published all eight modules in lockstep at `0.1.0`.
- Added the transparent AST runtime, stack-safe synchronous and asynchronous interpreters, resource lifecycle combinators, failure channels, dependency injection, and pipeline utilities.
- Added schedule, JDBC, Ring, HTTP client, observability, typed Clojure, and structured concurrency modules.
- Added the Todo example application with integration tests and runnable module demonstrations.

### Changed
- Removed the obsolete `fx.ring/wrap-fx-failure` synonym. Use `wrap-fx-failures`.
- Standardized release metadata, module coordinates, and documentation on `0.1.0`.

[Unreleased]: https://github.com/lambdaseq/fx/compare/v0.1.0...HEAD
[0.1.0]: https://github.com/lambdaseq/fx/releases/tag/v0.1.0
