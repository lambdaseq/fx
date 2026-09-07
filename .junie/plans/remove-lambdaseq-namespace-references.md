---
sessionId: session-260908-001634-ui4j
---

# Requirements

### Overview & Goals
The project previously used the group prefix `com.lambdaseq` across namespaces (e.g., `com.lambdaseq.fx.core`), file paths (`src/com/lambdaseq/fx/...`), dependency coordinates (`com.lambdaseq/fx-core`), and documentation. The goal of this task is to completely remove all mentions of `com.lambdaseq` from namespaces, directory structures, source code, tests, build configurations, and documentation, standardizing on concise `fx.*` namespaces (e.g., `fx.core`, `fx.jdbc`, `fx.ring`, `fx.typed`) and `fx/*` library names.

### Scope

#### In Scope
- **Source code namespaces**: Refactor all Clojure/ClojureScript source files to use root `fx.*` namespaces (`fx.core`, `fx.utils`, `fx.jdbc`, `fx.jdbc.sql`, `fx.ring`, `fx.ring.response`, `fx.typed`).
- **File directory relocation**: Move source and test files from `com/lambdaseq/fx/` paths to `fx/` paths across all 4 modules (`fx-core`, `fx-jdbc`, `fx-ring`, `fx-typed`).
- **Inter-module dependencies**: Update `deps.edn` in `fx-jdbc` and `fx-ring` to depend on `fx/core` instead of `com.lambdaseq/fx-core`.
- **Test suites**: Update all test namespaces, requires, and Typed Clojure inline test expressions in `modules/fx-typed/test/fx/typed_test.cljc`.
- **Build configuration**: Update `build.clj` and `pom.xml`.
- **Documentation**: Update `README.md`, `CHANGELOG.md`, `doc/intro.md`, `modules/fx-jdbc/README.md`, `modules/fx-ring/README.md`, and plan files.

#### Out of Scope
- Modifying the public function API names, combinator behavior, or runtime semantics.
- Adding new feature dependencies or modifying external third-party library versions.

# Technical Design

### Current Implementation
The repository contains four submodules (`fx-core`, `fx-jdbc`, `fx-ring`, `fx-typed`) under `modules/`. Currently, all namespaces are prefixed with `com.lambdaseq.fx.*`:
- Source files reside in `modules/<module>/src/com/lambdaseq/fx/`
- Test files reside in `modules/<module>/test/com/lambdaseq/fx/`
- `modules/fx-jdbc/deps.edn` and `modules/fx-ring/deps.edn` refer to `com.lambdaseq/fx-core`
- `modules/fx-typed/test/com/lambdaseq/fx/typed_test.cljc` contains multiple inline `:requires [[com.lambdaseq.fx.core :as fx] [com.lambdaseq.fx.typed]]` clauses in `is-tc-e` type checker test macros
- Qualified context keys such as `:com.lambdaseq.fx.jdbc/datasource` and `:com.lambdaseq.fx.ring/request` are defined using the old namespace prefix.

### Key Decisions
- **Canonical Namespace Hierarchy**: Use `fx.<module>` (e.g., `fx.core`, `fx.utils`, `fx.jdbc`, `fx.jdbc.sql`, `fx.ring`, `fx.ring.response`, `fx.typed`).
- **Context Keys**: Standardize context keys to use `:fx.jdbc/datasource` and `:fx.ring/request` to align with the simplified namespace structure.
- **Directory Structure**: Migrate `src/com/lambdaseq/fx/` -> `src/fx/` and `test/com/lambdaseq/fx/` -> `test/fx/` according to standard Clojure classpath resolution rules.

### File Structure & Namespace Mapping

| Module | Old Path & Namespace | New Path & Namespace |
|---|---|---|
| `fx-core` | `src/com/lambdaseq/fx/core.cljc` (`com.lambdaseq.fx.core`) | `src/fx/core.cljc` (`fx.core`) |
| `fx-core` | `src/com/lambdaseq/fx/utils.cljc` (`com.lambdaseq.fx.utils`) | `src/fx/utils.cljc` (`fx.utils`) |
| `fx-core` | `test/com/lambdaseq/fx/core_test.cljc` (`com.lambdaseq.fx.core-test`) | `test/fx/core_test.cljc` (`fx.core-test`) |
| `fx-core` | `test/com/lambdaseq/fx/utils_test.cljc` (`com.lambdaseq.fx.utils-test`) | `test/fx/utils_test.cljc` (`fx.utils-test`) |
| `fx-jdbc` | `src/com/lambdaseq/fx/jdbc.clj` (`com.lambdaseq.fx.jdbc`) | `src/fx/jdbc.clj` (`fx.jdbc`) |
| `fx-jdbc` | `src/com/lambdaseq/fx/jdbc/sql.clj` (`com.lambdaseq.fx.jdbc.sql`) | `src/fx/jdbc/sql.clj` (`fx.jdbc.sql`) |
| `fx-jdbc` | `test/com/lambdaseq/fx/jdbc_test.clj` (`com.lambdaseq.fx.jdbc-test`) | `test/fx/jdbc_test.clj` (`fx.jdbc-test`) |
| `fx-ring` | `src/com/lambdaseq/fx/ring.clj` (`com.lambdaseq.fx.ring`) | `src/fx/ring.clj` (`fx.ring`) |
| `fx-ring` | `src/com/lambdaseq/fx/ring/response.clj` (`com.lambdaseq.fx.ring.response`) | `src/fx/ring/response.clj` (`fx.ring.response`) |
| `fx-ring` | `test/com/lambdaseq/fx/ring_test.clj` (`com.lambdaseq.fx.ring-test`) | `test/fx/ring_test.clj` (`fx.ring-test`) |
| `fx-typed` | `src/com/lambdaseq/fx/typed.cljc` (`com.lambdaseq.fx.typed`) | `src/fx/typed.cljc` (`fx.typed`) |
| `fx-typed` | `test/com/lambdaseq/fx/typed_test.cljc` (`com.lambdaseq.fx.typed-test`) | `test/fx/typed_test.cljc` (`fx.typed-test`) |

### Risks & Mitigations
- **Typed Clojure test runner failures**: `modules/fx-typed/test/com/lambdaseq/fx/typed_test.cljc` has numerous explicit `:requires` options passed to `is-tc-e`.
  - *Mitigation*: Ensure every inline type-checker require map is updated to `[fx.core :as fx]` and `[fx.typed]`.
- **JVM Type Instances**: In `fx.core`, predicates like `effect?` and `failure?` check `(instance? fx.core.IEffect x)` and `(instance? fx.core.IFailure x)`.
  - *Mitigation*: Ensure Java class name references in Clojure interop match the new `fx.core.IEffect` and `fx.core.IFailure` generated class names.
- **Orphan empty folders**: Moving files may leave empty `src/com/` and `test/com/` directories.
  - *Mitigation*: Explicitly remove obsolete directory trees after file relocation.

# Testing

### Validation Approach
Verification will be performed directly using Clojure's test runner and repository search tooling:

1. **Automated Test Execution**:
   - Run the test suite across all modules:
     ```bash
     clojure -M:dev:test -m cognitect.test-runner -d modules/fx-core/test -d modules/fx-typed/test -d modules/fx-jdbc/test -d modules/fx-ring/test
     ```
   - Run the tools.build test alias:
     ```bash
     clojure -T:build test
     ```
   - Verify that all 90 tests and 535+ assertions in core, utils, jdbc, ring, and typed modules pass with 0 errors and 0 failures.

2. **Repository-Wide String Search**:
   - Perform a full repository grep for `com.lambdaseq` and `lambdaseq` to verify zero residual references remain in code, comments, config files, or documentation.

# Delivery Steps

### ✓ Step 1: Refactor core source namespaces, file paths, and module dependencies
All source files across `fx-core`, `fx-jdbc`, `fx-ring`, and `fx-typed` are located under standard `src/fx/` directory structures with `fx.*` namespace declarations and updated dependency coordinates.

- Relocate source files from `modules/*/src/com/lambdaseq/fx/...` to `modules/*/src/fx/...`:
  - `modules/fx-core/src/com/lambdaseq/fx/core.cljc` -> `modules/fx-core/src/fx/core.cljc`
  - `modules/fx-core/src/com/lambdaseq/fx/utils.cljc` -> `modules/fx-core/src/fx/utils.cljc`
  - `modules/fx-jdbc/src/com/lambdaseq/fx/jdbc.clj` -> `modules/fx-jdbc/src/fx/jdbc.clj`
  - `modules/fx-jdbc/src/com/lambdaseq/fx/jdbc/sql.clj` -> `modules/fx-jdbc/src/fx/jdbc/sql.clj`
  - `modules/fx-ring/src/com/lambdaseq/fx/ring.clj` -> `modules/fx-ring/src/fx/ring.clj`
  - `modules/fx-ring/src/com/lambdaseq/fx/ring/response.clj` -> `modules/fx-ring/src/fx/ring/response.clj`
  - `modules/fx-typed/src/com/lambdaseq/fx/typed.cljc` -> `modules/fx-typed/src/fx/typed.cljc`
- Clean up legacy directory trees under `src/com/`.
- Update namespace definitions and internal requires:
  - In `fx.core`: update `ns` to `fx.core`, change explicit class/interface checks (`instance? fx.core.IEffect`, `instance? fx.core.IFailure`, `instance? fx.core.IUnwindable`).
  - In `fx.utils`: update `ns` to `fx.utils` and require `[fx.core :as fx]`.
  - In `fx.jdbc`: update `ns` to `fx.jdbc`, import protocols/records from `fx.core`, and update require `[fx.core :as fx]`.
  - In `fx.jdbc.sql`: update `ns` to `fx.jdbc.sql`, requires `[fx.core :as fx]` and `[fx.jdbc :as fx-jdbc]`, and change context lookup `:com.lambdaseq.fx.jdbc/datasource` to `:fx.jdbc/datasource`.
  - In `fx.ring` and `fx.ring.response`: update `ns` declarations, requires, and `request-key` to `:fx.ring/request`.
  - In `fx.typed`: update `ns` to `fx.typed` and require `[fx.core :as fx]`.
- Update inter-module dependency coordinates in `modules/fx-jdbc/deps.edn` and `modules/fx-ring/deps.edn` from `com.lambdaseq/fx-core` to `fx/core`.

### ✓ Step 2: Refactor test suites, file paths, and Typed Clojure test fixtures
All test files across all modules are migrated to `test/fx/` with updated namespaces and all test fixture assertions pass.

- Relocate test files from `modules/*/test/com/lambdaseq/fx/...` to `modules/*/test/fx/...`:
  - `modules/fx-core/test/com/lambdaseq/fx/core_test.cljc` -> `modules/fx-core/test/fx/core_test.cljc`
  - `modules/fx-core/test/com/lambdaseq/fx/utils_test.cljc` -> `modules/fx-core/test/fx/utils_test.cljc`
  - `modules/fx-jdbc/test/com/lambdaseq/fx/jdbc_test.clj` -> `modules/fx-jdbc/test/fx/jdbc_test.clj`
  - `modules/fx-ring/test/com/lambdaseq/fx/ring_test.clj` -> `modules/fx-ring/test/fx/ring_test.clj`
  - `modules/fx-typed/test/com/lambdaseq/fx/typed_test.cljc` -> `modules/fx-typed/test/fx/typed_test.cljc`
- Clean up legacy directory trees under `test/com/`.
- Update namespace definitions and requires in `fx.core-test`, `fx.utils-test`, `fx.jdbc-test`, and `fx.ring-test`.
- Update `modules/fx-typed/test/fx/typed_test.cljc`: update `ns` declaration, requires, and all inline `:requires [[fx.core :as fx] [fx.typed]]` clauses across `is-tc-e` / `tc-e` type checker invocations.

### ✓ Step 3: Update build configuration, project manifests, and documentation
Root build scripts, project manifests, and all markdown documentation reflect the simplified `fx.*` namespaces and `fx/*` dependency names.

- Update `build.clj` to use `(def lib 'fx/fx)` instead of `com.lambdaseq/fx`.
- Update `pom.xml` to use groupId `<groupId>fx</groupId>`, name `<name>fx/fx</name>`, and clean up repository URLs.
- Update `README.md` to reference `fx/core`, `fx/typed`, `fx/jdbc`, `fx/ring`, and `fx.core` / `fx.utils` namespaces.
- Update `CHANGELOG.md` to remove `com.lambdaseq` namespace mentions in changelog entries and comparison links.
- Update `doc/intro.md` to reference `fx/fx`, `fx.core`, and record names without `com.lambdaseq`.
- Update `modules/fx-jdbc/README.md` and `modules/fx-ring/README.md` with updated `deps.edn` coordinates (`fx/jdbc`, `fx/ring`, `fx/core`), namespace requires, and context key examples (`:fx.jdbc/datasource`, `:fx.ring/request`).
- Update plan documents in `.junie/plans/` where applicable to keep documentation consistent.

### ✓ Step 4: Validate test suite execution and verify absence of legacy namespace references
The complete test suite runs green with 0 errors and a global search confirms zero remaining occurrences of `com.lambdaseq`.

- Execute the full test runner via `clojure -M:dev:test -m cognitect.test-runner -d modules/fx-core/test -d modules/fx-typed/test -d modules/fx-jdbc/test -d modules/fx-ring/test` (and `clojure -T:build test`).
- Verify all 90+ tests across core, typed, jdbc, and ring modules pass without failures or errors.
- Run a global search across all project files to ensure 0 references to `com.lambdaseq` remain.