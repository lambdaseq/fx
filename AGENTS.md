# Repository Guidelines & Architecture Directives

## 1. Strict Prohibition of Function Aliases and Synonyms
- **Canonical Naming Only:** Every effect combinator, constructor, and runtime runner must have exactly one single, canonical name. Never define or export synonym aliases (e.g., do not export `fold>` as an alias for `match>`, `bracket>` for `acquire-release>`, or non-suffixed variants like `zip`, `sleep`, or `traverse`).
- **Standardized Suffix Conventions:**
  - Use the `>` suffix strictly for effect constructors, combinators, and pipeline transforms (e.g., `succeed>`, `fail>`, `map>`, `match>`, `acquire-release>`).
  - Use the `!` suffix strictly for runners that execute side-effects or drive pipeline evaluation to completion (e.g., `run-sync!`, `run-async!`).
  - Use standard predicates with `?` (e.g., `effect?`, `failure?`).
- **Rationale for Utility Maximization:** Removing redundant aliases minimizes the public API footprint, prevents codebase fragmentation across teams, ensures predictable grep/search operations, and eliminates dead-weight test overhead.

---

## 2. Deterministic Argument Types (No Dual-Mode Callback vs. Effect Arguments)
- **Monomorphic Parameter Contracts:** Function parameters must not support polymorphic "either callback function or Effect record" dispatch. If a parameter position is designed to accept an `Effect`, it must strictly accept an `Effect`. If a parameter position is designed to accept a pure transformation function, it must strictly accept a function.
- **Elimination of Dynamic Coercion Helpers:** Do not use polymorphic runtime evaluators (such as `eval-eff-or-fn`). Passing a callback function to an effect parameter or an effect to a callback parameter must be treated as an invalid contract violation.
- **Explicit Pipeline Composition:**
  - For static fallbacks and combinators (e.g., `or-else>`, `catch>`, `ensure>`), require explicit `Effect` instances.
  - When dynamic evaluation dependent on runtime input is necessary, compose explicitly using pure mappings (`map>`), flat-mapping (`mapcat>`), or explicitly designated effect-producing function combinators.
- **Rationale for Utility Maximization:** Strict type segregation eliminates runtime introspection overhead, removes silent arity-mismatch bugs, simplifies debugging traces, and makes effect composition statically verifiable and predictable.
