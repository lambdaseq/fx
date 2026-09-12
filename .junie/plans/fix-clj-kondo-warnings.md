---
sessionId: session-260912-180115-orgn
---

# Requirements

### Overview & Goals
The goal of this task is to resolve all warnings emitted when executing the full repository CI pipeline:
```bash
clj -T:build ci
```
This includes resolving all 70 `clj-kondo` linter warnings across all modules, as well as fixing all 270+ ClojureScript compiler warnings emitted during `cljs-smoke` (primarily protocol implementation syntax issues in `fx-async`, undeclared self-referential record constructors, missing host namespaces, and platform-specific functions).

The entire CI suite (`clean`, `release-check`, `cljs-smoke`, `test`, `example-test`, and `jar`) must pass cleanly with zero errors, zero protocol warnings, and zero linter warnings while preserving all runtime semantics and adhering strictly to repository architecture directives.

### Scope
- **In Scope:**
  - Fixing all protocol implementation warnings in `modules/fx-async` (`async.cljc`, `channel.cljc`, and `coordination.cljc`) by converting dot-qualified interface names (`fx.core.ITagged`, `fx.core.IEffect`, `fx.core.IContinuation`) to namespace-qualified protocol symbols (`fx/ITagged`, `fx/IEffect`, `fx/IContinuation`).
  - Scoping JVM-specific types and buffers (`UnboundedBuffer`, `ConcurrentLinkedQueue`, `CompletableFuture`, `Semaphore`, `CountDownLatch`, `async/<!!`) behind `#?(:clj ...)` in `modules/fx-async/src/fx/async/coordination.cljc`.
  - Fixing host interop and exception handling in `modules/fx-async/src/fx/async/fiber.cljc` (`UUID/randomUUID` -> `#?(:clj ... :cljs (random-uuid))`, and guarding `InterruptedException`).
  - Resolving undeclared self-referential constructor factory warnings (`->RetryFrame`, `->AllFrame`, `->ForEachFrame`, `->CondFrame`, `->RetryScheduleFrame`, `->RepeatScheduleFrame`, `->ScheduleUnifiedFrame`) in `modules/fx-core/src/fx/core.cljc` and `modules/fx-schedule/src/fx/schedule.cljc` by using direct positional constructors `(RecordName. ...)`.
  - Resolving undeclared platform symbols in observability modules: `*err*` in `modules/fx-observability/src/fx/observability/log.cljc` and `format` in `modules/fx-observability/src/fx/observability/trace.cljc`.
  - Maintaining all 70 previously resolved `clj-kondo` linter fixes across `fx-core`, `fx-typed`, `fx-jdbc`, `fx-observability`, `fx-schedule`, `fx-http-client`, and `fx-async`.
- **Out of Scope:**
  - No changes to public API symbols, signatures, or semantics.
  - No introduction of function aliases, synonyms, or polymorphic dynamic evaluators (strict adherence to repository guidelines).
  - No alteration of existing test assertions or pipeline behaviors.

### Acceptance Criteria
- `clj -T:build clean && clj -T:build ci` completes with exit code 0 and emits 0 protocol warnings, 0 undeclared var warnings, and 0 missing namespace warnings during `cljs-smoke`.
- `clj -T:build kondo` reports `errors: 0, warnings: 0` across all modules.
- `clj -T:build test` passes all 186 unit tests and 1043 assertions with 0 failures and 0 errors.
- `clj -T:build example-test` passes all 35 tests with 0 failures and 0 errors.
- `clj -T:build release-check` validates the release contract cleanly.

# Technical Design

### Current Implementation & Root Cause Analysis
Running `clj -T:build clean && clj -T:build ci` currently triggers over 270 compiler warnings during ClojureScript smoke compilation (`cljs-smoke`):

1. **Protocol Implementation Syntax in ClojureScript (`fx-async`):**
   - In `modules/fx-async/src/fx/async.cljc`, `modules/fx-async/src/fx/async/channel.cljc`, and `modules/fx-async/src/fx/async/coordination.cljc`, effect records and frame records implement protocols using Java-style dot qualification:
     ```clojure
     (defrecord StepEffectFrame [effect]
       fx.core.IContinuation
       (-resume [_ val context stack] ...))
     ```
   - While Clojure JVM accepts dot-notation as implementing the underlying interface class, ClojureScript treats dot-notation as referencing a host JavaScript interface, which does not exist. This emits over 240 warnings:
     - `Symbol fx.core.IEffect is not a protocol`
     - `Bad method signature in protocol implementation, fx.core.IEffect does not declare method called -step`
     - `Symbol fx.core.ITagged is not a protocol`
     - `Bad method signature in protocol implementation, fx.core.ITagged does not declare method called tag`
     - `Symbol fx.core.IContinuation is not a protocol`
     - `Bad method signature in protocol implementation, fx.core.IContinuation does not declare method called -resume`
   - In contrast, `modules/fx-observability/src/fx/observability/log.cljc` implements protocols using `fx/ITagged` and `fx/IEffect` (slash syntax), which compiles cleanly without any protocol warnings.

2. **JVM-Only Buffers and Concurrency Primitives (`coordination.cljc`):**
   - `UnboundedBuffer` in `fx.async.coordination` directly implements JVM interfaces `clojure.core.async.impl.protocols.Buffer`, `clojure.core.async.impl.protocols.UnblockingBuffer`, and `clojure.lang.Counted` around `java.util.Queue`. None of these exist in ClojureScript, producing 8 protocol warnings.
   - `ConcurrentLinkedQueue`, `CompletableFuture`, `Semaphore`, and `CountDownLatch` are called directly in CLJS code paths, causing undeclared var warnings.
   - `AsyncQueue/-queue-take!` calls `(async/<!! chan)`. In ClojureScript, `<!!` (blocking take) does not exist.

3. **Self-Referential Record Constructors in `defrecord` Bodies:**
   - In `modules/fx-core/src/fx/core.cljc`: `RetryFrame`, `AllFrame`, `ForEachFrame`, and `CondFrame` invoke `->RetryFrame`, `->AllFrame`, `->ForEachFrame`, and `->CondFrame` inside their own method implementations. In ClojureScript, the factory function `->RecordName` is only interned *after* the `defrecord` macro evaluation completes, causing "Use of undeclared Var" warnings.
   - In `modules/fx-schedule/src/fx/schedule.cljc`: `RetryScheduleFrame`, `RepeatScheduleFrame`, and `ScheduleUnifiedFrame` similarly call `->RetryScheduleFrame`, `->RepeatScheduleFrame`, and `->ScheduleUnifiedFrame`.

4. **Host Interop Differences in `fiber.cljc`:**
   - Line 97 calls `java.util.UUID/randomUUID` unconditionally, causing `No such namespace: java.util.UUID` and `Use of undeclared Var java.util.UUID/randomUUID` in ClojureScript.
   - Lines 222-226 in `execute-fiber-task!` catch `InterruptedException` and `Throwable`, neither of which are defined in ClojureScript.

5. **Platform-Specific Functions in Observability Modules:**
   - In `fx.observability.log` line 29: `(binding [*out* *err*] ...)` triggers undeclared var `*err*` in ClojureScript.
   - In `fx.observability.trace` line 64: `(format "00-%s-%s-%s" ...)` triggers undeclared var `format` because `clojure.core/format` is JVM-only.

---

### Key Decisions
- **Namespace-Qualified Slash Syntax for Protocols:** Replace `fx.core.ITagged`, `fx.core.IEffect`, and `fx.core.IContinuation` with `fx/ITagged`, `fx/IEffect`, and `fx/IContinuation` across all `fx-async` files. This is standard and fully supported on both Clojure JVM and ClojureScript.
- **Direct Positional Constructors within `defrecord`:** Replace self-referential `->RecordName` calls within `defrecord` bodies with direct `(RecordName. ...)` constructor invocations. This avoids CLJS forward-reference warnings and provides faster record instantiation.
- **Reader Conditional Scoping for JVM Concurrency:**
  - Wrap `UnboundedBuffer` and its usage in `make-queue` in `#?(:clj ...)`.
  - Guard `Semaphore.`, `CountDownLatch.`, and `CompletableFuture.` with `#?(:clj ...)`, and provide lightweight CLJS counterparts or stubs.
  - Scope `(async/<!! chan)` to `:clj` in `AsyncQueue/-queue-take!`.
  - Wrap `java.util.UUID/randomUUID` to use `(random-uuid)` in ClojureScript.
  - Guard `InterruptedException` in `fiber.cljc` with `#?(:clj ...)`.
- **Portable String Concatenation and Console Logging:**
  - Replace `(format "00-%s-%s-%s" ...)` in `trace.cljc` with direct `(str "00-" ... "-" ... "-" ...)` concatenation.
  - Replace `(binding [*out* *err*] ...)` in `log.cljc` with `#?(:clj (binding [*out* *err*] (println formatted)) :cljs (js/console.error formatted))`.

---

### Proposed Changes by Module

#### 1. `modules/fx-async`
- `src/fx/async.cljc`:
  - Change `fx.core.IContinuation` -> `fx/IContinuation` in `StepEffectFrame`.
  - Change `fx.core.ITagged` -> `fx/ITagged` and `fx.core.IEffect` -> `fx/IEffect` across `ForkEffect`, `JoinEffect`, `InterruptEffect`, `FiberStatusEffect`, `FiberChildrenEffect`, `SleepFiberEffect`, `TimeoutFiberEffect`, and `SuperviseFiberEffect`.
- `src/fx/async/channel.cljc`:
  - Change `fx.core.IContinuation` -> `fx/IContinuation` in `StepEffectFrame`.
  - Change `fx.core.ITagged` -> `fx/ITagged` and `fx.core.IEffect` -> `fx/IEffect` across `ChanEffect`, `ChanPutEffect`, `ChanTakeEffect`, `ChanAltsEffect`, `ChanSlideEffect`, `ChanDropEffect`, `ChanCloseEffect`, `ChanDrainEffect`, `ChanTapEffect`, and `ChanUntapEffect`.
- `src/fx/async/coordination.cljc`:
  - Change `fx.core.IContinuation` -> `fx/IContinuation` in `StepEffectFrame`.
  - Change `fx.core.ITagged` -> `fx/ITagged` and `fx.core.IEffect` -> `fx/IEffect` across all 28 coordination effect records (`QueueConstructorEffect`, `QueueOfferEffect`, `DeferredConstructorEffect`, `SemaphoreConstructorEffect`, `LatchConstructorEffect`, `RefConstructorEffect`, `HubConstructorEffect`, etc.).
  - Wrap `UnboundedBuffer` definition in `#?(:clj ...)`. In `make-queue`, branch `:unbounded` with `#?(:clj (async/chan (->UnboundedBuffer (ConcurrentLinkedQueue.))) :cljs (async/chan))`.
  - In `AsyncQueue`: wrap `(-queue-take! [_])` body in `#?(:clj (let [res (async/<!! chan)] ...) :cljs (let [res (async/poll! chan)] ...))`.
  - In `make-deferred`: wrap `(->DeferredCell #?(:clj (CompletableFuture.) :cljs (atom nil)))`.
  - In `make-semaphore` and `SemaphoreCell`: guard `(Semaphore. (int permits))` and `SemaphoreCell` with `#?(:clj ...)`.
  - In `make-latch` and `LatchCell`: guard `(CountDownLatch. (int count))` and `LatchCell` with `#?(:clj ...)`.
- `src/fx/async/fiber.cljc`:
  - Line 97: Replace `(java.util.UUID/randomUUID)` with `#?(:clj (java.util.UUID/randomUUID) :cljs (random-uuid))`.
  - Lines 220-229: Wrap `(catch InterruptedException ...)` in `#?(:clj ...)`, and catch `#?(:clj Throwable :cljs :default)` with appropriate platform error payload.

#### 2. `modules/fx-core`
- `src/fx/core.cljc`:
  - Line 231 in `RetryFrame`: Replace `(->RetryFrame ...)` with `(RetryFrame. target value policy (inc attempt) next-delay)`.
  - Line 243 in `AllFrame`: Replace `(->AllFrame ...)` with `(AllFrame. rest-effs new-results)`.
  - Line 256 in `ForEachFrame`: Replace `(->ForEachFrame ...)` with `(ForEachFrame. f rest-items new-results)`.
  - Line 307 in `CondFrame`: Replace `(->CondFrame ...)` with `(CondFrame. value next-expr rest-conds)`.

#### 3. `modules/fx-schedule`
- `src/fx/schedule.cljc`:
  - Line 414 in `RetryScheduleFrame`: Replace `(->RetryScheduleFrame ...)` with `(RetryScheduleFrame. target schedule (:state step-res))`.
  - Line 429 in `RepeatScheduleFrame`: Replace `(->RepeatScheduleFrame ...)` with `(RepeatScheduleFrame. target schedule (:state step-res) res)`.
  - Line 441 in `ScheduleUnifiedFrame`: Replace `(->ScheduleUnifiedFrame ...)` with `(ScheduleUnifiedFrame. target schedule (:state step-res))`.

#### 4. `modules/fx-observability`
- `src/fx/observability/log.cljc`:
  - Line 29: Use `#?(:clj (binding [*out* *err*] (println formatted)) :cljs (js/console.error formatted))`.
- `src/fx/observability/trace.cljc`:
  - Line 64: In `format-traceparent`, replace `format` with portable `str`: `(str "00-" (or trace-id (random-trace-id)) "-" (or span-id (random-span-id)) "-" (if sampled? "01" "00"))`.

---

### Risks & Mitigations
- **Risk:** Changing protocol implementation syntax from dot to slash could affect Java reflection or interface inheritance on the JVM.
  - *Mitigation:* In Clojure, `(defrecord Foo [] my.ns/IProtocol ...)` generates identical Java bytecode implementing `my.ns.IProtocol` interface. Verified via Clojure REPL: `(instance? fx.core.IEffect (->Bar))` returns `true`.
- **Risk:** Replacing `->RecordName` with `(RecordName. ...)` could fail if positional arguments don't match the defrecord fields.
  - *Mitigation:* Ensure argument counts and orders exactly match each record's field declaration.
- **Risk:** Scoping `async/<!!` in `AsyncQueue/-queue-take!` in ClojureScript could change take semantics.
  - *Mitigation:* ClojureScript is single-threaded and does not support blocking thread park (`<!!`). Using non-blocking `poll!` or async `<!` is the canonical CLJS approach.

# Testing

### Validation Approach
Verification will be performed directly via the repo's build tools and tasks:
1. `clj -T:build clean && clj -T:build cljs-smoke` — must complete with 0 protocol warnings, 0 undeclared var warnings, and 0 missing namespace warnings.
2. `clj -T:build kondo` — must exit with 0 errors and 0 warnings across all 8 modules.
3. `clj -T:build test` — executes all 186 unit tests across all modules to verify zero regression.
4. `clj -T:build example-test` — executes all 35 example tests.
5. `clj -T:build release-check` — ensures public API symbols and documentation references remain consistent.
6. `clj -T:build ci` — executes the complete CI pipeline end-to-end (`clean`, `release-check`, `cljs-smoke`, `test`, `example-test`, `jar`) with green exit status.

### Key Scenarios
- **Protocol Warning Elimination:** `cljs-smoke` compiles `fx.async`, `fx.async.channel`, and `fx.async.coordination` without emitting "Symbol ... is not a protocol" or "Bad method signature in protocol implementation".
- **Self-Referential Record Instantiation:** `RetryFrame`, `AllFrame`, `ForEachFrame`, `CondFrame`, and schedule frames evaluate recursive execution steps cleanly in both Clojure and ClojureScript without forward-reference warnings.
- **Observability Cross-Platform Formatting:** `trace.cljc` generates W3C traceparents and `log.cljc` routes error logs cleanly without relying on JVM-only `format` or `*err*`.
- **Concurrency & Async Integrity:** All unit tests in `fx.async.channel-test`, `fx.async.coordination-test`, `fx.async.fiber-test`, and `fx.async.parallel-test` pass without deadlock or timing regressions.

<!-- id: plan -->
## Implementation Plan

### ✓ Step 1: Fix self-referential record constructors in `fx-core` and `fx-schedule`
<!-- id: 1 -->
- In `modules/fx-core/src/fx/core.cljc`: Replace `->RetryFrame`, `->AllFrame`, `->ForEachFrame`, and `->CondFrame` within their own `defrecord` implementations with direct `(RetryFrame. ...)`, `(AllFrame. ...)`, `(ForEachFrame. ...)`, and `(CondFrame. ...)`.
- In `modules/fx-schedule/src/fx/schedule.cljc`: Replace `->RetryScheduleFrame`, `->RepeatScheduleFrame`, and `->ScheduleUnifiedFrame` with direct `(RetryScheduleFrame. ...)`, `(RepeatScheduleFrame. ...)`, and `(ScheduleUnifiedFrame. ...)`.

### ✓ Step 2: Fix platform-specific functions in `fx-observability`
<!-- id: 2 -->
- In `modules/fx-observability/src/fx/observability/log.cljc`: Replace `*err*` binding with `#?(:clj (binding [*out* *err*] (println formatted)) :cljs (js/console.error formatted))`.
- In `modules/fx-observability/src/fx/observability/trace.cljc`: Replace `(format "00-%s-%s-%s" ...)` with portable `str` concatenation.

### ✓ Step 3: Fix host interop and exception handling in `fx.async.fiber`
<!-- id: 3 -->
- In `modules/fx-async/src/fx/async/fiber.cljc`: Replace `(java.util.UUID/randomUUID)` with `#?(:clj (java.util.UUID/randomUUID) :cljs (random-uuid))`.
- Wrap `InterruptedException` catch in `#?(:clj ...)` and make general catch `#?(:clj Throwable :cljs :default)`.

### ✓ Step 4: Fix protocol syntax and JVM concurrency scoping in `fx-async`
<!-- id: 4 -->
- In `modules/fx-async/src/fx/async.cljc`: Update protocol implementations from `fx.core.IContinuation`, `fx.core.ITagged`, `fx.core.IEffect` to `fx/IContinuation`, `fx/ITagged`, `fx/IEffect`.
- In `modules/fx-async/src/fx/async/channel.cljc`: Update protocol implementations from `fx.core.*` to `fx/*`.
- In `modules/fx-async/src/fx/async/coordination.cljc`: Update protocol implementations from `fx.core.*` to `fx/*`.
- In `modules/fx-async/src/fx/async/coordination.cljc`: Wrap `UnboundedBuffer` in `#?(:clj ...)`, guard `ConcurrentLinkedQueue`, `CompletableFuture`, `Semaphore`, and `CountDownLatch` behind `#?(:clj ...)`, and scope `async/<!!` in `AsyncQueue/-queue-take!` behind `#?(:clj ...)`.

### ✓ Step 5: Full verification and CI validation
<!-- id: 5 -->
- Run `clj -T:build clean && clj -T:build cljs-smoke` and verify 0 protocol/var warnings.
- Run `clj -T:build kondo` and verify 0 errors, 0 warnings across all modules.
- Run `clj -T:build test` and `clj -T:build example-test`.
- Run `clj -T:build release-check`.
- Run `clj -T:build ci` to ensure the entire CI pipeline succeeds cleanly.