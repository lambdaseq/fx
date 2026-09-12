(ns fx.schedule
  "Composable, pure schedules, recurrence policies, retries, and resilience primitives for fx."
  (:refer-clojure :exclude [identity])
  (:require [fx.core :as fx]))

;; ---------------------------------------------------------------------------
;; Schedule Protocol & Core Record
;; ---------------------------------------------------------------------------

(defprotocol ISchedule
  "Protocol for composable schedule state machines."
  (-initial-state [this]
    "Returns the initial state value for the schedule.")
  (-step [this state now input]
    "Performs a single transition step given current `state`, timestamp `now` (ms), and `input`.
     Returns a map:
       {:decision :recur | :halt
        :delay-ms <long>
        :state    <any>
        :out      <any>}"))

(defrecord Schedule [initial-state-fn step-fn]
  ISchedule
  (-initial-state [_]
    (initial-state-fn))
  (-step [_ state now input]
    (step-fn state now input)))

(defn schedule?
  "Returns true if `x` satisfies ISchedule."
  [x]
  #?(:clj  (instance? fx.schedule.ISchedule x)
     :cljs (satisfies? ISchedule x)))

(defn make-schedule
  "Constructs an ISchedule instance given an `initial-state-fn` and `step-fn`."
  [initial-state-fn step-fn]
  (->Schedule initial-state-fn step-fn))

;; ---------------------------------------------------------------------------
;; Foundational Schedule Constructors
;; ---------------------------------------------------------------------------

(defn forever>
  "Creates a schedule that recurs indefinitely with 0 delay.
   Outputs the total count of completed recurrences."
  []
  (make-schedule
   (fn [] 0)
   (fn [count _now _input]
     {:decision :recur
      :delay-ms 0
      :state    (inc count)
      :out      count})))

(defn recur-n>
  "Creates a schedule that recurs at most `n` times with 0 delay.
   Outputs the total count of completed recurrences."
  [n]
  (make-schedule
   (fn [] 0)
   (fn [count _now _input]
     (if (< count n)
       {:decision :recur
        :delay-ms 0
        :state    (inc count)
        :out      count}
       {:decision :halt
        :delay-ms 0
        :state    count
        :out      count}))))

(defn once>
  "Creates a schedule that recurs exactly once with 0 delay."
  []
  (recur-n> 1))

(defn fixed>
  "Creates a schedule that recurs indefinitely with a constant `delay-ms` interval.
   Outputs the total count of completed recurrences."
  [delay-ms]
  (let [d (long (max 0 delay-ms))]
    (make-schedule
     (fn [] 0)
     (fn [count _now _input]
       {:decision :recur
        :delay-ms d
        :state    (inc count)
        :out      count}))))

(defn linear-backoff>
  "Creates a schedule that increases delay linearly (`(* initial-ms attempt)`).
   Options map:
     `:initial-ms` - base delay in milliseconds (default: 100)
     `:max-ms`     - optional ceiling cap in milliseconds"
  ([]
   (linear-backoff> {:initial-ms 100}))
  ([opts]
   (let [initial-ms (long (get opts :initial-ms 100))
         max-ms     (when-let [m (:max-ms opts)] (long m))]
     (make-schedule
      (fn [] 0)
      (fn [count _now _input]
        (let [multiplier (inc count)
              computed   (* initial-ms multiplier)
              delay-ms   (if max-ms (min computed max-ms) computed)]
          {:decision :recur
           :delay-ms (long (max 0 delay-ms))
           :state    (inc count)
           :out      delay-ms}))))))

(defn exponential-backoff>
  "Creates a schedule that increases delay exponentially (`(* initial-ms (Math/pow factor attempt))`).
   Options map:
     `:initial-ms` - base delay in milliseconds (default: 100)
     `:factor`     - exponential growth multiplier (default: 2.0)
     `:max-ms`     - optional ceiling cap in milliseconds"
  ([]
   (exponential-backoff> {:initial-ms 100 :factor 2.0}))
  ([opts]
   (let [initial-ms (double (get opts :initial-ms 100))
         factor     (double (get opts :factor 2.0))
         max-ms     (when-let [m (:max-ms opts)] (long m))]
     (make-schedule
      (fn [] 0)
      (fn [count _now _input]
        (let [computed (long (* initial-ms (Math/pow factor (double count))))
              delay-ms (if max-ms (min computed max-ms) computed)]
          {:decision :recur
           :delay-ms (long (max 0 delay-ms))
           :state    (inc count)
           :out      delay-ms}))))))

(defn fibonacci-backoff>
  "Creates a schedule that increases delay according to the Fibonacci sequence.
   Options map:
     `:initial-ms` - base delay multiplier in milliseconds (default: 100)
     `:max-ms`     - optional ceiling cap in milliseconds"
  ([]
   (fibonacci-backoff> {:initial-ms 100}))
  ([opts]
   (let [initial-ms (long (get opts :initial-ms 100))
         max-ms     (when-let [m (:max-ms opts)] (long m))]
     (make-schedule
      (fn [] [0 1])
      (fn [[a b] _now _input]
        (let [computed (long (* initial-ms b))
              delay-ms (if max-ms (min computed max-ms) computed)]
          {:decision :recur
           :delay-ms (long (max 0 delay-ms))
           :state    [b (+ a b)]
           :out      delay-ms}))))))

(defn elapsed>
  "Creates a schedule that recurs as long as the elapsed time since start is less than `max-duration-ms`.
   Outputs elapsed time in milliseconds."
  [max-duration-ms]
  (let [max-ms (long max-duration-ms)]
    (make-schedule
     (fn [] {:start-time nil :elapsed 0})
     (fn [{:keys [start-time]} now _input]
       (let [start   (or start-time now)
             elapsed (- now start)]
         (if (< elapsed max-ms)
           {:decision :recur
            :delay-ms 0
            :state    {:start-time start :elapsed elapsed}
            :out      elapsed}
           {:decision :halt
            :delay-ms 0
            :state    {:start-time start :elapsed elapsed}
            :out      elapsed}))))))

;; ---------------------------------------------------------------------------
;; Schedule Composition Combinators
;; ---------------------------------------------------------------------------

(defn intersect>
  "Combines two schedules into one that recurs only as long as *both* schedules recur.
   Uses the maximum delay of both schedules on each step.
   Outputs a pair `[out-a out-b]`."
  [sched-a sched-b]
  (make-schedule
   (fn [] [(-initial-state sched-a) (-initial-state sched-b)])
   (fn [[st-a st-b] now input]
     (let [res-a    (-step sched-a st-a now input)
           res-b    (-step sched-b st-b now input)
           decision (if (and (= (:decision res-a) :recur)
                             (= (:decision res-b) :recur))
                      :recur
                      :halt)
           delay-ms (max (long (:delay-ms res-a))
                         (long (:delay-ms res-b)))]
       {:decision decision
        :delay-ms (long (max 0 delay-ms))
        :state    [(:state res-a) (:state res-b)]
        :out      [(:out res-a) (:out res-b)]}))))

(defn union>
  "Combines two schedules into one that recurs as long as *either* schedule recurs.
   Uses the minimum delay of both continuing schedules on each step.
   Outputs a pair `[out-a out-b]`."
  [sched-a sched-b]
  (make-schedule
   (fn [] [(-initial-state sched-a) (-initial-state sched-b)])
   (fn [[st-a st-b] now input]
     (let [res-a    (-step sched-a st-a now input)
           res-b    (-step sched-b st-b now input)
           recur-a? (= (:decision res-a) :recur)
           recur-b? (= (:decision res-b) :recur)
           decision (if (or recur-a? recur-b?) :recur :halt)
           delay-ms (cond
                      (and recur-a? recur-b?) (min (long (:delay-ms res-a)) (long (:delay-ms res-b)))
                      recur-a?                (long (:delay-ms res-a))
                      recur-b?                (long (:delay-ms res-b))
                      :else                   (min (long (:delay-ms res-a)) (long (:delay-ms res-b))))]
       {:decision decision
        :delay-ms (long (max 0 delay-ms))
        :state    [(:state res-a) (:state res-b)]
        :out      [(:out res-a) (:out res-b)]}))))

(defn and-then>
  "Sequentially composes two schedules. Runs `sched-a` to exhaustion, then seamlessly continues with `sched-b`."
  [sched-a sched-b]
  (make-schedule
   (fn [] {:phase :a :state (-initial-state sched-a)})
   (fn [{:keys [phase state]} now input]
     (if (= phase :a)
       (let [res-a (-step sched-a state now input)]
         (if (= (:decision res-a) :recur)
           {:decision :recur
            :delay-ms (long (:delay-ms res-a))
            :state    {:phase :a :state (:state res-a)}
            :out      (:out res-a)}
           (let [b-init (-initial-state sched-b)
                 res-b  (-step sched-b b-init now input)]
             {:decision (:decision res-b)
              :delay-ms (long (:delay-ms res-b))
              :state    {:phase :b :state (:state res-b)}
              :out      (:out res-b)})))
       (let [res-b (-step sched-b state now input)]
         {:decision (:decision res-b)
          :delay-ms (long (:delay-ms res-b))
          :state    {:phase :b :state (:state res-b)}
          :out      (:out res-b)})))))

;; ---------------------------------------------------------------------------
;; Timing & Delay Modifiers
;; ---------------------------------------------------------------------------

(defn jitter>
  "Applies randomized jitter `[(* delay (- 1.0 factor)), (* delay (+ 1.0 factor))]` to schedule delays.
   `factor` defaults to 0.1 (10% variance)."
  ([schedule]
   (jitter> schedule 0.1))
  ([schedule factor]
   (let [f (double factor)]
     (make-schedule
      (fn [] (-initial-state schedule))
      (fn [state now input]
        (let [res      (-step schedule state now input)
              delay-ms (long (:delay-ms res))]
          (if (pos? delay-ms)
            (let [min-f (max 0.0 (- 1.0 f))
                  max-f (+ 1.0 f)
                  rnd   (rand)
                  mult  (+ min-f (* rnd (- max-f min-f)))
                  jd    (long (* delay-ms mult))]
              (assoc res :delay-ms (long (max 0 jd))))
            res)))))))

(defn modify-delay>
  "Transforms the computed delay of a schedule using pure function `(f delay-ms state)` or `(f delay-ms)`."
  [schedule f]
  (make-schedule
   (fn [] (-initial-state schedule))
   (fn [state now input]
     (let [res   (-step schedule state now input)
           new-d (try
                   (f (:delay-ms res) (:state res))
                   (catch #?(:clj Throwable :cljs :default) _
                     (f (:delay-ms res))))]
       (assoc res :delay-ms (long (max 0 (or new-d 0))))))))

;; ---------------------------------------------------------------------------
;; Condition & Filtering Combinators
;; ---------------------------------------------------------------------------

(defn while-input>
  "Recurs only while `(pred input)` returns truthy."
  [schedule pred]
  (make-schedule
   (fn [] (-initial-state schedule))
   (fn [state now input]
     (if (pred input)
       (-step schedule state now input)
       {:decision :halt
        :delay-ms 0
        :state    state
        :out      nil}))))

(defn until-input>
  "Recurs until `(pred input)` returns truthy (halts when `(pred input)` is truthy)."
  [schedule pred]
  (make-schedule
   (fn [] (-initial-state schedule))
   (fn [state now input]
     (if-not (pred input)
       (-step schedule state now input)
       {:decision :halt
        :delay-ms 0
        :state    state
        :out      nil}))))

(defn while-output>
  "Recurs only while `(pred output)` of the step result returns truthy."
  [schedule pred]
  (make-schedule
   (fn [] (-initial-state schedule))
   (fn [state now input]
     (let [res (-step schedule state now input)]
       (if (and (= (:decision res) :recur) (pred (:out res)))
         res
         (assoc res :decision :halt))))))

(defn until-output>
  "Recurs until `(pred output)` of the step result returns truthy."
  [schedule pred]
  (make-schedule
   (fn [] (-initial-state schedule))
   (fn [state now input]
     (let [res (-step schedule state now input)]
       (if (and (= (:decision res) :recur) (not (pred (:out res))))
         res
         (assoc res :decision :halt))))))

(defn- extract-tag [input]
  (cond
    (fx/failure? input) (fx/tag input)
    (and (map? input) (contains? input :tag)) (:tag input)
    :else input))

(defn while-tag>
  "Filters a retry schedule to recur only when failure `:tag` matches `expected-tag` (keyword or set of keywords)."
  [schedule expected-tag]
  (let [match? (if (set? expected-tag) expected-tag #{expected-tag})]
    (while-input> schedule (fn [input] (contains? match? (extract-tag input))))))

(defn until-tag>
  "Filters a retry schedule to halt when failure `:tag` matches `halt-tag` (keyword or set of keywords)."
  [schedule halt-tag]
  (let [match? (if (set? halt-tag) halt-tag #{halt-tag})]
    (until-input> schedule (fn [input] (contains? match? (extract-tag input))))))

;; ---------------------------------------------------------------------------
;; Output Inspection & Transformation Combinators
;; ---------------------------------------------------------------------------

(defn map-output>
  "Purely transforms the `:out` value produced by each step of the schedule with `(f out)`."
  [schedule f]
  (make-schedule
   (fn [] (-initial-state schedule))
   (fn [state now input]
     (let [res (-step schedule state now input)]
       (assoc res :out (f (:out res)))))))

(defn tap-step>
  "Executes a non-interfering side effect `(tap-fn {:decision d :delay-ms ms :input in :out out :state st})`
   after each schedule step."
  [schedule tap-fn]
  (make-schedule
   (fn [] (-initial-state schedule))
   (fn [state now input]
     (let [res (-step schedule state now input)]
       (try
         (tap-fn {:decision (:decision res)
                  :delay-ms (:delay-ms res)
                  :input    input
                  :out      (:out res)
                  :state    (:state res)})
         (catch #?(:clj Throwable :cljs :default) _ nil))
       res))))

;; ---------------------------------------------------------------------------
;; Runtime Timing Helper
;; ---------------------------------------------------------------------------

(defn- current-time-ms []
  #?(:clj  (System/currentTimeMillis)
     :cljs (.now js/Date)))

(defn step-schedule!
  "Drives a single step evaluation of `schedule` given current `state`, optional `now` timestamp (ms), and `input`.
   Returns `{:decision :recur | :halt, :delay-ms <long>, :state <any>, :out <any>}`."
  ([schedule state input]
   (step-schedule! schedule state (current-time-ms) input))
  ([schedule state now input]
   (-step schedule state (long now) input)))

;; ---------------------------------------------------------------------------
;; Continuation Frame Records
;; ---------------------------------------------------------------------------

(defrecord RetryScheduleFrame [target schedule sched-state]
  fx/IContinuation
  (-resume [_ res context stack]
    (if (fx/failure? res)
      (let [now      (current-time-ms)
            step-res (-step schedule sched-state now res)]
        (if (= (:decision step-res) :recur)
          (let [d (long (get step-res :delay-ms 0))]
            #?(:clj (when (pos? d) (Thread/sleep d)) :cljs nil)
            [target nil context (conj stack (->RetryScheduleFrame target schedule (:state step-res)))])
          [nil res context stack]))
      [nil res context stack])))

(defrecord RepeatScheduleFrame [target schedule sched-state last-val]
  fx/IContinuation
  (-resume [_ res context stack]
    (if (fx/failure? res)
      [nil res context stack]
      (let [now      (current-time-ms)
            step-res (-step schedule sched-state now res)]
        (if (= (:decision step-res) :recur)
          (let [d (long (get step-res :delay-ms 0))]
            #?(:clj (when (pos? d) (Thread/sleep d)) :cljs nil)
            [target nil context (conj stack (->RepeatScheduleFrame target schedule (:state step-res) res))])
          [nil res context stack])))))

(defrecord ScheduleUnifiedFrame [target schedule sched-state]
  fx/IContinuation
  (-resume [_ res context stack]
    (let [now      (current-time-ms)
          step-res (-step schedule sched-state now res)]
      (if (= (:decision step-res) :recur)
        (let [d (long (get step-res :delay-ms 0))]
          #?(:clj (when (pos? d) (Thread/sleep d)) :cljs nil)
          [target nil context (conj stack (->ScheduleUnifiedFrame target schedule (:state step-res)))])
        [nil res context stack]))))

;; ---------------------------------------------------------------------------
;; Effect Records
;; ---------------------------------------------------------------------------

(defrecord RetryScheduleEffect [tag prev-effect data target schedule]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (if (and (some? val) (fx/effect? target) (nil? (:prev-effect target)))
                  (fx/chain> (fx/succeed> val) target)
                  (or target (fx/succeed> val)))
            init-st (-initial-state schedule)]
        [eff nil context (conj stack (->RetryScheduleFrame eff schedule init-st))]))))

(defrecord RepeatScheduleEffect [tag prev-effect data target schedule]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (if (and (some? val) (fx/effect? target) (nil? (:prev-effect target)))
                  (fx/chain> (fx/succeed> val) target)
                  (or target (fx/succeed> val)))
            init-st (-initial-state schedule)]
        [eff nil context (conj stack (->RepeatScheduleFrame eff schedule init-st val))]))))

(defrecord ScheduleUnifiedEffect [tag prev-effect data target schedule]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (if (and (some? val) (fx/effect? target) (nil? (:prev-effect target)))
                  (fx/chain> (fx/succeed> val) target)
                  (or target (fx/succeed> val)))
            init-st (-initial-state schedule)]
        [eff nil context (conj stack (->ScheduleUnifiedFrame eff schedule init-st))]))))

;; ---------------------------------------------------------------------------
;; Effect Combinator Constructors
;; ---------------------------------------------------------------------------

(defn retry-schedule>
  "Retries `effect` on failure according to `schedule`.
   Halts and returns the successful value when `effect` succeeds, or the final failure if the schedule halts.

   Supports standalone execution and pipeline usage:
     (retry-schedule> (fetch-data>) (exponential-backoff>))
     (-> (fetch-data>) (retry-schedule> (recur-n> 3)))"
  ([schedule]
   (->RetryScheduleEffect :retry-schedule nil {:target nil :schedule schedule} nil schedule))
  ([target-or-schedule schedule-or-nil]
   (if (and (schedule? target-or-schedule) (nil? schedule-or-nil))
     (->RetryScheduleEffect :retry-schedule nil {:target nil :schedule target-or-schedule} nil target-or-schedule)
     (->RetryScheduleEffect :retry-schedule nil {:target target-or-schedule :schedule schedule-or-nil} target-or-schedule schedule-or-nil)))
  ([prev-effect target-effect schedule]
   (->RetryScheduleEffect :retry-schedule prev-effect {:target target-effect :schedule schedule} target-effect schedule)))

(defn repeat-schedule>
  "Repeats `effect` on success according to `schedule`.
   Halts and propagates failure immediately if `effect` fails, or returns the last success value when schedule halts.

   Supports standalone execution and pipeline usage:
     (repeat-schedule> (poll-job>) (fixed> 1000))
     (-> (poll-job>) (repeat-schedule> (recur-n> 5)))"
  ([schedule]
   (->RepeatScheduleEffect :repeat-schedule nil {:target nil :schedule schedule} nil schedule))
  ([target-or-schedule schedule-or-nil]
   (if (and (schedule? target-or-schedule) (nil? schedule-or-nil))
     (->RepeatScheduleEffect :repeat-schedule nil {:target nil :schedule target-or-schedule} nil target-or-schedule)
     (->RepeatScheduleEffect :repeat-schedule nil {:target target-or-schedule :schedule schedule-or-nil} target-or-schedule schedule-or-nil)))
  ([prev-effect target-effect schedule]
   (->RepeatScheduleEffect :repeat-schedule prev-effect {:target target-effect :schedule schedule} target-effect schedule)))

(defn schedule>
  "Evaluates `effect` continually according to `schedule` on both success and failure until schedule halts.

   Supports standalone execution and pipeline usage:
     (schedule> (poll-job>) (fixed> 500))
     (-> (poll-job>) (schedule> (recur-n> 3)))"
  ([schedule]
   (->ScheduleUnifiedEffect :schedule nil {:target nil :schedule schedule} nil schedule))
  ([target-or-schedule schedule-or-nil]
   (if (and (schedule? target-or-schedule) (nil? schedule-or-nil))
     (->ScheduleUnifiedEffect :schedule nil {:target nil :schedule target-or-schedule} nil target-or-schedule)
     (->ScheduleUnifiedEffect :schedule nil {:target target-or-schedule :schedule schedule-or-nil} target-or-schedule schedule-or-nil)))
  ([prev-effect target-effect schedule]
   (->ScheduleUnifiedEffect :schedule prev-effect {:target target-effect :schedule schedule} target-effect schedule)))

;; ---------------------------------------------------------------------------
;; Observability Hooks
;; ---------------------------------------------------------------------------

(defn- record-metric!
  "Safely records an in-memory metric when `:fx.observability/metrics-registry` is in `context`."
  [context metric-type metric-name tags val]
  (when-let [registry (or (get context :fx.observability/metrics-registry)
                          (get context :metrics-registry))]
    (when (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) registry)
      (let [k [metric-name (or tags {})]]
        (case metric-type
          :counter (swap! registry update-in [:counters k] (fnil #(+ % (long (or val 1))) 0))
          :gauge   (swap! registry assoc-in [:gauges k] val)
          nil)))))

;; ---------------------------------------------------------------------------
;; Circuit Breaker
;; ---------------------------------------------------------------------------

(defn make-circuit-breaker
  "Creates a new stateful circuit breaker atom with initial state `:closed`.
   Options map:
     `:failure-threshold` - consecutive failure count before opening (default: 3)
     `:reset-timeout-ms`  - milliseconds before attempting half-open probe (default: 5000)
     `:trip-on?`          - predicate on `IFailure` deciding if it counts toward tripping (default: (constantly true))
     `:on-state-change`   - optional callback `(fn [old-state new-state])`"
  ([]
   (make-circuit-breaker {}))
  ([opts]
   (atom {:state             :closed
          :failure-count     0
          :tripped-at        nil
          :failure-threshold (long (get opts :failure-threshold 3))
          :reset-timeout-ms  (long (get opts :reset-timeout-ms 5000))
          :trip-on?          (get opts :trip-on? (constantly true))
          :on-state-change   (:on-state-change opts)})))

(defn circuit-breaker-state
  "Returns current state keyword (`:closed`, `:open`, or `:half-open`) of a circuit breaker atom."
  [breaker-atom]
  (:state @breaker-atom))

(defn- notify-state-change! [on-state-change old-st new-st context]
  (when (not= old-st new-st)
    (when (fn? on-state-change)
      (try
        (on-state-change old-st new-st)
        (catch #?(:clj Throwable :cljs :default) _ nil)))
    (record-metric! context :counter "fx.schedule.circuit_breaker.state_change"
                    {:from (name old-st) :to (name new-st)} 1)))

(defrecord CircuitBreakerFrame [breaker-atom trip-on? on-state-change prior-state]
  fx/IContinuation
  (-resume [_ res context stack]
    (if (fx/failure? res)
      (do
        (when (trip-on? res)
          (let [now (current-time-ms)]
            (let [[old-s new-s]
                  (loop []
                    (let [curr @breaker-atom
                          old-st (:state curr)]
                      (case old-st
                        :half-open
                        (let [next-state (assoc curr :state :open :tripped-at now :failure-count (inc (:failure-count curr)))]
                          (if (compare-and-set! breaker-atom curr next-state)
                            [old-st :open]
                            (recur)))

                        :closed
                        (let [new-count (inc (:failure-count curr))
                              threshold (:failure-threshold curr)
                              tripped?  (>= new-count threshold)
                              next-st   (if tripped? :open :closed)
                              next-state (assoc curr :state next-st
                                                :failure-count (if tripped? new-count new-count)
                                                :tripped-at (when tripped? now))]
                          (if (compare-and-set! breaker-atom curr next-state)
                            [old-st next-st]
                            (recur)))

                        [old-st old-st])))]
              (notify-state-change! on-state-change old-s new-s context))))
        [nil res context stack])
      (do
        (let [[old-s new-s]
              (loop []
                (let [curr @breaker-atom
                      old-st (:state curr)]
                  (if (or (= old-st :half-open) (pos? (:failure-count curr)))
                    (let [next-state (assoc curr :state :closed :failure-count 0 :tripped-at nil)]
                      (if (compare-and-set! breaker-atom curr next-state)
                        [old-st :closed]
                        (recur)))
                    [old-st old-st])))]
          (notify-state-change! on-state-change old-s new-s context))
        [nil res context stack]))))

(defrecord CircuitBreakerEffect [tag prev-effect data target breaker-atom]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (let [eff (if (and (some? val) (fx/effect? target) (nil? (:prev-effect target)))
                  (fx/chain> (fx/succeed> val) target)
                  (or target (fx/succeed> val)))
            now (current-time-ms)
            curr @breaker-atom
            st   (:state curr)]
        (case st
          :closed
          [eff nil context (conj stack (->CircuitBreakerFrame breaker-atom (:trip-on? curr) (:on-state-change curr) :closed))]

          :open
          (let [tripped-at (:tripped-at curr)
                reset-ms   (:reset-timeout-ms curr)]
            (if (and tripped-at (>= (- now tripped-at) reset-ms))
              ;; Attempt transition to half-open
              (if (compare-and-set! breaker-atom curr (assoc curr :state :half-open))
                (do
                  (notify-state-change! (:on-state-change curr) :open :half-open context)
                  [eff nil context (conj stack (->CircuitBreakerFrame breaker-atom (:trip-on? curr) (:on-state-change curr) :half-open))])
                ;; CAS lost, re-step
                (-step this val context stack))
              ;; Circuit remains open, fast-fail
              [nil (fx/make-failure :circuit-breaker/open
                                    {:state            :open
                                     :tripped-at       tripped-at
                                     :reset-timeout-ms reset-ms})
               context stack]))

          :half-open
          [eff nil context (conj stack (->CircuitBreakerFrame breaker-atom (:trip-on? curr) (:on-state-change curr) :half-open))])))))

(defn circuit-breaker>
  "Wraps an effect with stateful circuit breaker protection.
   When open, fast-fails immediately with `:circuit-breaker/open` without executing the inner effect.

   Accepts either a circuit breaker atom (from `make-circuit-breaker`) or an options map.

   Supports standalone execution and pipeline usage:
     (circuit-breaker> (fetch-remote>) breaker)
     (-> (fetch-remote>) (circuit-breaker> {:failure-threshold 5 :reset-timeout-ms 10000}))"
  ([breaker-or-opts]
   (let [breaker (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) breaker-or-opts)
                   breaker-or-opts
                   (make-circuit-breaker breaker-or-opts))]
     (->CircuitBreakerEffect :circuit-breaker nil {:breaker breaker} nil breaker)))
  ([target-or-breaker breaker-or-opts]
   (if (or (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) target-or-breaker)
           (and (map? target-or-breaker) (not (fx/effect? target-or-breaker))))
     (let [breaker (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) target-or-breaker)
                     target-or-breaker
                     (make-circuit-breaker target-or-breaker))]
       (->CircuitBreakerEffect :circuit-breaker nil {:breaker breaker} nil breaker))
     (let [breaker (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) breaker-or-opts)
                     breaker-or-opts
                     (make-circuit-breaker breaker-or-opts))]
       (->CircuitBreakerEffect :circuit-breaker nil {:target target-or-breaker :breaker breaker} target-or-breaker breaker))))
  ([prev-effect target-effect breaker-or-opts]
   (let [breaker (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) breaker-or-opts)
                   breaker-or-opts
                   (make-circuit-breaker breaker-or-opts))]
     (->CircuitBreakerEffect :circuit-breaker prev-effect {:target target-effect :breaker breaker} target-effect breaker))))

;; ---------------------------------------------------------------------------
;; Rate Limiter
;; ---------------------------------------------------------------------------

(defn make-rate-limiter
  "Creates a new stateful token-bucket rate limiter atom.
   Options map:
     `:limit`        - maximum token capacity / burst limit (default: 10)
     `:interval-ms`  - time window in milliseconds for full replenishment (default: 1000)"
  ([]
   (make-rate-limiter {}))
  ([opts]
   (let [limit       (double (get opts :limit 10))
         interval-ms (double (get opts :interval-ms 1000))]
     (atom {:limit       limit
            :interval-ms interval-ms
            :tokens      limit
            :last-refill (current-time-ms)}))))

(defn- try-acquire-token! [limiter-atom]
  (let [now (current-time-ms)]
    (loop []
      (let [curr         @limiter-atom
            limit        (:limit curr)
            interval-ms  (:interval-ms curr)
            last-refill  (:last-refill curr)
            elapsed      (max 0.0 (double (- now last-refill)))
            refill-rate  (/ limit interval-ms)
            replenished  (min limit (+ (:tokens curr) (* elapsed refill-rate)))]
        (if (>= replenished 1.0)
          (let [next-state (assoc curr :tokens (- replenished 1.0) :last-refill now)]
            (if (compare-and-set! limiter-atom curr next-state)
              true
              (recur)))
          (let [next-state (assoc curr :tokens replenished :last-refill now)]
            (if (compare-and-set! limiter-atom curr next-state)
              false
              (recur))))))))

(defrecord RateLimiterEffect [tag prev-effect data target limiter-atom]
  fx/ITagged
  (tag [_] tag)
  fx/IEffect
  (prev-effect [_] prev-effect)
  (-step [this val context stack]
    (if (some? prev-effect)
      [prev-effect val context (conj stack (fx/->StepEffectFrame (assoc this :prev-effect nil)))]
      (if (try-acquire-token! limiter-atom)
        (let [eff (if (and (some? val) (fx/effect? target) (nil? (:prev-effect target)))
                    (fx/chain> (fx/succeed> val) target)
                    (or target (fx/succeed> val)))]
          [eff nil context stack])
        (do
          (record-metric! context :counter "fx.schedule.rate_limiter.rejected" {} 1)
          [nil (fx/make-failure :rate-limiter/exceeded
                                {:limit       (:limit @limiter-atom)
                                 :interval-ms (:interval-ms @limiter-atom)
                                 :available   (:tokens @limiter-atom)})
           context stack])))))

(defn rate-limiter>
  "Wraps an effect with token-bucket rate limiting.
   When token budget is exhausted, fails fast with `:rate-limiter/exceeded`.

   Accepts either a rate limiter atom (from `make-rate-limiter`) or an options map.

   Supports standalone execution and pipeline usage:
     (rate-limiter> (call-api>) limiter)
     (-> (call-api>) (rate-limiter> {:limit 5 :interval-ms 1000}))"
  ([limiter-or-opts]
   (let [limiter (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) limiter-or-opts)
                   limiter-or-opts
                   (make-rate-limiter limiter-or-opts))]
     (->RateLimiterEffect :rate-limiter nil {:limiter limiter} nil limiter)))
  ([target-or-limiter limiter-or-opts]
   (if (or (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) target-or-limiter)
           (and (map? target-or-limiter) (not (fx/effect? target-or-limiter))))
     (let [limiter (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) target-or-limiter)
                     target-or-limiter
                     (make-rate-limiter target-or-limiter))]
       (->RateLimiterEffect :rate-limiter nil {:limiter limiter} nil limiter))
     (let [limiter (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) limiter-or-opts)
                     limiter-or-opts
                     (make-rate-limiter limiter-or-opts))]
       (->RateLimiterEffect :rate-limiter nil {:target target-or-limiter :limiter limiter} target-or-limiter limiter))))
  ([prev-effect target-effect limiter-or-opts]
   (let [limiter (if (instance? #?(:clj clojure.lang.Atom :cljs cljs.core/Atom) limiter-or-opts)
                   limiter-or-opts
                   (make-rate-limiter limiter-or-opts))]
     (->RateLimiterEffect :rate-limiter prev-effect {:target target-effect :limiter limiter} target-effect limiter))))
