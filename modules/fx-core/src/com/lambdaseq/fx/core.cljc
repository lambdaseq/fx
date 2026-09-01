(ns com.lambdaseq.fx.core
  (:refer-clojure :exclude [tap>])
  #?(:clj (:import (java.util.concurrent CompletableFuture)
                   (java.util.function Supplier))))

(declare run-sync! failure->value)

(defprotocol ITagged
  "Protocol representing a tagged data structure in the effect system."
  (tag [this]
    "Returns the keyword identifying the tag (e.g. effect tag or failure tag)."))

(defprotocol IEffect
  "Protocol representing an executable effect in a computation pipeline."
  (-eval! [this v]
    "Evaluates the run function of the effect with input value `v` and returns the result.")
  (prev-effect [this]
    "Returns the upstream (previous) effect in the chain, or nil if this is the root."))

(defrecord Effect [tag prev-effect run]
  ITagged
  (tag [_] tag)
  IEffect
  (-eval! [_ v] (run v))
  (prev-effect [_] prev-effect))

(defprotocol IFailure
  "Protocol representing a typed failure in the effect system."
  (error-data [this]
    "Returns the error payload/data of the failure."))

(defrecord Failure
  [tag error-data]
  ITagged
  (tag [_] tag)
  IFailure
  (error-data [_] error-data))

(defn chain>
  "Chains `current-effect` onto `prev-effect` by setting `prev-effect` as its upstream dependency."
  [prev-effect current-effect]
  (assoc current-effect :prev-effect prev-effect))

(def ^:dynamic *context*
  "Dynamic map holding execution context and runtime state (e.g. :runner)."
  {})

(defn- -run!
  "Helper function for running an effect using the current runner in *context*."
  [effect]
  (let [runner (get *context* :runner run-sync!)]
    (runner effect)))

(defn effect?
  "Returns true if `x` implements IEffect or is an instance of Effect."
  [x]
  (instance? Effect x))

(defn failure?
  "Returns true if `x` implements IFailure or is an instance of Failure."
  [x]
  (instance? Failure x))

(defn make-effect
  "Creates a new Effect record given a `tag` keyword, an upstream `prev-effect` (or nil),
   and a single-argument execution function `run`."
  [tag prev-effect run]
  (Effect. tag prev-effect run))

(defn make-failure
  "Creates a new Failure record given a `tag` keyword and an `err` payload map or value."
  [tag err]
  (Failure. tag err))

(defmacro maybe-propagate-failure
  "If `v` is a failure, short-circuits and returns `v` directly. Otherwise evaluates `body`."
  [v & body]
  `(let [v# ~v]
     (if (failure? v#)
       v#
       (do ~@body))))

(defmacro maybe-propagate-effect
  "If `v` is an effect, returns `v` directly. Otherwise evaluates `body`."
  [v & body]
  `(let [v# ~v]
     (if (effect? v#)
       v#
       (do ~@body))))

(defn succeed>
  "Creates a successful effect yielding `value`.

   Example:
     (fx/succeed> 42)"
  [value]
  (make-effect :succeed nil (fn [_] value)))

(defn fail>
  "Creates a failed effect. Accepts either an existing IFailure instance, or a `tag` keyword
   and an `error-data` payload.

   Examples:
     (fx/fail> (fx/make-failure :not-found {:id 10}))
     (fx/fail> :not-found {:id 10})"
  ([failure]
   (make-effect :fail nil (constantly failure)))
  ([tag error-data]
   (make-effect :fail nil (constantly (make-failure tag error-data)))))

(defn map>
  "Maps a pure function `f` over the successful value of an effect.
   Short-circuits if the upstream effect yielded a failure.

   Supports point-free pipeline usage:
     (-> (fx/succeed> 10)
         (fx/map> inc))"
  ([f]
   (map> nil f))
  ([prev-effect f]
   (make-effect :map
                prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (f value))))))

(defn map-ctx>
  "Maps a binary function `(f value context)` over the successful value and active context.
   Short-circuits if the upstream effect yielded a failure.

   Supports point-free pipeline usage:
     (-> (fx/succeed> {:amount 100})
         (fx/map-ctx> (fn [order {:keys [tax-rate]}]
                        (assoc order :total (* (:amount order) (inc (or tax-rate 0)))))))"
  ([f]
   (map-ctx> nil f))
  ([prev-effect f]
   (make-effect :map-ctx
                prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (f value *context*))))))

(defn tap>
  "Executes a side-effecting function `f` on the successful value (e.g. logging or metrics)
   and propagates the original value unchanged to the next effect in the pipeline.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/tap> #(println \"Current value:\" %))
         (fx/map> inc))"
  ([f]
   (tap> nil f))
  ([prev-effect f]
   (make-effect :tap
                prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (f value)
                                           value)))))

(defn tap-error>
  "Executes a side-effecting function `f` on the failure (e.g. logging or metrics)
   when a failure occurs, and propagates the original failure unchanged to the next
   effect in the pipeline. Short-circuits (does not run `f`) if the upstream effect succeeded.

   Example:
     (-> (fx/fail> :not-found {:id 10})
         (fx/tap-error> (fn [err] (println \"Failed with tag:\" (fx/tag err))))
         (fx/catch> ...))"
  ([f]
   (tap-error> nil f))
  ([prev-effect f]
   (make-effect :tap-error
                prev-effect
                (fn [value]
                  (if (failure? value)
                    (do
                      (f value)
                      value)
                    value)))))

(defn do-ctx>
  "Executes a side-effecting binary function `(f value context)` on the successful value and
   active context, and propagates the original value unchanged.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/do-ctx> (fn [v {:keys [logger]}] (when logger (logger v))))
         (fx/map> inc))"
  ([f]
   (do-ctx> nil f))
  ([prev-effect f]
   (make-effect :do-ctx
                prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (f value *context*)
                                           value)))))

(defn context>
  "Creates an effect yielding the active execution context map or a value at `key`.

   Examples:
     (fx/context>) ;; yields entire *context* map
     (fx/context> :db) ;; yields (:db *context*)
     (fx/context> :db default-db) ;; yields default-db if :db is absent in *context*"
  ([]
   (make-effect :context nil (fn [_] *context*)))
  ([key]
   (make-effect :context nil (fn [_] (get *context* key))))
  ([key default-val]
   (make-effect :context nil (fn [_] (get *context* key default-val)))))

(defn service>
  "Creates an effect extracting service `key` from the active execution context.

   Examples:
     (fx/service> :db)
     (fx/service> :timeout 5000)"
  ([key]
   (context> key))
  ([key default-val]
   (context> key default-val)))

(defn provide>
  "Executes `effect` within a dynamic context merged with `context-map`.

   Supports point-free pipeline threading and standalone wrapping:
     (-> (fx/service> :db)
         (fx/provide> {:db mock-db}))

     (fx/provide> {:db mock-db} (fx/service> :db))

     (-> (fx/succeed> 10)
         (fx/provide> (fx/map-ctx> (fn [v ctx] (* v (:multiplier ctx)))) {:multiplier 4}))"
  ([context-map]
   (provide> nil context-map))
  ([a b]
   (let [[target-effect context-map] (cond
                                       (effect? a) [a b]
                                       (effect? b) [b a]
                                       (and (map? a) (not (effect? a))) [b a]
                                       :else [a b])]
     (make-effect :provide
                  nil
                  (fn [value]
                    (binding [*context* (assoc (merge *context* context-map) :runner (get *context* :runner run-sync!))]
                      (if target-effect
                        (let [eff (if (some? value)
                                    (chain> (succeed> value) target-effect)
                                    target-effect)]
                          (-run! eff))
                        value))))))
  ([prev-effect body-effect context-map]
   (make-effect :provide
                prev-effect
                (fn [value]
                  (binding [*context* (assoc (merge *context* context-map) :runner (get *context* :runner run-sync!))]
                    (let [eff (if (some? value)
                                (chain> (succeed> value) body-effect)
                                body-effect)]
                      (-run! eff)))))))

(defn provide-service>
  "Executes `effect` within a dynamic context where `key` is bound to `service-impl`.

   Supports point-free pipeline threading and standalone wrapping:
     (-> (fx/service> :db)
         (fx/provide-service> :db mock-db))

     (fx/provide-service> :db mock-db (fx/service> :db))"
  ([key service-impl]
   (provide> {key service-impl}))
  ([a b c]
   (if (keyword? a)
     (provide> c {a b})
     (provide> a {b c}))))

(defn ensure>
  "Executes a `finalizer-effect` guaranteed after the previous effect completes,
   regardless of whether the previous effect succeeded or failed.

   If the upstream effect succeeds, the finalizer runs and the original successful
   value is returned (unless the finalizer itself fails).
   If the upstream effect fails, the finalizer runs and the original failure
   is returned (unless the finalizer itself fails).

   Supports point-free pipeline usage:
     (-> (fx/succeed> 10)
         (fx/ensure> (fx/tap> (fn [_] (println \"Cleanup\")))))

     (-> (fx/fail> :not-found {:id 42})
         (fx/ensure> (fx/tap> (fn [_] (println \"Cleanup\")))))"
  ([finalizer-effect]
   (ensure> nil finalizer-effect))
  ([prev-effect finalizer-effect]
   (let [eff (if (effect? finalizer-effect)
               finalizer-effect
               (tap> finalizer-effect))]
     (make-effect :ensure
                  prev-effect
                  (fn [value]
                    (let [fin-input (if (failure? value) nil value)
                          fin-eff (if (nil? (:prev-effect eff))
                                    (chain> (succeed> fin-input) eff)
                                    eff)
                          fin-res (try
                                    (-run! fin-eff)
                                    (catch #?(:clj Throwable :cljs :default) e
                                      (make-failure :ensure e)))]
                      (cond
                        (failure? fin-res) fin-res
                        (failure? value) value
                        :else value)))))))

(defn- handle-try-exception [catch-handler e]
  (cond
    (nil? catch-handler)
    (make-failure :try e)

    (keyword? catch-handler)
    (make-failure catch-handler e)

    (effect? catch-handler)
    (-run! (chain> (succeed> e) catch-handler))

    (map? catch-handler)
    (let [matched-handler (some (fn [[k handler]]
                                  (cond
                                    #?@(:clj [(and (class? k) (instance? k e)) handler])
                                    (and (fn? k) (k e)) handler
                                    (= k :default) handler
                                    :else nil))
                                catch-handler)]
      (if matched-handler
        (handle-try-exception matched-handler e)
        (make-failure :try e)))

    (ifn? catch-handler)
    (let [res (catch-handler e)]
      (if (failure? res)
        res
        (make-failure :try res)))

    :else
    (make-failure :try e)))

(defn- eval-eff-or-fn
  "Evaluates `eff-or-fn` passing `resource` if needed."
  [eff-or-fn resource]
  (cond
    (effect? eff-or-fn)
    (-run! (if (nil? (:prev-effect eff-or-fn))
             (chain> (succeed> resource) eff-or-fn)
             eff-or-fn))

    (ifn? eff-or-fn)
    (let [res (try
                (eff-or-fn resource)
                (catch #?(:clj clojure.lang.ArityException :cljs :default) _
                  (eff-or-fn)))]
      (if (effect? res)
        (-run! (if (nil? (:prev-effect res))
                 (chain> (succeed> resource) res)
                 res))
        res))

    :else
    eff-or-fn))

(defn acquire-release>
  "Acquires a resource, runs `use-eff-fn`, and guarantees `release-eff-fn` executes
   if acquisition succeeded, regardless of whether usage succeeded, failed, or threw an exception.

   Supports standalone execution and point-free pipeline threading:
     (fx/acquire-release>
       (open-db-connection> config)
       (fn [conn] (run-queries> conn))
       (fn [conn] (close-conn> conn)))

     (-> (open-db-connection> config)
         (fx/acquire-release>
           (fn [conn] (run-queries> conn))
           (fn [conn] (close-conn> conn))))"
  ([use-eff-fn release-eff-fn]
   (make-effect :acquire-release nil
                (fn [value]
                  (maybe-propagate-failure value
                                           (if (nil? value)
                                             nil
                                             (let [use-res (try
                                                             (eval-eff-or-fn use-eff-fn value)
                                                             (catch #?(:clj Throwable :cljs :default) e
                                                               (try (eval-eff-or-fn release-eff-fn value)
                                                                    (catch #?(:clj Throwable :cljs :default) _ nil))
                                                               (throw e)))
                                                   rel-res (try
                                                             (eval-eff-or-fn release-eff-fn value)
                                                             (catch #?(:clj Throwable :cljs :default) e
                                                               (make-failure :release-error e)))]
                                               (cond
                                                 (failure? use-res) use-res
                                                 (failure? rel-res) rel-res
                                                 :else use-res)))))))
  ([acquire-eff use-eff-fn release-eff-fn]
   (make-effect :acquire-release nil
                (fn [value]
                  (let [acq-eff (if (and (some? value) (effect? acquire-eff) (nil? (:prev-effect acquire-eff)))
                                  (chain> (succeed> value) acquire-eff)
                                  acquire-eff)
                        resource (if (effect? acq-eff)
                                   (-run! acq-eff)
                                   (eval-eff-or-fn acq-eff value))]
                    (if (failure? resource)
                      resource
                      (let [use-res (try
                                      (eval-eff-or-fn use-eff-fn resource)
                                      (catch #?(:clj Throwable :cljs :default) e
                                        (try (eval-eff-or-fn release-eff-fn resource)
                                             (catch #?(:clj Throwable :cljs :default) _ nil))
                                        (throw e)))
                            rel-res (try
                                      (eval-eff-or-fn release-eff-fn resource)
                                      (catch #?(:clj Throwable :cljs :default) e
                                        (make-failure :release-error e)))]
                        (cond
                          (failure? use-res) use-res
                          (failure? rel-res) rel-res
                          :else use-res)))))))
  ([prev-effect acquire-eff use-eff-fn release-eff-fn]
   (let [acq (if (effect? acquire-eff)
               (chain> prev-effect acquire-eff)
               acquire-eff)]
     (acquire-release> acq use-eff-fn release-eff-fn))))

(defn die>
  "Yields an effect that halts execution by throwing a fatal defect exception."
  ([]
   (die> "Effect defect encountered"))
  ([err]
   (make-effect :die nil
                (fn [_]
                  (cond
                    (instance? #?(:clj Throwable :cljs js/Error) err)
                    (throw err)

                    (string? err)
                    (throw (ex-info err {:defect err}))

                    (map? err)
                    (throw (ex-info "Effect defect encountered" err))

                    :else
                    (throw (ex-info "Effect defect encountered" {:defect err}))))))
  ([msg data]
   (make-effect :die nil
                (fn [_]
                  (throw (ex-info msg data))))))

(defn or-die>
  "Converts any upstream domain failure into a fatal defect exception.
   If upstream succeeded, propagates value unmodified.

   Supports point-free pipeline usage and custom error messages or formatters:
     (-> (fx/fail> :not-found {:id 42})
         (fx/or-die>))

     (-> (fx/fail> :not-found {:id 42})
         (fx/or-die> \"Failed to load entity\"))"
  ([]
   (or-die> nil))
  ([prev-effect]
   (or-die> prev-effect nil))
  ([prev-effect msg-or-fn]
   (make-effect :or-die prev-effect
                (fn [value]
                  (if (failure? value)
                    (let [val-map (failure->value value)
                          err-data (error-data value)]
                      (cond
                        (fn? msg-or-fn)
                        (let [res (msg-or-fn value)]
                          (if (instance? #?(:clj Throwable :cljs js/Error) res)
                            (throw res)
                            (throw (ex-info (if (string? res) res "Effect terminated with fatal defect")
                                            (if (map? res) res val-map)))))

                        (string? msg-or-fn)
                        (throw (ex-info msg-or-fn val-map))

                        (instance? #?(:clj Throwable :cljs js/Error) err-data)
                        (throw (ex-info "Effect terminated with fatal defect" val-map err-data))

                        :else
                        (throw (ex-info "Effect terminated with fatal defect" val-map))))
                    value)))))

(defn match>
  "Converges failure and success channels by applying `on-failure` or `on-success`.
   `on-failure` receives the Failure record and `on-success` receives the success value.
   Both can be functions returning values or effects, or standalone effects.

   Supports point-free pipeline usage:
     (-> (fetch-user> id)
         (fx/match>
           (fn [err] (render-error-view err))
           (fn [user] (render-profile-view user))))"
  ([on-failure on-success]
   (match> nil on-failure on-success))
  ([prev-effect on-failure on-success]
   (make-effect :match prev-effect
                (fn [value]
                  (if (failure? value)
                    (eval-eff-or-fn on-failure value)
                    (eval-eff-or-fn on-success value))))))

(defn or-else>
  "Executes `fallback-eff` if upstream evaluates to a failure.
   Propagates success value unmodified.

   Supports point-free pipeline usage:
     (-> (fetch-from-cache> id)
         (fx/or-else> (fetch-from-remote-db> id)))

     (-> (fetch-from-cache> id)
         (fx/or-else> (fn [err] (fetch-from-remote-db> id))))"
  ([fallback-eff]
   (or-else> nil fallback-eff))
  ([prev-effect fallback-eff]
   (make-effect :or-else prev-effect
                (fn [value]
                  (if (failure? value)
                    (eval-eff-or-fn fallback-eff value)
                    value)))))



(defn or-else-fail>
  "Replaces any upstream failure with a new Failure having `tag` and `error-data`
   (or the provided failure / function result). Propagates success value unmodified.

   Example:
     (-> (fetch-user> id)
         (fx/or-else-fail> :user-not-found {:id id}))"
  ([fallback-failure]
   (or-else-fail> nil fallback-failure))
  ([a b]
   (if (keyword? a)
     (or-else-fail> nil a b)
     (make-effect :or-else-fail a
                  (fn [value]
                    (if (failure? value)
                      (if (failure? b)
                        b
                        (if (ifn? b)
                          (let [res (b value)]
                            (if (failure? res) res (make-failure :or-else-fail res)))
                          (make-failure :or-else-fail b)))
                      value)))))
  ([prev-effect tag error-data]
   (make-effect :or-else-fail prev-effect
                (fn [value]
                  (if (failure? value)
                    (make-failure tag (if (fn? error-data) (error-data value) error-data))
                    value)))))

(defn retry>
  "Retries an effect according to policy `{:max-attempts n :delay-ms ms :backoff-factor f :retry-if pred}`.

   Supports standalone execution and pipeline usage:
     (fx/retry> (flaky-effect) {:max-attempts 3 :delay-ms 100})
     (-> (flaky-effect) (fx/retry> {:max-attempts 3 :delay-ms 100 :backoff-factor 2.0}))
     (-> (succeed> id) (fx/retry> (fn [id] (fetch-user-by-id id)) {:max-attempts 3}))"
  ([policy]
   (retry> nil nil policy))
  ([target-or-policy policy-or-nil]
   (if (and (map? target-or-policy) (not (effect? target-or-policy)) (nil? policy-or-nil))
     (retry> nil nil target-or-policy)
     (let [target-effect target-or-policy
           policy (or policy-or-nil {})]
       (make-effect :retry nil
                    (fn [value]
                      (let [eff (if (and (some? value) (effect? target-effect) (nil? (:prev-effect target-effect)))
                                  (chain> (succeed> value) target-effect)
                                  target-effect)]
                        (loop [attempt 1
                               curr-delay (long (get policy :delay-ms 0))]
                          (let [res (eval-eff-or-fn eff value)]
                            (if (and (failure? res)
                                     (< attempt (get policy :max-attempts 3))
                                     (if-let [pred (:retry-if policy)] (pred res) true))
                              (do
                                #?(:clj  (when (pos? curr-delay) (Thread/sleep curr-delay))
                                   :cljs nil)
                                (recur (inc attempt) (long (* curr-delay (get policy :backoff-factor 1.0)))))
                              res)))))))))
  ([prev-effect target-effect policy]
   (make-effect :retry prev-effect
                (fn [value]
                  (loop [attempt 1
                         curr-delay (long (get policy :delay-ms 0))]
                    (let [res (eval-eff-or-fn target-effect value)]
                      (if (and (failure? res)
                               (< attempt (get policy :max-attempts 3))
                               (if-let [pred (:retry-if policy)] (pred res) true))
                        (do
                          #?(:clj  (when (pos? curr-delay) (Thread/sleep curr-delay))
                             :cljs nil)
                          (recur (inc attempt) (long (* curr-delay (get policy :backoff-factor 1.0)))))
                        res)))))))



(defn try>
  "Creates an effect that executes an inner effect combinator, catching any thrown
   exceptions (Throwable on Clojure, :default on ClojureScript) and converting them into typed failures.

   Supports point-free pipeline usage, standalone evaluation, custom keyword failure types,
   custom catch effect/function handlers, and options maps:
     (-> (succeed> \"10\") (try> (map> parse-long)))
     (-> (succeed> \"abc\") (try> (map> parse-long) :parse-error))
     (-> (succeed> \"abc\") (try> (map> parse-long) (map> (fn [e] (make-failure :parse-error (.getMessage e))))))
     (try> (map> #(slurp \"file.txt\")))
     (try> (map> #(slurp \"file.txt\")) :io-error)
     (try> {:try (map> #(slurp \"file.txt\")) :catch :io-error})"
  ([effect-or-opts]
   (if (and (map? effect-or-opts) (contains? effect-or-opts :try))
     (try> nil (:try effect-or-opts) (:catch effect-or-opts))
     (try> nil effect-or-opts nil)))
  ([a b]
   (cond
     (and (effect? a) (effect? b))
     (try> a b nil)

     (and (effect? a) (map? b) (contains? b :try))
     (try> a (:try b) (:catch b))

     (effect? a)
     (try> nil a b)

     :else
     (try> nil a b)))
  ([prev-effect body-effect catch-handler]
   (let [eff (if (effect? body-effect)
               body-effect
               (make-effect :try-body nil
                            (fn [v]
                              (if (fn? body-effect)
                                (try
                                  (body-effect v)
                                  (catch #?(:clj clojure.lang.ArityException :cljs :default) _
                                    (body-effect)))
                                body-effect))))]
     (make-effect :try
                  prev-effect
                  (fn [value]
                    (maybe-propagate-failure value
                                             (try
                                               (-run! (chain> (succeed> value) eff))
                                               (catch #?(:clj Throwable :cljs :default) e
                                                 (handle-try-exception catch-handler e)))))))))

(defn all>
  "Combines a vector or sequence of independent effects into a single effect that returns
   a vector of their evaluated results.

   Example:
     (fx/all> [(fx/succeed> 1)
               (fx/succeed> 2)])
     ;; => [1 2]"
  [effects]
  (make-effect
    :all
    nil
    (fn [_]
      (->> effects
           (mapv (fn [eff] (-run! eff)))))))

(defn for-each>
  "Maps a collection through an effect-producing function `f-eff` and collects results into a vector.
   Short-circuits on the first failure.

   Supports standalone execution and point-free pipeline threading:
     (fx/for-each> [1 2 3] (fn [id] (fetch-user-by-id> id)))
     (-> (fx/succeed> [1 2 3])
         (fx/for-each> (fn [id] (fetch-user-by-id> id))))"
  ([f-eff]
   (for-each> nil f-eff))
  ([a b]
   (if (and (or (sequential? a) (set? a) (nil? a)) (not (effect? a)))
     (make-effect :for-each nil
                  (fn [_]
                    (reduce (fn [acc item]
                              (let [res (eval-eff-or-fn b item)]
                                (if (failure? res)
                                  (reduced res)
                                  (conj acc res))))
                            []
                            (or a []))))
     (make-effect :for-each a
                  (fn [value]
                    (maybe-propagate-failure value
                                             (reduce (fn [acc item]
                                                       (let [res (eval-eff-or-fn b item)]
                                                         (if (failure? res)
                                                           (reduced res)
                                                           (conj acc res))))
                                                     []
                                                     (or value [])))))))
  ([prev-effect coll f-eff]
   (make-effect :for-each prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (reduce (fn [acc item]
                                                     (let [res (eval-eff-or-fn f-eff item)]
                                                       (if (failure? res)
                                                         (reduced res)
                                                         (conj acc res))))
                                                   []
                                                   (or (if (some? coll) coll value) [])))))))

(defn zip-with>
  "Evaluates two effects and combines their successful results using binary function `f`.
   Short-circuits if either effect fails.

   Example:
     (fx/zip-with> (fetch-user> id) (fetch-perms> id)
                   (fn [u p] (assoc u :permissions p)))"
  ([eff-a eff-b f]
   (make-effect :zip-with nil
                (fn [value]
                  (let [a (if (and (some? value) (effect? eff-a) (nil? (:prev-effect eff-a)))
                            (chain> (succeed> value) eff-a)
                            eff-a)
                        res-a (eval-eff-or-fn a value)]
                    (if (failure? res-a)
                      res-a
                      (let [b (if (and (some? value) (effect? eff-b) (nil? (:prev-effect eff-b)))
                                (chain> (succeed> value) eff-b)
                                eff-b)
                            res-b (eval-eff-or-fn b value)]
                        (if (failure? res-b)
                          res-b
                          (f res-a res-b))))))))
  ([prev-effect eff-a eff-b f]
   (make-effect :zip-with prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (let [res-a (eval-eff-or-fn eff-a value)]
                                             (if (failure? res-a)
                                               res-a
                                               (let [res-b (eval-eff-or-fn eff-b value)]
                                                 (if (failure? res-b)
                                                   res-b
                                                   (f res-a res-b))))))))))

(defn zip>
  "Evaluates two effects and pairs their results into a 2-element vector `[res-a res-b]`.
   Short-circuits if either effect fails.

   Example:
     (fx/zip> (fetch-user> id) (fetch-profile> id))"
  ([eff-a eff-b]
   (zip-with> eff-a eff-b vector))
  ([prev-effect eff-a eff-b]
   (zip-with> prev-effect eff-a eff-b vector)))

(defn mapcat>
  "Flat-maps over an effect by applying an inner effect combinator to the successful value.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/mapcat> (fx/map> inc)))"
  ([inner-effect]
   (mapcat> nil inner-effect))
  ([prev-effect inner-effect]
   (make-effect :mapcat
                prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (if (effect? inner-effect)
                                             ; Maybe should eval with nil here?
                                             (-run! (chain> (succeed> value)
                                                            inner-effect))
                                             (make-failure :mapcat>-result-not-an-effect
                                                           {:result inner-effect})))))))

(defn if>
  "Conditional branching combinator. Evaluates `cond-eff` with the current value:
   if truthy, evaluates `then-effect`; otherwise evaluates `else-effect`.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/if> (fx/map> even?)
                 (fx/map> inc)
                 (fx/map> dec)))"
  ([cond-eff then-effect else-effect]
   (if> nil cond-eff then-effect else-effect))
  ([prev-effect cond-eff then-effect else-effect]
   (make-effect :if
                prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           (let [branch-eff (if (-run! (chain> (succeed> value)
                                                                               cond-eff))
                                                              then-effect
                                                              else-effect)]
                                             (-run! (chain> (succeed> value)
                                                            branch-eff))))))))

(defn cond>
  "Multi-branch conditional combinator. Takes paired test and expression effects:
   `[prev-effect test-1 expr-1 test-2 expr-2 ...]`.
   Evaluates tests in order with the current value; when a test evaluates to a truthy
   value, executes the corresponding expression effect.

   If no test matches, returns a Failure with tag `:cond` and error-data `:no-conditions`.
   Short-circuits if the upstream effect yielded a failure.

   Example:
     (-> (fx/succeed> 10)
         (fx/cond>
           (fx/map> odd?)  (fx/map> inc)
           (fx/map> even?) (fx/map> dec)))"
  [prev-effect & conditions]
  (let [conditions (partition 2 conditions)]
    (make-effect :cond
                 prev-effect
                 (fn [value]
                   (maybe-propagate-failure value
                                            (let [res-eff (loop [conditions conditions]
                                                            (if-let [[test-eff expr-effect] (first conditions)]
                                                              (if (-run! (chain> (succeed> value)
                                                                                 test-eff))
                                                                expr-effect
                                                                (recur (rest conditions)))
                                                              (fail> :cond :no-conditions)))]
                                              (-run! (chain> (succeed> value)
                                                             res-eff))))))))

(defn catch>
  "Catches specific failure tags using a handler map `f-map` of `{failure-tag handler-effect}`.
   When a failure matches a key in `f-map`, executes the corresponding handler effect passing the
   failure's `error-data` as input. Unmatched failures and successful values pass through untouched.

   Example:
     (-> (fx/fail> :not-found {:id 42})
         (fx/catch> {:not-found (fx/map> (fn [{:keys [id]}] (str \"User \" id \" missing\")))}))"
  ([f-map]
   (catch> nil f-map))
  ([prev-effect f-map]
   (make-effect :catch
                prev-effect
                (fn [value]
                  (if (failure? value)
                    (let [failure-tag (tag value)
                          error-data (error-data value)
                          f-effect (get f-map failure-tag)]
                      (if f-effect
                        ; maybe it should be (f value)
                        (-run! (chain> (succeed> error-data)
                                       f-effect))
                        value))
                    value)))))

(defn failure->value
  "Converts an IFailure instance into a plain map `{:tag ... :error-data ...}`."
  [failure]
  {:tag        (tag failure)
   :error-data (error-data failure)})

(defn catchall>
  "Catches any failure and routes its map representation `{:tag ... :error-data ...}` as input
   to `inner-effect`. Successful values pass through untouched.

   Example:
     (-> (fx/fail> :error {:code 500})
         (fx/catchall> (fx/map> (fn [{:keys [tag error-data]}] (str \"Caught: \" tag))))))"
  ([inner-effect]
   (catchall> nil inner-effect))
  ([prev-effect inner-effect]
   (make-effect :catch-all
                prev-effect
                (fn [value]
                  (if (failure? value)
                    (-run! (chain> (succeed> (failure->value value))
                                   inner-effect))
                    value)))))

(defn run-sync!
  "Synchronously evaluates an effect pipeline from root to leaf and returns the final value
   or failure. Binds `*context*` with optional initial `context` (or merged with active context)
   and ensures `:runner run-sync!` is present during execution.

   Examples:
     (fx/run-sync! (fx/succeed> 42))
     ;; => 42

     (fx/run-sync! (fx/service> :db) {:db mock-db})
     ;; => mock-db"
  ([effect]
   (run-sync! effect {}))
  ([effect context]
   (binding [*context* (assoc (merge *context* context) :runner run-sync!)]
     (->> effect
          (iterate prev-effect)
          (take-while some?)
          (reverse)
          (reduce (fn [acc effect]
                    (-eval! effect acc))
                  nil)))))

(defn sleep>
  "Creates an effect that pauses evaluation for `ms` milliseconds and propagates upstream value."
  ([ms]
   (sleep> nil ms))
  ([prev-effect ms]
   (make-effect :sleep prev-effect
                (fn [value]
                  (maybe-propagate-failure value
                                           #?(:clj  (when (pos? ms) (Thread/sleep ms))
                                              :cljs nil)
                                           value)))))

(defn run-async!
  "Asynchronously evaluates an effect pipeline returning a java.util.concurrent.CompletableFuture (Clojure)
   or a js/Promise (ClojureScript).

   Examples:
     @(fx/run-async! (fx/succeed> 42))
     ;; => 42"
  ([effect]
   (run-async! effect {}))
  ([effect context]
   #?(:clj
      (CompletableFuture/supplyAsync
        (reify Supplier
          (get [_]
            (run-sync! effect context))))
      :cljs
      (js/Promise.
        (fn [resolve reject]
            (try
              (resolve (run-sync! effect context))
              (catch :default e
                (reject e))))))))