(ns fx.typed
  (:require ^:clj-kondo/ignore [fx.core :as fx]
            [typed.clojure :as t]))

(t/defalias Context '{})

(t/ann-protocol [[failure-tag :< t/Keyword :variance :covariant]
                 [error :variance :covariant]] fx/IFailure
                -error [(fx/IFailure failure-tag error) -> error])

(t/ann-protocol [[tag :< t/Keyword :variance :covariant]] fx/ITagged
                -tag [(fx/ITagged tag) -> tag])

(t/ann-protocol [[in :variance :contravariant]
                 [out :variance :covariant]
                 [failure :< (t/Option (fx/IFailure t/Keyword t/Any)) :variance :covariant]
                 [context :< Context :variance :covariant]] fx/IEffect
                -prev-effect [(fx/IEffect in out failure context) -> (t/Option (fx/IEffect t/Any in failure Context))]
                -step [(fx/IEffect in out failure context) in context (t/List t/Any)
                       -> (t/Vec t/Any)])

(t/ann fx/effect? [t/Any -> Boolean])

(t/ann fx/failure? [t/Any -> Boolean])

(t/ann fx/make-effect
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              [t/Keyword
               (t/Option (fx/IEffect t/Nothing in failure context))
               (t/Map t/Keyword t/Any)
               -> (fx/IEffect in out failure context)]))

(t/ann fx/make-failure (t/All [[key :< t/Keyword] error]
                              [key error
                               -> (fx/IFailure key error)]))

(t/ann fx/failure->value
       (t/All [[key :< t/Keyword] error]
              [(fx/IFailure key error)
               -> (t/HMap :mandatory {:tag key :error-data error})]))

(t/ann fx/chain>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [current-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              [(fx/IEffect t/Any in prev-failure context)
               (fx/IEffect in out current-failure context)
               -> (fx/IEffect t/Any out (t/U prev-failure current-failure) context)]))

(t/ann fx/succeed> (t/All [x] [x -> (fx/IEffect t/Any x nil Context)]))

(t/ann fx/fail>
       (t/All [[key :< t/Keyword] x]
              (t/IFn
               [x -> (fx/IEffect t/Any t/Nothing (fx/IFailure (t/Val :fail) x) Context)]
               [key x -> (fx/IEffect t/Any t/Nothing (fx/IFailure key x) Context)])))

(t/ann fx/map>
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [[in -> out]
                -> (fx/IEffect in out nil Context)]
               [(fx/IEffect t/Any in failure context)
                [in -> out]
                -> (fx/IEffect t/Any out failure context)])))

(t/ann fx/map-ctx>
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [[in context -> out]
                -> (fx/IEffect in out nil Context)]
               [(fx/IEffect t/Any in failure context)
                [in context -> out]
                -> (fx/IEffect t/Any out failure context)])))

(t/ann fx/tap>
       (t/All [[out :< t/Any]
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [[out -> t/Any]
                -> (fx/IEffect out out nil Context)]
               [(fx/IEffect t/Any out failure context)
                [out -> t/Any]
                -> (fx/IEffect t/Any out failure context)])))

(t/ann fx/tap-error>
       (t/All [[out :< t/Any]
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [[failure -> t/Any]
                -> (fx/IEffect out out failure Context)]
               [(fx/IEffect t/Any out failure context)
                [failure -> t/Any]
                -> (fx/IEffect t/Any out failure context)])))

(t/ann fx/do-ctx>
       (t/All [[out :< t/Any]
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [[out context -> t/Any]
                -> (fx/IEffect out out nil Context)]
               [(fx/IEffect t/Any out failure context)
                [out context -> t/Any]
                -> (fx/IEffect t/Any out failure context)])))

(t/ann fx/context>
       (t/All [out [context :< Context]]
              (t/IFn
               [-> (fx/IEffect t/Any context nil context)]
               [t/Any -> (fx/IEffect t/Any out nil context)]
               [t/Any out -> (fx/IEffect t/Any out nil context)])))

(t/ann fx/service>
       (t/All [out [context :< Context]]
              (t/IFn
               [t/Any -> (fx/IEffect t/Any out nil context)]
               [t/Any out -> (fx/IEffect t/Any out nil context)])))

(t/ann fx/provide>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [body-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [provided-ctx :< Context]
               [context :< Context]]
              (t/IFn
               [provided-ctx
                -> (fx/IEffect t/Any t/Any nil context)]
               [(fx/IEffect in out body-failure context)
                provided-ctx
                -> (fx/IEffect in out body-failure context)]
               [provided-ctx
                (fx/IEffect in out body-failure context)
                -> (fx/IEffect in out body-failure context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect in out body-failure context)
                provided-ctx
                -> (fx/IEffect t/Any out (t/U prev-failure body-failure) context)])))

(t/ann fx/provide-service>
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [key :< t/Keyword]
               service-impl
               [context :< Context]]
              (t/IFn
               [key service-impl
                -> (fx/IEffect t/Any t/Any nil context)]
               [(fx/IEffect in out failure context)
                key service-impl
                -> (fx/IEffect in out failure context)]
               [key service-impl
                (fx/IEffect in out failure context)
                -> (fx/IEffect in out failure context)])))

(t/ann fx/ensure>
       (t/All [[out :< t/Any]
               [finalizer-out :< t/Any]
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [finalizer-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [(fx/IEffect t/Any finalizer-out finalizer-failure context)
                -> (fx/IEffect t/Any t/Any (t/U finalizer-failure (fx/IFailure (t/Val :ensure) t/Any)) context)]
               [(fx/IEffect t/Any out prev-failure context)
                (fx/IEffect t/Any finalizer-out finalizer-failure context)
                -> (fx/IEffect t/Any out (t/U prev-failure finalizer-failure (fx/IFailure (t/Val :ensure) t/Any)) context)]
               [[out -> t/Any]
                -> (fx/IEffect out out (t/Option (fx/IFailure (t/Val :ensure) t/Any)) Context)]
               [(fx/IEffect t/Any out prev-failure context)
                [out -> t/Any]
                -> (fx/IEffect t/Any out (t/U prev-failure (fx/IFailure (t/Val :ensure) t/Any)) context)])))

(t/ann fx/try>
       (t/All [[in :< t/Any]
               [out :< t/Any]
               [handler-out :< t/Any]
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [body-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [handler-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [key :< t/Keyword]
               [context :< Context]]
              (t/IFn
               [(fx/IEffect in out body-failure context)
                -> (fx/IEffect in out (t/U body-failure (fx/IFailure (t/Val :try) t/Any)) context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect in out body-failure context)
                -> (fx/IEffect t/Any out (t/U prev-failure body-failure (fx/IFailure (t/Val :try) t/Any)) context)]
               [(fx/IEffect in out body-failure context)
                key
                -> (fx/IEffect in out (t/U body-failure (fx/IFailure key t/Any)) context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect in out body-failure context)
                key
                -> (fx/IEffect t/Any out (t/U prev-failure body-failure (fx/IFailure key t/Any)) context)]
               [(fx/IEffect in out body-failure context)
                (fx/IEffect t/Any handler-out handler-failure context)
                -> (fx/IEffect in (t/U out handler-out) (t/U body-failure handler-failure) context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect in out body-failure context)
                (fx/IEffect t/Any handler-out handler-failure context)
                -> (fx/IEffect t/Any (t/U out handler-out) (t/U prev-failure body-failure handler-failure) context)]
               [(fx/IEffect in out body-failure context)
                [t/Any -> handler-failure]
                -> (fx/IEffect in out (t/U body-failure handler-failure) context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect in out body-failure context)
                [t/Any -> handler-failure]
                -> (fx/IEffect t/Any out (t/U prev-failure body-failure handler-failure) context)])))

(t/ann fx/mapcat>
       (t/All [in out]
              (t/IFn
               [[in -> (fx/IEffect t/Any out nil Context)]
                -> (fx/IEffect in out nil Context)]
               [[in -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                -> (fx/IEffect in out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
               [(fx/IEffect t/Any in nil Context)
                [in -> (fx/IEffect t/Any out nil Context)]
                -> (fx/IEffect t/Any out nil Context)]
               [(fx/IEffect t/Any in (t/Option (fx/IFailure t/Keyword t/Any)) Context)
                [in -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)])))

(t/ann fx/if>
       (t/All [in then-out else-out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [cond-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [then-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [else-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [(fx/IEffect in Boolean cond-failure context)
                (fx/IEffect in then-out then-failure context)
                (fx/IEffect in else-out else-failure context)
                -> (fx/IEffect in (t/U then-out else-out) (t/U cond-failure then-failure else-failure) context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect in Boolean cond-failure context)
                (fx/IEffect in then-out then-failure context)
                (fx/IEffect in else-out else-failure context)
                -> (fx/IEffect t/Any (t/U then-out else-out) (t/U prev-failure cond-failure then-failure else-failure) context)])))

(t/ann fx/cond>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              [(fx/IEffect t/Any in prev-failure context)
               (t/U (fx/IEffect in Boolean nil context) (fx/IEffect in out nil context)) :*
               -> (fx/IEffect t/Any out (t/U prev-failure (fx/IFailure (t/Val :cond) (t/Val :no-conditions))) context)]))

(t/ann fx/all>
       (t/All [x]
              [(t/Vec (fx/IEffect t/Any x nil Context))
               -> (fx/IEffect t/Any (t/Vec x) nil Context)]))

(t/ann fx/catch>
       (t/All [in out
               [failure-tag :< t/Keyword]
               error
               [handler-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [(t/Map failure-tag (fx/IEffect error out handler-failure context))
                -> (fx/IEffect in (t/U in out) handler-failure context)]
               [(fx/IEffect t/Any in (fx/IFailure failure-tag error) context)
                (t/Map failure-tag (fx/IEffect error out handler-failure context))
                -> (fx/IEffect t/Any (t/U in out) handler-failure context)])))

(t/ann fx/catchall>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [handler-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [(fx/IEffect (t/HMap :mandatory {:tag t/Keyword :error-data t/Any}) out handler-failure context)
                -> (fx/IEffect in (t/U in out) handler-failure context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect (t/HMap :mandatory {:tag t/Keyword :error-data t/Any}) out handler-failure context)
                -> (fx/IEffect t/Any (t/U in out) handler-failure context)])))

(t/ann fx/run-sync!
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [run-ctx :< Context]
               [context :< Context]]
              (t/IFn [(fx/IEffect in out nil context)
                      -> out]
                     [(fx/IEffect in out nil context)
                      run-ctx -> out]
                     [(fx/IEffect in out failure context)
                      -> (t/U out failure)]
                     [(fx/IEffect in out failure context)
                      run-ctx -> (t/U out failure)])))

(t/ann fx/acquire-release>
       (t/All [res out]
              (t/IFn
               [(fx/IEffect t/Any res nil Context)
                [res -> (fx/IEffect t/Any out nil Context)]
                [res -> (fx/IEffect t/Any t/Any nil Context)]
                -> (fx/IEffect t/Any out nil Context)]
               [(fx/IEffect t/Any res (t/Option (fx/IFailure t/Keyword t/Any)) Context)
                [res -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                [res -> (fx/IEffect t/Any t/Any (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
               [[res -> (fx/IEffect t/Any out nil Context)]
                [res -> (fx/IEffect t/Any t/Any nil Context)]
                -> (fx/IEffect res out nil Context)]
               [[res -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                [res -> (fx/IEffect t/Any t/Any (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                -> (fx/IEffect res out (t/Option (fx/IFailure t/Keyword t/Any)) Context)])))

(t/ann fx/die>
       (t/IFn
        [-> (fx/IEffect t/Any t/Nothing nil Context)]
        [t/Any -> (fx/IEffect t/Any t/Nothing nil Context)]
        [t/Str t/Any -> (fx/IEffect t/Any t/Nothing nil Context)]))

(t/ann fx/or-die>
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [-> (fx/IEffect in in nil Context)]
               [(fx/IEffect in out failure context)
                -> (fx/IEffect in out nil context)]
               [(fx/IEffect in out failure context)
                t/Any
                -> (fx/IEffect in out nil context)])))

(t/ann fx/match>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [on-fail-out :< t/Any]
               [on-succ-out :< t/Any]
               [context :< Context]]
              (t/IFn
               [[prev-failure -> on-fail-out]
                [out -> on-succ-out]
                -> (fx/IEffect out (t/U on-fail-out on-succ-out) nil Context)]
               [(fx/IEffect t/Any out prev-failure context)
                [prev-failure -> on-fail-out]
                [out -> on-succ-out]
                -> (fx/IEffect t/Any (t/U on-fail-out on-succ-out) nil context)])))

(t/ann fx/or-else>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [fallback-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [fallback-out :< t/Any]
               [context :< Context]]
              (t/IFn
               [(fx/IEffect t/Any fallback-out fallback-failure context)
                -> (fx/IEffect in (t/U in fallback-out) fallback-failure context)]
               [(fx/IEffect t/Any out prev-failure context)
                (fx/IEffect t/Any fallback-out fallback-failure context)
                -> (fx/IEffect t/Any (t/U out fallback-out) fallback-failure context)])))

(t/ann fx/or-else-fail>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [key :< t/Keyword]
               err
               [context :< Context]]
              (t/IFn
               [(fx/IFailure key err)
                -> (fx/IEffect in in (fx/IFailure key err) Context)]
               [key err
                -> (fx/IEffect in in (fx/IFailure key err) Context)]
               [(fx/IEffect t/Any out prev-failure context)
                (fx/IFailure key err)
                -> (fx/IEffect t/Any out (fx/IFailure key err) context)]
               [(fx/IEffect t/Any out prev-failure context)
                key err
                -> (fx/IEffect t/Any out (fx/IFailure key err) context)])))

(t/ann fx/retry>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [(t/Map t/Any t/Any)
                -> (fx/IEffect in out failure context)]
               [(fx/IEffect in out failure context)
                (t/Map t/Any t/Any)
                -> (fx/IEffect in out failure context)]
               [(fx/IEffect t/Any in prev-failure context)
                (fx/IEffect in out failure context)
                (t/Map t/Any t/Any)
                -> (fx/IEffect t/Any out (t/U prev-failure failure) context)])))

(t/ann fx/for-each>
       (t/All [item out]
              (t/IFn
               [[item -> (fx/IEffect t/Any out nil Context)]
                -> (fx/IEffect (t/Vec item) (t/Vec out) nil Context)]
               [[item -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                -> (fx/IEffect (t/Vec item) (t/Vec out) (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
               [(t/Vec item)
                [item -> (fx/IEffect t/Any out nil Context)]
                -> (fx/IEffect t/Any (t/Vec out) nil Context)]
               [(t/Vec item)
                [item -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                -> (fx/IEffect t/Any (t/Vec out) (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
               [(fx/IEffect t/Any (t/Vec item) nil Context)
                [item -> (fx/IEffect t/Any out nil Context)]
                -> (fx/IEffect t/Any (t/Vec out) nil Context)]
               [(fx/IEffect t/Any (t/Vec item) (t/Option (fx/IFailure t/Keyword t/Any)) Context)
                [item -> (fx/IEffect t/Any out (t/Option (fx/IFailure t/Keyword t/Any)) Context)]
                -> (fx/IEffect t/Any (t/Vec out) (t/Option (fx/IFailure t/Keyword t/Any)) Context)])))

(t/ann fx/zip-with>
       (t/All [out-a out-b out
               [fail-a :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [fail-b :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [(fx/IEffect t/Any out-a fail-a context)
                (fx/IEffect t/Any out-b fail-b context)
                [out-a out-b -> out]
                -> (fx/IEffect t/Any out (t/U fail-a fail-b) context)])))

(t/ann fx/zip>
       (t/All [out-a out-b
               [fail-a :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [fail-b :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [(fx/IEffect t/Any out-a fail-a context)
                (fx/IEffect t/Any out-b fail-b context)
                -> (fx/IEffect t/Any (t/HVec [out-a out-b]) (t/U fail-a fail-b) context)])))

(t/ann fx/sleep>
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
               [Long -> (fx/IEffect in in nil context)]
               [(fx/IEffect in out failure context) Long
                -> (fx/IEffect in out failure context)])))

(t/ann fx/run-async!
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [run-ctx :< Context]
               [context :< Context]]
              (t/IFn [(fx/IEffect in out nil context)
                      -> (t/U java.util.concurrent.CompletableFuture t/Any)]
                     [(fx/IEffect in out nil context)
                      run-ctx -> (t/U java.util.concurrent.CompletableFuture t/Any)]
                     [(fx/IEffect in out failure context)
                      -> (t/U java.util.concurrent.CompletableFuture t/Any)]
                     [(fx/IEffect in out failure context)
                      run-ctx -> (t/U java.util.concurrent.CompletableFuture t/Any)])))
