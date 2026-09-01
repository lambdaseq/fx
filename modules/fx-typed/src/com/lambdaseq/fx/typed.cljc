(ns com.lambdaseq.fx.typed
  (:require [com.lambdaseq.fx.core :as fx]
            [typed.clojure :as t]))

(t/defalias Context '{})

(t/ann-protocol [[failure-type :< t/Keyword :variance :covariant]
                 [error :variance :covariant]] fx/IFailure
  -failure-type [(fx/IFailure failure-type error) -> failure-type]
  -error [(fx/IFailure failure-type error) -> error])

(t/ann-protocol [[in :variance :contravariant]
                 [out :variance :covariant]
                 [failure :< (t/Option (fx/IFailure t/Keyword t/Any)) :variance :covariant]
                 [context :< Context :variance :covariant]] fx/IEffect
  -effect-type [(fx/IEffect in out failure context)
                -> t/Keyword]
  -prev-effect [(fx/IEffect in out failure context) -> (t/Option (fx/IEffect t/Any in failure Context))]
  -eval! [(fx/IEffect in out failure context)
          -> out])

(t/ann fx/effect? [t/Any -> Boolean])

(t/ann fx/failure? [t/Any -> Boolean])

(t/ann fx/make-effect
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              [t/Keyword
               (t/Option (fx/IEffect t/Nothing in failure context))
               [in -> out]
               -> (fx/IEffect in out failure context)]))

(t/ann fx/make-failure (t/All [[key :< t/Keyword] error]
                              [key error
                               -> (fx/IFailure key error)]))

(t/ann fx/failure->value
       (t/All [[key :< t/Keyword] error]
              [(fx/IFailure key error)
               -> (t/HMap :mandatory {:type key :error-data error})]))

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

(t/ann fx/do>
       (t/All [out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
                [[out -> t/Any]
                 -> (fx/IEffect out out nil Context)]
                [(fx/IEffect t/Any out failure context)
                 [out -> t/Any]
                 -> (fx/IEffect t/Any out failure context)])))

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
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [inner-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
                [(fx/IEffect in out inner-failure context)
                 -> (fx/IEffect in out inner-failure context)]
                [(fx/IEffect t/Any in prev-failure context)
                 (fx/IEffect in out inner-failure context)
                 -> (fx/IEffect t/Any out (t/U prev-failure inner-failure) context)])))

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
               [failure-type :< t/Keyword]
               error
               [handler-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
                [(t/Map failure-type (fx/IEffect error out handler-failure context))
                 -> (fx/IEffect in (t/U in out) handler-failure context)]
                [(fx/IEffect t/Any in (fx/IFailure failure-type error) context)
                 (t/Map failure-type (fx/IEffect error out handler-failure context))
                 -> (fx/IEffect t/Any (t/U in out) handler-failure context)])))

(t/ann fx/catchall>
       (t/All [in out
               [prev-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [handler-failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn
                [(fx/IEffect (t/HMap :mandatory {:type t/Keyword :error-data t/Any}) out handler-failure context)
                 -> (fx/IEffect in (t/U in out) handler-failure context)]
                [(fx/IEffect t/Any in prev-failure context)
                 (fx/IEffect (t/HMap :mandatory {:type t/Keyword :error-data t/Any}) out handler-failure context)
                 -> (fx/IEffect t/Any (t/U in out) handler-failure context)])))

(t/ann fx/run-sync!
       (t/All [in out
               [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
               [context :< Context]]
              (t/IFn [(fx/IEffect in out nil context)
                      -> out]
                     [(fx/IEffect in out nil context)
                      in -> out]
                     [(fx/IEffect in out failure context)
                      -> (t/U out failure)]
                     [(fx/IEffect in out failure context)
                      in -> (t/U out failure)])))