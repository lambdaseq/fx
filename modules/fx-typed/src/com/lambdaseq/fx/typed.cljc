(ns com.lambdaseq.fx.typed
  (:require [com.lambdaseq.fx.core :as fx]
            [typed.clojure :as t]))

(t/defalias Context (t/HMap :mandatory {}))

(t/ann-protocol [[failure-type :< t/Keyword :variance :covariant]
                 [error :variance :covariant]] fx/IFailure
  -failure-type [(fx/IFailure failure-type error) -> failure-type]
  -error [(fx/IFailure failure-type error) -> error])

(t/ann-protocol [[in :variance :covariant]
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

(t/ann fx/make-effect (t/All [in out
                              [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
                              [context :< Context]]
                             [t/Keyword
                              (t/Option (fx/IEffect t/Any in failure context))
                              [in -> out]
                              -> (fx/IEffect in out failure context)]))

(t/ann fx/make-failure (t/All [[key :< t/Keyword] error]
                              [key error
                               -> (fx/IFailure key error)]))

(t/ann fx/succeed> (t/All [x] [x -> (fx/IEffect nil x nil Context)]))

(t/ann fx/input> (t/All [x] [-> (fx/IEffect x x nil (t/Assoc Context ':input x))]))

(t/ann fx/fail>
       (t/All [[key :< t/Keyword] x]
              (t/IFn
                [x -> (fx/IEffect t/Any t/Nothing (fx/IFailure (t/Value :fail) x) Context)]
                [key x -> (fx/IEffect t/Any t/Nothing (fx/IFailure key x) Context)])))

(t/ann fx/map> (t/All [in out
                       [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
                       [context :< Context]]
                      [[in -> out]
                       (fx/IEffect t/Any in failure context)
                       -> (fx/IEffect t/Any out failure context)]))

(t/ann fx/do> (t/All [out
                      [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
                      [context :< Context]]
                     [[out -> t/Any]
                      (fx/IEffect t/Any out failure context)
                      -> (fx/IEffect t/Any out failure context)]))

(t/ann fx/mapcat> (t/All [in out
                          [failure :< (t/Option (fx/IFailure t/Keyword t/Any))]
                          [context :< Context]]
                         [(fx/IEffect t/Any out failure (t/Assoc context ':input in))
                          (fx/IEffect t/Any in failure context)
                          -> (fx/IEffect t/Any out failure context)]))

(t/ann fx/if> (t/All [in then-out else-out
                      failure then-failure else-failure
                      [context :< Context]]
                     [[in -> Boolean]
                      (fx/IEffect in then-out then-failure (t/Assoc context ':input in))
                      (fx/IEffect in else-out else-failure (t/Assoc context ':input in))
                      (fx/IEffect t/Any in failure context)
                      -> (fx/IEffect in (t/U then-out else-out) failure context)]))

(t/ann fx/catch> (t/All [in out
                         [failure :< (fx/IFailure t/Any error)]
                         [context :< Context]]
                        [(t/HMap t/Keyword (fx/IEffect t/Any out
                                             ; TODO: Here nil is not correct, it should be the union of all failures that are not handled
                                             nil
                                             (t/Assoc context ':input error)))
                         (fx/IEffect t/any in prev-failure context)
                         -> (fx/IEffect in out prev-failure context)]))

(t/ann fx/catchall> (t/All [in out
                            [failure :< (fx/IFailure t/Any error)]
                            [context :< Context]]
                           [(fx/IEffect t/Any out nil (t/Assoc context ':input error))
                            (fx/IEffect t/Any in prev-failure context)
                            -> (fx/IEffect in out nil context)]))

(t/ann fx/run-sync! (t/All [in out [context :< Context]]
                           (t/IFn [(fx/IEffect in out t/Nothing context)
                                   -> out]
                                  [(fx/IEffect in out t/Nothing context)
                                   in -> out])))