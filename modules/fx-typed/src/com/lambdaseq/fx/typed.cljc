(ns com.lambdaseq.fx.typed
  (:require [com.lambdaseq.fx.core :as fx]
            [typed.clojure :as t]))

(t/ann-protocol [[in :variance :covariant]
                 [out :variance :covariant]
                 [failure :variance :covariant]] fx/IEffect
  -effect-type [(fx/IEffect in out failure)
                -> t/Keyword]
  -prev-effect [(fx/IEffect in out failure) -> (t/Option (fx/IEffect t/Any in failure))]
  -eval! [(fx/IEffect in out failure)
          -> out])

(t/ann-protocol [[failure-type :< t/Keyword :variance :covariant]
                 [error :variance :covariant]] fx/IFailure
  -failure-type [(fx/IFailure error) -> failure-type]
  -error [(fx/IFailure error) -> error])

(t/ann fx/effect? [t/Any -> Boolean])

(t/ann fx/failure? [t/Any -> Boolean])

(t/ann fx/make-effect (t/All [in out failure]
                             [t/Keyword
                              (t/Option (fx/IEffect t/Any in failure))
                              [in -> out]
                              -> (fx/IEffect in out failure)]))

(t/ann fx/make-failure (t/All [[key :< t/Keyword] error]
                              [key error
                               -> (fx/IFailure key error)]))

(t/ann fx/succeed> (t/All [x] [x -> (fx/IEffect nil x nil)]))

(t/ann fx/input> (t/All [x] [-> (fx/IEffect x x nil)]))

(t/ann fx/fail>
       (t/All [[key :< t/Keyword] x]
              (t/IFn
                [x -> (fx/IEffect t/Any t/Nothing (fx/IFailure (t/Value :fail) x))]
                [key x -> (fx/IEffect t/Any t/Nothing (fx/IFailure key x))])))

(t/ann fx/map> (t/All [in out failure]
                      [[in -> out]
                       (fx/IEffect t/Any in failure)
                       -> (fx/IEffect t/Any out failure)]))

(t/ann fx/do> (t/All [out failure]
                     [[out -> t/Any]
                      (fx/IEffect t/Any out failure)
                      -> (fx/IEffect t/Any out failure)]))

(t/ann fx/mapcat> (t/All [in out failure]
                         [(fx/IEffect in out failure)
                          (fx/IEffect t/Any in failure)
                          -> (fx/IEffect t/Any out failure)]))

(t/ann fx/if> (t/All [in then-out else-out failure then-failure else-failure]
                     [[in -> Boolean]
                      (fx/IEffect in then-out then-failure)
                      (fx/IEffect in else-out else-failure)
                      -> (fx/IEffect in (t/U then-out else-out) failure)]))

(t/ann fx/catch> (t/All [in out failure]
                        [(t/HMap t/Keyword (fx/IEffect error out nil))
                         (fx/IEffect t/any in prev-failure)
                         -> (fx/IEffect in out prev-failure)]))

(t/ann fx/catchall> (t/All [in out error]
                           [[error -> (fx/IEffect t/Any out nil)]
                            (fx/IEffect t/Any in prev-failure)
                            -> (fx/IEffect in out nil)]))

(t/ann fx/run-sync! (t/All [in out]
                           (t/IFn [(fx/IEffect in out t/Nothing)
                                   -> out]
                                  [(fx/IEffect in out t/Nothing)
                                   in -> out])))