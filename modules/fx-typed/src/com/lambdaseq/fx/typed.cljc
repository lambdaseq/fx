(ns com.lambdaseq.fx.typed
  (:require [com.lambdaseq.fx.core :as fx]
            [typed.clojure :as t]
            ))

(t/ann-protocol [[in :variance :covariant]
                 [out :variance :covariant]] fx/IEffect
                -effect-type [(fx/IEffect in out)
                              -> t/Keyword]
                -next-effect [(fx/IEffect in out)
                              -> (t/Option (fx/IEffect t/Any in))]
                -eval! [(fx/IEffect in out)
                        -> out])

(t/ann-protocol [[error :variance :covariant]] fx/IFailure
                -failure-type [(fx/IFailure error) -> t/Keyword]
                -error [(fx/IFailure error) -> error])

(t/ann fx/effect? [t/Any -> Boolean])

(t/ann fx/failure? [t/Any -> Boolean])

(t/ann fx/make-effect (t/All [in out]
                             [t/Keyword
                              (t/Option (fx/IEffect t/Any in))
                              [in -> out]
                              -> (fx/IEffect in out)]))

(t/ann fx/make-failure (t/All [error]
                              [t/Keyword error
                               -> (fx/IFailure error)]))

(t/ann fx/succeed> (t/All [x] [x -> (fx/IEffect nil x)]))

(t/ann fx/fail> (t/All [x]
                       [x -> (fx/IFailure x)]
                       [t/Keyword x -> (fx/IFailure x)]))

(t/ann fx/map> (t/All [in out]
                      [[in -> out]
                       (fx/IEffect t/Any in)
                       -> (fx/IEffect in out)]))

(t/ann fx/do> (t/All [inout]
                     [[inout -> t/Any]
                      (fx/IEffect t/Any inout)
                      -> (fx/IEffect inout inout)]))

(t/ann fx/mapcat> (t/All [in out]
                         [[in -> (fx/Effect t/Any out)]
                          (fx/IEffect t/Any in)
                          -> (fx/IEffect in out)]))

(t/ann fx/if> (t/All [in then-out else-out]
                     [[in -> Boolean]
                      [in -> (fx/IEffect t/Any then-out)]
                      [in -> (fx/IEffect t/Any else-out)]
                      -> (fx/IEffect in (t/U then-out else-out))]))

(t/ann fx/catch> (t/All [in out error]
                        [(t/HMap t/Keyword [error -> (fx/IEffect t/Any out)])
                         (fx/IEffect t/any in)
                         -> (fx/IEffect in out)]))

(t/ann fx/catchall> (t/All [in out error]
                           [[error -> (fx/IEffect t/Any out)]
                            (fx/IEffect t/Any in)
                            -> (fx/IEffect in out)]))

(t/ann fx/run-sync! (t/All [in out error]
                           [(fx/IEffect in out)
                            -> (t/U out error)]))