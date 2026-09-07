(ns com.lambdaseq.fx.ring.response
  (:require [com.lambdaseq.fx.core :as fx]
            [ring.util.response :as ring-resp]))

(def request-key :com.lambdaseq.fx.ring/request)

(defn request>
  "Creates an effect yielding the active Ring request map (or a field) from context."
  ([]
   (fx/context> request-key))
  ([key]
   (-> (fx/context> request-key)
       (fx/map> (fn [req] (get req key)))))
  ([key default-val]
   (-> (fx/context> request-key)
       (fx/map> (fn [req] (get req key default-val))))))

(defn response>
  "Creates an effect yielding a standard 200 Ring response map wrapping `body`."
  ([body]
   (fx/succeed> (ring-resp/response body)))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/response body))))))

(defn ok>
  "Creates an effect yielding a 200 OK Ring response map wrapping `body`."
  ([]
   (fx/succeed> (ring-resp/response nil)))
  ([body]
   (fx/succeed> (ring-resp/response body)))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/response body))))))

(defn created>
  "Creates an effect yielding a 201 Created Ring response map."
  ([url]
   (fx/succeed> (ring-resp/created url)))
  ([url body]
   (fx/succeed> (ring-resp/created url body)))
  ([eff url body]
   (-> eff (fx/map> (fn [_] (ring-resp/created url body))))))

(defn bad-request>
  "Creates an effect yielding a 400 Bad Request Ring response map."
  ([]
   (fx/succeed> (ring-resp/bad-request nil)))
  ([body]
   (fx/succeed> (ring-resp/bad-request body)))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/bad-request body))))))

(defn not-found>
  "Creates an effect yielding a 404 Not Found Ring response map."
  ([]
   (fx/succeed> (ring-resp/not-found nil)))
  ([body]
   (fx/succeed> (ring-resp/not-found body)))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/not-found body))))))

(defn internal-server-error>
  "Creates an effect yielding a 500 Internal Server Error Ring response map."
  ([]
   (fx/succeed> (ring-resp/status (ring-resp/response nil) 500)))
  ([body]
   (fx/succeed> (ring-resp/status (ring-resp/response body) 500)))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/status (ring-resp/response body) 500))))))

(defn redirect>
  "Creates an effect yielding a 302 (or custom status) Redirect Ring response map."
  ([url]
   (fx/succeed> (ring-resp/redirect url)))
  ([url status-code]
   (fx/succeed> (ring-resp/redirect url status-code)))
  ([eff url status-code]
   (-> eff (fx/map> (fn [_] (ring-resp/redirect url status-code))))))

(defn status>
  "Sets the HTTP status code on the response produced by upstream effect."
  ([status-code]
   (fx/map> (fn [resp] (ring-resp/status resp status-code))))
  ([eff status-code]
   (-> eff (fx/map> (fn [resp] (ring-resp/status resp status-code))))))

(defn header>
  "Adds an HTTP header to the response produced by upstream effect."
  ([header-name header-val]
   (fx/map> (fn [resp] (ring-resp/header resp header-name header-val))))
  ([eff header-name header-val]
   (-> eff (fx/map> (fn [resp] (ring-resp/header resp header-name header-val))))))

(defn content-type>
  "Sets the Content-Type header on the response produced by upstream effect."
  ([content-type-str]
   (fx/map> (fn [resp] (ring-resp/content-type resp content-type-str))))
  ([eff content-type-str]
   (-> eff (fx/map> (fn [resp] (ring-resp/content-type resp content-type-str))))))
