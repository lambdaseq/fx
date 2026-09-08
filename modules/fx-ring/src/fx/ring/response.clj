(ns fx.ring.response
  (:require [fx.core :as fx]
            [ring.util.response :as ring-resp]))

(def request-key :fx.ring/request)

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
  "Transforms the value of `eff` into a standard 200 Ring response map,
   or creates a 200 response effect from a static `body`."
  ([]
   (fx/map> (fn [body] (ring-resp/response body))))
  ([eff-or-body]
   (if (fx/effect? eff-or-body)
     (fx/map> eff-or-body (fn [body] (ring-resp/response body)))
     (fx/succeed> (ring-resp/response eff-or-body))))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/response body))))))

(defn ok>
  "Transforms the value of `eff` into a 200 OK Ring response map,
   or creates a 200 OK response effect from a static `body`."
  ([]
   (fx/map> (fn [body] (ring-resp/response body))))
  ([eff-or-body]
   (if (fx/effect? eff-or-body)
     (fx/map> eff-or-body (fn [body] (ring-resp/response body)))
     (fx/succeed> (ring-resp/response eff-or-body))))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/response body))))))

(defn created>
  "Transforms the value of `eff` into a 201 Created Ring response map,
   or creates a 201 Created response effect."
  ([]
   (fx/map> (fn [body] (ring-resp/status (ring-resp/response body) 201))))
  ([eff-or-url]
   (if (fx/effect? eff-or-url)
     (fx/map> eff-or-url (fn [body] (ring-resp/status (ring-resp/response body) 201)))
     (fx/succeed> (ring-resp/created eff-or-url))))
  ([eff-or-url url-or-body]
   (if (fx/effect? eff-or-url)
     (fx/map> eff-or-url (fn [body] (ring-resp/created url-or-body body)))
     (fx/succeed> (ring-resp/created eff-or-url url-or-body))))
  ([eff url body]
   (-> eff (fx/map> (fn [_] (ring-resp/created url body))))))

(defn bad-request>
  "Transforms the value of `eff` into a 400 Bad Request Ring response map,
   or creates a 400 Bad Request response effect."
  ([]
   (fx/map> (fn [body] (ring-resp/bad-request body))))
  ([eff-or-body]
   (if (fx/effect? eff-or-body)
     (fx/map> eff-or-body (fn [body] (ring-resp/bad-request body)))
     (fx/succeed> (ring-resp/bad-request eff-or-body))))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/bad-request body))))))

(defn not-found>
  "Transforms the value of `eff` into a 404 Not Found Ring response map,
   or creates a 404 Not Found response effect."
  ([]
   (fx/map> (fn [body] (ring-resp/not-found body))))
  ([eff-or-body]
   (if (fx/effect? eff-or-body)
     (fx/map> eff-or-body (fn [body] (ring-resp/not-found body)))
     (fx/succeed> (ring-resp/not-found eff-or-body))))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/not-found body))))))

(defn internal-server-error>
  "Transforms the value of `eff` into a 500 Internal Server Error Ring response map,
   or creates a 500 Internal Server Error response effect."
  ([]
   (fx/map> (fn [body] (ring-resp/status (ring-resp/response body) 500))))
  ([eff-or-body]
   (if (fx/effect? eff-or-body)
     (fx/map> eff-or-body (fn [body] (ring-resp/status (ring-resp/response body) 500)))
     (fx/succeed> (ring-resp/status (ring-resp/response eff-or-body) 500))))
  ([eff body]
   (-> eff (fx/map> (fn [_] (ring-resp/status (ring-resp/response body) 500))))))

(defn redirect>
  "Creates an effect yielding a 302 (or custom status) Redirect Ring response map."
  ([url]
   (fx/succeed> (ring-resp/redirect url)))
  ([eff-or-url url-or-status]
   (if (fx/effect? eff-or-url)
     (fx/map> eff-or-url (fn [_] (ring-resp/redirect url-or-status)))
     (fx/succeed> (ring-resp/redirect eff-or-url url-or-status))))
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
