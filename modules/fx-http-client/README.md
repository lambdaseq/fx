# fx/http-client

Declarative, purely functional HTTP client effects for the `fx` effect system, built on [`hato`](https://github.com/gnarroway/hato) and Java 11's `java.net.http.HttpClient`.

`fx-http-client` provides composable HTTP effects featuring automatic ambient client context resolution, rich response combinators, categorized failure modeling, and seamless interoperability with `fx-schedule` retry and resilience primitives.

## Installation

Add the dependency to your `deps.edn`:

```clojure
{:deps {io.github.conjurernix/fx.http-client {:mvn/version "0.0.1-alpha"}}}
;; or local module coordinate
{:deps {fx/http-client {:mvn/version "0.0.1-alpha"}}}
```

Requires `fx/core` (`io.github.conjurernix/fx.core`).

## Philosophy & Key Concepts

- **Effects as Blueprints**: HTTP requests are descriptions of network operations represented as immutable data records, executed at pipeline boundaries via `fx/run-sync!` or `fx/run-async!`.
- **Categorized Failure Modeling**: HTTP failures (4xx, 5xx, timeouts, connection errors) are mapped to typed `fx.core/Failure` records (`:http/client-error`, `:http/server-error`, `:http/timeout`, `:http/connection-error`, `:http/error`), enabling targeted retry policies via `fx.schedule/while-tag>`.
- **Ambient Context Resolution**: Requests resolve an ambient HTTP client bound in the execution context under `:fx.http-client/client`, falling back to a shared default client if none is specified.
- **Canonical Naming & Monomorphic Contracts**: Strict adherence to repository conventions—effect constructors end with `>`, predicates with `?`, with deterministic argument contracts.

---

## Quickstart

```clojure
(ns example.http
  (:require [fx.core :as fx]
            [fx.http-client :as http]
            [fx.schedule :as sched]))

;; Simple GET request extracting the response body
(def get-user-body
  (-> (http/get> "https://api.example.com/users/42" {:as :json})
      (http/body>)))

;; Executing with retry on 5xx server errors or timeouts
(def resilient-request
  (-> (http/get> "https://api.example.com/data")
      (sched/retry-schedule>
        (-> (sched/exponential-backoff> {:initial-ms 100 :factor 2.0})
            (sched/intersect> (sched/recur-n> 3))
            (sched/while-tag> :http/server-error)))))

;; Evaluate the effect pipeline
(fx/run-sync! get-user-body)
```

---

## API Reference

### Client Lifecycle (`fx.http-client`)

| Function | Signature | Description |
|---|---|---|
| `build-client>` | `[opts]` | Creates an effect yielding a configured `java.net.http.HttpClient` instance via Hato. |
| `client?` | `[x]` | Returns true if `x` is an instance of `java.net.http.HttpClient`. |
| `with-client>` | `[eff client]` | Injects `client` into `:fx.http-client/client` in the execution context for `eff`. |

### Request Constructors & Verb Combinators (`fx.http-client`)

| Function | Signature | Description |
|---|---|---|
| `request>` | `([req-map] [client req-map])` | Constructs an HTTP request effect from a request map using context or explicit client. |
| `get>` | `([url] [url opts] [client url opts])` | HTTP GET request effect. |
| `post>` | `([url opts] [client url opts])` | HTTP POST request effect. |
| `put>` | `([url opts] [client url opts])` | HTTP PUT request effect. |
| `delete>` | `([url] [url opts] [client url opts])` | HTTP DELETE request effect. |
| `patch>` | `([url opts] [client url opts])` | HTTP PATCH request effect. |
| `head>` | `([url] [url opts] [client url opts])` | HTTP HEAD request effect. |
| `options>` | `([url] [url opts] [client url opts])` | HTTP OPTIONS request effect. |

### Response Combinators & Predicates (`fx.http-client`)

| Function | Signature | Description |
|---|---|---|
| `body>` | `[eff]` | Transforms upstream HTTP response effect by extracting its `:body`. |
| `status>` | `[eff]` | Transforms upstream HTTP response effect by extracting its `:status`. |
| `headers>` | `[eff]` | Transforms upstream HTTP response effect by extracting its `:headers`. |
| `response?` | `[x]` | Predicate testing if `x` is a valid HTTP response map. |
| `ok?` | `[x]` | Predicate testing if `x` has HTTP status 200. |
| `success?` | `[x]` | Predicate testing if `x` has a 2xx HTTP status (200-299). |
| `redirect?` | `[x]` | Predicate testing if `x` has a 3xx HTTP status (300-399). |
| `client-error?` | `[x]` | Predicate testing if `x` is a client error (4xx) failure. |
| `server-error?` | `[x]` | Predicate testing if `x` is a server error (5xx) failure. |
| `timeout-error?` | `[x]` | Predicate testing if `x` is a timeout failure. |
| `connection-error?` | `[x]` | Predicate testing if `x` is a connection error failure. |
| `http-error?` | `[x]` | Predicate testing if `x` is any categorized HTTP failure. |
