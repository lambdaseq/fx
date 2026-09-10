# Todo Example Application

A full-stack functional Clojure REST API demonstrating how to build web services using the `fx` effect system libraries (`fx.core`, `fx.jdbc`, `fx.ring`) alongside standard Clojure ecosystem libraries:

- **[fx.core](https://github.com/conjurernix/fx)**: Pure effect pipelines, typed failure channels (`IFailure`), and continuation stack runtime.
- **[fx.jdbc](https://github.com/conjurernix/fx)**: Ambient database connection management and transaction handling.
- **[fx.ring](https://github.com/conjurernix/fx)**: Declarative Ring HTTP handler wrapping (`wrap-fx`), ambient context injection, and structured failure translation.
- **[fx.schedule](https://github.com/conjurernix/fx)**: Composable retry policies with exponential backoff & jitter, endpoint rate limiting, and background worker recurrence.
- **[fx.http-client](https://github.com/conjurernix/fx)**: Declarative outbound HTTP requests, ambient client context resolution, response combinators, and resilient error categorization.
- **[fx.async](https://github.com/conjurernix/fx)**: Structured concurrency, lightweight fiber supervisor hierarchies, speculative racing, CSP channels, and async coordination primitives.
- **[fx.observability](https://github.com/conjurernix/fx)**: Zero-dependency contextual structured logging (`fx.observability.log`), distributed tracing & W3C context propagation (`fx.observability.trace`), and concurrent in-memory metrics (`fx.observability.metrics`).
- **[HoneySQL v2](https://github.com/seancorfield/honeysql)**: Data-driven SQL generation.
- **[Reitit](https://github.com/metosin/reitit)**: Declarative, data-driven HTTP routing.
- **[Muuntaja](https://github.com/metosin/muuntaja)**: Content negotiation and automated JSON encoding/decoding.
- **[SQLite (In-Memory)](https://github.com/xerial/sqlite-jdbc)**: Zero-configuration shared in-memory database persistence.

---

## Architecture Overview

```
                      +-----------------------------+
                      |       HTTP Client           |
                      +--------------+--------------+
                                     |
                             JSON HTTP Request
                                     v
                      +-----------------------------+
                      |   Ring Jetty Adapter / HTTP  |
                      +--------------+--------------+
                                     |
                                     v
                      +-----------------------------+
                      |    Reitit Router & Muuntaja |
                      +--------------+--------------+
                                     |
                                     v
                      +-----------------------------+
                      |       fx.ring/wrap-fx       |
                      |   (Injects ::fx-jdbc/ds)    |
                      +--------------+--------------+
                                     |
                                     v
                      +-----------------------------+
                      |     todo.domain Pipelines   |
                      |   (Pure Effect Composition) |
                      +-------+-------------+-------+
                              |             |
                     Success  |             |  Failure
                              v             v
                    +------------+   +-------------------+
                    | todo.db    |   | fx.ring           |
                    | (HoneySQL) |   | :failure-map      |
                    +-----+------+   +---------+---------+
                          |                    |
                          v                    v
                    +------------+   +-------------------+
                    | fx.jdbc    |   | JSON HTTP Error   |
                    | SQLite DB  |   | (400, 404, 500)   |
                    +------------+   +-------------------+
```

### Key Principles
1. **Pure Effect Pipelines:** Business logic in `todo.domain` is defined as pure effect descriptions composed via combinators (`fx/map>`, `fx/mapcat>`, `fx/fail>`, `fx/succeed>`).
2. **Ambient Dependency Injection:** Handlers do not hardcode database connections; `fx.jdbc` statement executors resolve `::fx.jdbc/datasource` dynamically from ambient execution context injected by `fx.ring/wrap-fx`.
3. **Typed Error Channels & Resilience:** Validation and lookup failures are returned via `fx/fail>` with domain tags (`:todo/invalid-input`, `:todo/not-found`). Transient errors are retried automatically via `fx.schedule/retry-schedule>`, while rate limiting failures (`:rate-limiter/exceeded`) and circuit breaker trips (`:circuit-breaker/open`) map to HTTP 429 and 503 status codes in `todo.routes/failure-map`.
4. **Composable Layer Lifecycles (`fx.layer`):** Datasource, HTTP server, metrics registry, and background maintenance worker lifecycles are defined as pure layers (`datasource-layer>`, `http-server-layer>`, `worker-layer>`, `app-layer>`), providing deterministic reverse-order teardown and zero thread leakage.

---

## Resilience & Background Workers (`fx.schedule`)

The application integrates the `fx.schedule` resilience algebra for production-grade robustness:

```
                          +-------------------------------+
                          |   POST /api/todos Request     |
                          +---------------+---------------+
                                          |
                                          v
                          +-------------------------------+
                          |     Token-Bucket Rate Limiter |
                          |  (10 req/s with burst budget) |
                          +-------+---------------+-------+
                                  |               |
                         Allowed  |               | Exceeded
                                  v               v
                   +--------------------+   +--------------------+
                   | create-todo-handler|   | HTTP 429 Error     |
                   | (Execute Pipeline) |   | (Too Many Requests)|
                   +----------+---------+   +--------------------+
                              |
                              v
                   +--------------------+
                   | db-retry-policy    |
                   | (Exp Backoff +     |
                   |  Jitter, 3 retries)|
                   +----------+---------+
                              |
                              v
                   +--------------------+
                   | SQLite Database    |
                   +--------------------+
```

### 1. Database Retry Policies (`todo.resilience`)
All critical database queries and mutations in `todo.db` wrap their operations with `db-retry-policy`:
```clojure
(def db-retry-policy
  (-> (sched/exponential-backoff> {:initial-ms 50 :factor 2.0 :max-ms 1000})
      (sched/jitter> 0.1)
      (sched/intersect> (sched/recur-n> 3))
      (sched/while-tag> #{:fx.jdbc/error :jdbc/error :db/busy :sqlite/busy})))
```
If an intermittent lock or busy error occurs, the operation is retried with randomized exponential backoff up to 3 times before returning a structured error.

### 2. Route Rate Limiting (`todo.routes`)
Write endpoints (such as `POST /api/todos`) are protected against bursts and traffic spikes using an in-memory token-bucket rate limiter:
```clojure
(wrap-rate-limit create-todo-handler> rate-limiter)
```
When token capacity is exhausted, the handler fast-fails with `:rate-limiter/exceeded`, which translates into HTTP `429 Too Many Requests`.

### 3. Background Maintenance Worker (`todo.worker`)
Automated maintenance (such as pruning completed todos older than 30 days and incrementing maintenance run metrics) runs periodically in the background:
- Managed as an `fx.layer` (`worker-layer>`) wired into `todo.main/app-layer>`.
- Starts a background daemon thread on system boot and terminates cleanly with zero thread leakage when the system shuts down.

---

## Outbound HTTP & Synchronization (`fx.http-client`)

The application integrates `fx.http-client` for declarative outbound HTTP communication:

```
                          +-----------------------------------+
                          |  POST /api/todos/import-remote    |
                          +-----------------+-----------------+
                                            |
                                            v
                          +-----------------------------------+
                          | fx.http-client/get> (timeout 5s)  |
                          +-----------------+-----------------+
                                            |
                                            v
                          +-----------------------------------+
                          | fx.jdbc/with-transaction>         |
                          | (Batch Insert Todos into SQLite)  |
                          +-----------------------------------+

                          +-----------------------------------+
                          | POST /api/todos/:id/notify-webhook|
                          +-----------------+-----------------+
                                            |
                                            v
                          +-----------------------------------+
                          | todo.domain/get-todo-by-id>       |
                          +-----------------+-----------------+
                                            |
                                            v
                          +-----------------------------------+
                          | fx.http-client/post>              |
                          | (Exponential Backoff Retries)     |
                          +-----------------+-----------------+
                                            |
                                            v
                          +-----------------------------------+
                          | External Webhook Endpoint         |
                          +-----------------------------------+
```

### 1. Remote Todo Synchronization (`todo.domain/import-remote-todos>`)
Fetches remote JSON items from an external endpoint via `http/get>`, normalizes schemas, and batch-inserts valid todos into SQLite within a database transaction boundary (`fx.jdbc/with-transaction>`).

### 2. Resilient Webhook Notifications (`todo.domain/notify-webhook>`)
Dispatches completion payloads to external webhooks via `http/post>`. Transient network failures (`:http/server-error`, `:http/timeout`, `:http/connection-error`) are automatically retried up to 3 times with exponential backoff and jitter (`todo.resilience/webhook-retry-policy`).

### 3. Managed HTTP Client Layer (`todo.main/http-client-layer>`)
Outbound HTTP requests resolve ambient client configurations from `:fx.http-client/client` in the execution context, managed cleanly through `app-layer>`.

---

## Project Structure

```
example/
├── deps.edn             # Standalone project dependencies & aliases
├── README.md            # Architecture, usage, and API guide
├── test.http            # Interactive HTTP requests for IntelliJ / REST Client
├── src/
│   └── todo/
│       ├── db.clj          # SQLite datasource, DDL, and HoneySQL queries with retries
│       ├── domain.clj      # Pure business logic & effect pipelines (CRUD, sync, webhooks)
│       ├── resilience.clj  # Retry policies, rate limiters, & failure filters
│       ├── routes.clj      # Reitit router, fx.ring wrapping, rate limiting, & failure map
│       ├── schema.clj      # Malli schemas, data coercion, & validation
│       ├── worker.clj      # Background recurring maintenance worker layer
│       ├── main.clj        # Server lifecycle (start-server!, stop-server!, -main)
│       └── examples/
│           └── http_client_demo.clj # Standalone runnable demo for fx-http-client
└── test/
    └── todo/
        ├── api_test.clj           # Integration tests for HTTP endpoints, rate limiting, & fx pipelines
        ├── db_test.clj            # Tests for database queries, HoneySQL, AST, and context DI
        ├── domain_test.clj        # Tests for validation short-circuiting, effect mocking, & domain logic
        ├── external_sync_test.clj # Tests for remote import, webhook resilience, & embedded mock server
        ├── resilience_test.clj    # Tests for retry policies, backoff steps, & rate limiters
        ├── routes_test.clj        # Tests for route effect handlers & failure translation map
        ├── schema_test.clj        # Tests for Malli schemas, coercion, and validation errors
        └── worker_test.clj        # Tests for background worker recurrence and layer lifecycle
```

---

## Quickstart

### 1. Run the Server

From the `example/` directory via tools.build:
```bash
cd example
clojure -T:build run
```

Or using Clojure CLI:
```bash
# From repository root
clojure -M:example -m todo.main

# Or from example directory
cd example
clojure -M -m todo.main
```

The server starts on port `3000` (or the port specified by the `PORT` environment variable):
```
Initializing database schema...
Todo application server started successfully on http://localhost:3000
```

### 2. Run the Test Suite

From the `example/` directory via tools.build:
```bash
cd example
clojure -T:build test
```

Or using Clojure CLI:
```bash
# From repository root
clojure -M:dev:test:example -m cognitect.test-runner -d example/test

# Or from example directory
cd example
clojure -M:test -m cognitect.test-runner -d test
```

### 3. Run Standalone HTTP Client Demonstration

Execute the standalone demonstration script showcasing basic GET with JSON decoding, resilient POST with retries across transient server errors, ambient client injection, and observability metrics:
```bash
# From repository root
clojure -M:example -m todo.examples.http-client-demo

# Or from example directory
cd example
clojure -M -m todo.examples.http-client-demo
```

### 4. Interactive HTTP Requests (`test.http`)

When the server is running on `http://localhost:3000`, open [`example/test.http`](test.http) in IntelliJ IDEA or your editor's HTTP/REST client to execute requests interactively (CRUD operations, remote import, webhook notification, query filtering, and validation error scenarios).

---

## REPL Workflow

You can interactively develop and test effects and system layers from the Clojure REPL:

```clojure
(require '[todo.main :as main]
         '[todo.db :as db]
         '[todo.domain :as domain]
         '[fx.core :as fx]
         '[fx.layer :as fx-layer])

;; Start embedded server interactively via layers
(def server (main/start-server! {:port 3000}))

;; Or manage layers directly with fx-layer
(def system (fx-layer/start-layer! (main/app-layer> {:port 3000})))
(def ds (:fx.jdbc/datasource system))
(def ctx {:fx.jdbc/datasource ds})

;; Create a todo via domain effect
(fx/run-sync! (domain/create-todo> {:title "Try fx effect system"
                                    :description "Experiment in REPL"})
              ctx)
;; => {:id 1, :title "Try fx effect system", :description "Experiment in REPL", :completed false, ...}

;; Query todos
(fx/run-sync! (domain/list-todos>) ctx)

;; Stop the server and release layer resources
(main/stop-server!)
;; or (fx-layer/stop-layer! system)
```

---

## API Reference & Curl Examples

### 1. Create a Todo
- **Endpoint:** `POST /api/todos`
- **Request:**
  ```bash
  curl -X POST http://localhost:3000/api/todos \
       -H "Content-Type: application/json" \
       -d '{"title": "Buy groceries", "description": "Milk, eggs, and bread"}'
  ```
- **Response (`201 Created`):**
  ```json
  {
    "id": 1,
    "title": "Buy groceries",
    "description": "Milk, eggs, and bread",
    "completed": false,
    "created-at": "2026-09-08T01:13:00.000Z",
    "updated-at": "2026-09-08T01:13:00.000Z"
  }
  ```

### 2. List All Todos
- **Endpoint:** `GET /api/todos`
- **Request:**
  ```bash
  curl http://localhost:3000/api/todos
  ```
- **Response (`200 OK`):**
  ```json
  [
    {
      "id": 1,
      "title": "Buy groceries",
      "description": "Milk, eggs, and bread",
      "completed": false,
      "created-at": "2026-09-08T01:13:00.000Z",
      "updated-at": "2026-09-08T01:13:00.000Z"
    }
  ]
  ```

### 3. Filter Todos by Completed Status
- **Endpoint:** `GET /api/todos?completed=true` or `GET /api/todos?completed=false`
- **Request:**
  ```bash
  curl "http://localhost:3000/api/todos?completed=false"
  ```

### 4. Get a Todo by ID
- **Endpoint:** `GET /api/todos/:id`
- **Request:**
  ```bash
  curl http://localhost:3000/api/todos/1
  ```
- **Response (`200 OK`):**
  ```json
  {
    "id": 1,
    "title": "Buy groceries",
    "description": "Milk, eggs, and bread",
    "completed": false,
    "created-at": "2026-09-08T01:13:00.000Z",
    "updated-at": "2026-09-08T01:13:00.000Z"
  }
  ```

### 5. Update a Todo
- **Endpoint:** `PUT /api/todos/:id`
- **Request:**
  ```bash
  curl -X PUT http://localhost:3000/api/todos/1 \
       -H "Content-Type: application/json" \
       -d '{"title": "Buy organic groceries", "completed": false}'
  ```
- **Response (`200 OK`):**
  ```json
  {
    "id": 1,
    "title": "Buy organic groceries",
    "description": "Milk, eggs, and bread",
    "completed": false,
    "created-at": "2026-09-08T01:13:00.000Z",
    "updated-at": "2026-09-08T01:14:00.000Z"
  }
  ```

### 6. Toggle Todo Completion Status
- **Endpoint:** `PATCH /api/todos/:id/toggle`
- **Request:**
  ```bash
  curl -X PATCH http://localhost:3000/api/todos/1/toggle
  ```
- **Response (`200 OK`):**
  ```json
  {
    "id": 1,
    "title": "Buy organic groceries",
    "description": "Milk, eggs, and bread",
    "completed": true,
    "created-at": "2026-09-08T01:13:00.000Z",
    "updated-at": "2026-09-08T01:15:00.000Z"
  }
  ```

### 7. Delete a Todo
- **Endpoint:** `DELETE /api/todos/:id`
- **Request:**
  ```bash
  curl -X DELETE http://localhost:3000/api/todos/1
  ```
- **Response (`200 OK`):**
  ```json
  {
    "deleted": true,
    "id": 1
  }
  ```

### 8. Import Remote Todos
- **Endpoint:** `POST /api/todos/import-remote`
- **Request:**
  ```bash
  curl -X POST http://localhost:3000/api/todos/import-remote \
       -H "Content-Type: application/json" \
       -d '{"url": "https://jsonplaceholder.typicode.com/todos", "limit": 5}'
  ```
- **Response (`200 OK`):**
  ```json
  {
    "imported-count": 2,
    "todos": [
      {
        "id": 1,
        "title": "delectus aut autem",
        "description": null,
        "completed": false,
        "created-at": "2026-09-08T01:13:00.000Z",
        "updated-at": "2026-09-08T01:13:00.000Z"
      }
    ]
  }
  ```

### 9. Dispatch Todo Webhook Notification
- **Endpoint:** `POST /api/todos/:id/notify-webhook`
- **Request:**
  ```bash
  curl -X POST http://localhost:3000/api/todos/1/notify-webhook \
       -H "Content-Type: application/json" \
       -d '{"webhook-url": "https://httpbin.org/post"}'
  ```
- **Response (`200 OK`):**
  ```json
  {
    "notified": true,
    "todo-id": 1,
    "webhook-url": "https://httpbin.org/post",
    "remote-status": 200
  }
  ```

### 10. Query Metrics
- **Endpoint:** `GET /api/metrics`
- **Request:**
  ```bash
  curl http://localhost:3000/api/metrics
  ```
- **Response (`200 OK`):**
  ```json
  {
    "counters": {
      "http.server.requests.total": {"tags": {"method": "GET"}, "value": 42},
      "todo.created.total": {"tags": {}, "value": 5},
      "todo.deleted.total": {"tags": {}, "value": 1}
    },
    "gauges": {},
    "timers": {
      "http.server.requests.duration": {
        "tags": {},
        "count": 42,
        "sum-ms": 128.4,
        "min-ms": 0.8,
        "max-ms": 14.2,
        "avg-ms": 3.05
      },
      "db.query.duration": {
        "tags": {"operation": "query-todos"},
        "count": 15,
        "sum-ms": 22.5,
        "min-ms": 0.5,
        "max-ms": 4.1,
        "avg-ms": 1.5
      }
    }
  }
  ```

### 9. Error Responses
- **Validation Error (`400 Bad Request`):**
  ```json
  {
    "error": "Bad Request",
    "details": {
      "message": "Field 'title' is required and must not be blank",
      "field": "title"
    }
  }
  ```
- **Not Found Error (`404 Not Found`):**
  ```json
  {
    "error": "Not Found",
    "details": {
      "message": "Todo not found with id 999",
      "id": 999
    }
  }
  ```
- **Rate Limit Exceeded Error (`429 Too Many Requests`):**
  ```json
  {
    "error": "Too Many Requests",
    "details": {
      "message": "Rate limit quota exceeded"
    }
  }
  ```
- **Circuit Breaker Open Error (`503 Service Unavailable`):**
  ```json
  {
    "error": "Service Unavailable",
    "details": {
      "message": "Downstream dependency circuit breaker is open"
    }
  }
  ```

---

## Observability & Distributed Tracing

The example application leverages the **`fx.observability`** subsystem to provide zero-dependency production observability:

### 1. Contextual Structured Logging (`fx.observability.log`)
HTTP requests automatically populate log annotations with `:request-id`, `:method`, and `:uri`. Domain workflows and system startup/shutdown log structured entries containing timestamps, active trace IDs, span IDs, and contextual metadata without passing logger objects through functions.

### 2. Distributed Tracing & W3C Context (`fx.observability.trace`)
- Incoming requests with a W3C `traceparent` header (e.g., `00-4bf92f3577b34da6a3ce929d0e0e4736-00f067aa0ba902b7-01`) automatically propagate their `trace-id` throughout all downstream domain workflows (`todo.create`, `todo.update`, `todo.delete`) and database queries (`db.query-todos`, `db.insert-todo`).
- If omitted, a valid 32-character hex trace ID is generated automatically.
- All HTTP responses include an outgoing `traceparent` header to correlate frontend requests with backend traces.

### 3. In-Memory Metrics (`fx.observability.metrics`)
Concurrent, lock-free metrics (`LongAdder`, `DoubleAdder`) record request throughput, latencies, and domain entity counters without external dependencies. The current state is queryable at `GET /api/metrics`.

---

## Standalone Demonstrations

### 1. `fx-async` Structured Concurrency Demo
Run the comprehensive async demonstration covering parallel mapping, speculative racing, fiber hierarchies, core.async channels, queues, hubs, deferreds, and semaphores:

```bash
clojure -M:example -m todo.examples.async-demo
```

### 2. `fx-http-client` Resilient Outbound HTTP Demo
Run the HTTP client demonstration covering JSON fetching, retry policies, ambient client binding, and trace propagation:

```bash
clojure -M:example -m todo.examples.http-client-demo
```
