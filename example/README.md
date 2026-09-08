# Todo Example Application

A full-stack functional Clojure REST API demonstrating how to build web services using the `fx` effect system libraries (`fx.core`, `fx.jdbc`, `fx.ring`) alongside standard Clojure ecosystem libraries:

- **[fx.core](https://github.com/conjurernix/fx)**: Pure effect pipelines, typed failure channels (`IFailure`), and continuation stack runtime.
- **[fx.jdbc](https://github.com/conjurernix/fx)**: Ambient database connection management and transaction handling.
- **[fx.ring](https://github.com/conjurernix/fx)**: Declarative Ring HTTP handler wrapping (`wrap-fx`), ambient context injection, and structured failure translation.
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
3. **Typed Error Channels:** Validation and lookup failures are returned via `fx/fail>` with domain tags (`:todo/invalid-input`, `:todo/not-found`). `fx.ring/wrap-fx` translates these tags into standard HTTP response maps and JSON status codes via `:failure-map`.

---

## Project Structure

```
example/
├── deps.edn             # Standalone project dependencies & aliases
├── README.md            # Architecture, usage, and API guide
├── test.http            # Interactive HTTP requests for IntelliJ / REST Client
├── src/
│   └── todo/
│       ├── db.clj       # SQLite datasource, DDL, and HoneySQL queries
│       ├── domain.clj   # Pure business logic & effect pipelines
│       ├── routes.clj   # Reitit router, fx.ring wrapping, & failure map
│       ├── schema.clj   # Malli schemas, data coercion, & validation
│       └── main.clj     # Server lifecycle (start-server!, stop-server!, -main)
└── test/
    └── todo/
        ├── api_test.clj    # Integration tests for HTTP endpoints & fx pipelines
        ├── db_test.clj     # Tests for database queries, HoneySQL, AST, and context DI
        ├── domain_test.clj # Tests for validation short-circuiting, effect mocking, & domain logic
        ├── routes_test.clj # Tests for route effect handlers & failure translation map
        └── schema_test.clj # Tests for Malli schemas, coercion, and validation errors
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

### 3. Interactive HTTP Requests (`test.http`)

When the server is running on `http://localhost:3000`, open [`example/test.http`](test.http) in IntelliJ IDEA or your editor's HTTP/REST client to execute requests interactively (CRUD operations, query filtering, and validation error scenarios).

---

## REPL Workflow

You can interactively develop and test effects from the Clojure REPL:

```clojure
(require '[todo.main :as main]
         '[todo.db :as db]
         '[todo.domain :as domain]
         '[fx.core :as fx])

;; Start embedded server interactively
(main/start-server! {:port 3000})

;; Execute effect pipelines directly with run-sync!
(def ds (fx/run-sync! (db/get-datasource>)))
(def ctx {:fx.jdbc/datasource ds})

;; Create a todo via domain effect
(fx/run-sync! (domain/create-todo> {:title "Try fx effect system"
                                    :description "Experiment in REPL"})
              ctx)
;; => {:id 1, :title "Try fx effect system", :description "Experiment in REPL", :completed false, ...}

;; Query todos
(fx/run-sync! (domain/list-todos>) ctx)

;; Stop the server
(main/stop-server!)
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

### 8. Error Responses
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
```
- **Response (`200 OK`):**
  ```json
  {
    "deleted": true,
    "id": 1
  }
  ```

### 8. Error Responses
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
