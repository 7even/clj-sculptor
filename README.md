# clj-sculptor

A Clojure library and CLI tool for parsing Clojure/ClojureScript code into an AST enriched with comments, then generating formatted code using predefined formatting rules.

## Features

- Parse Clojure code while preserving comments
- Apply custom formatting rules
- Command-line interface for batch processing
- Built on rewrite-clj for robust AST manipulation

## Usage

### Library

```clojure
(require '[clj-sculptor.core :as sculptor])

;; Format code directly
(sculptor/format-code "(defn foo [x]\n    (+ x 1))")
;; => "(defn foo [x]\n  (+ x 1))"

;; Works with vectors, maps, and nested structures
(sculptor/format-code "[1\n  2\n   3]")
;; => "[1\n 2\n 3]"
```

### CLI

```bash
# Format a file and print to stdout
clj -M:cli -i input.clj

# Format a file and save to output file
clj -M:cli -i input.clj -o formatted.clj
```

## Development

```bash
# Run tests
clj -M:test

# Start REPL with dev dependencies
clj -M:dev
```
