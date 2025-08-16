# Claude Guidelines for clj-sculptor

## Project Overview
clj-sculptor is a Clojure library and CLI tool for parsing Clojure/ClojureScript code into
an AST enriched with comments, then generating formatted code using predefined formatting rules.

## Git Guidelines
- Lowercase commit messages
- One-line commits unless many changes
- Multi-line format for co-authorship (blank line separation)
- **Commit messages**: Describe what's being added/changed in the diff, not the
  development process or removed alternatives

## Code Style Guidelines
- **Comparisons**: When comparing unknown value with constant, put constant second:
  `(= unknown-value :constant)` not `(= :constant unknown-value)`
  - Exception: In test assertions, use `(= expected actual)` order for clarity

## Test Guidelines
- **Tests**: Use `(testing "when condition" ...)` not `(testing "returns result" ...)`
- **Running tests**: Use REPL for testing: `(require 'clj-sculptor.rules-test :reload)`
  then `(clojure.test/run-tests 'clj-sculptor.rules-test)`

## Project Structure
- `src/clj_sculptor/core.clj` - Main library functions using rewrite-clj
- `src/clj_sculptor/cli.clj` - Command-line interface
- `test/` - Test files using Kaocha framework
- `deps.edn` - Dependencies and aliases configuration
- `tests.edn` - Kaocha test configuration

## Key Dependencies
- `rewrite-clj/rewrite-clj` - AST manipulation with comment preservation
- `org.clojure/tools.cli` - Command-line argument parsing
- `lambdaisland/kaocha` - Test runner

## rewrite-clj Usage
- **Raw functions (with `*` suffix)**: Use the "raw" versions of rewrite-clj functions
  (e.g., `z/next*`, `z/insert-right*`) for whitespace manipulation. These functions:
  - Don't skip whitespace/comment nodes during navigation (`z/next*`, `z/prev*`)
  - Don't automatically insert whitespace (`z/insert-right*`, `z/insert-left*`)
- **Regular functions**: The non-starred versions skip whitespace/comments and may add
  spacing automatically
- Example: `z/insert-right` adds space "if necessary", while `z/insert-right*` inserts
  exactly what you specify
