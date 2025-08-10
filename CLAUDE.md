# Claude Guidelines for clj-sculptor

## Project Overview
clj-sculptor is a Clojure library and CLI tool for parsing Clojure/ClojureScript code into
an AST enriched with comments, then generating formatted code using predefined formatting rules.

## Git Guidelines
- Lowercase commit messages
- One-line commits unless many changes
- Multi-line format for co-authorship (blank line separation)

## Test Guidelines
- **Tests**: Use `(testing "when condition" ...)` not `(testing "returns result" ...)`

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
