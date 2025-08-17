# Clojure Style Guide Implementation Plan

This document tracks the implementation of formatting rules based on the
[Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide).

## Phase 1: Architecture & Core Infrastructure
- [x] Design formatting rule architecture - Create a multimethod-based system where rules
      dispatch on node type
- [ ] Add configuration system - Support for customizing rules (even though we start without
      config options)

## Phase 2: Indentation & Alignment Rules
- [x] 2-space indentation - Convert tabs to spaces, ensure consistent indentation depth
      (AST-aware: functions=2 spaces, vectors/maps/sets=1 space alignment)
- [x] Add comprehensive tests for context-aware indentation - Test vectors `[1\n 2\n 3]`,
      maps `{:a 1\n :b 2}`, sets `#{1\n 2}`, nested structures `{:values [1\n  2]}`
- [x] Position-based alignment for collection elements - Align with first element position
- [x] Newline indentation insertion - Insert proper indentation after newlines when missing
- [x] Vertical alignment of function arguments - Align multi-line function call arguments
- [x] Namespace require statement formatting - Fix :require list alignment to match current
      formatting style
- [x] Namespace collection type normalization - :require uses vectors, :import uses lists
- [x] Namespace sorting - Alphabetical sorting of :require and :import statements
- [x] def/defn formatting - def values on separate lines, defn docstrings positioned correctly
- [x] Let form indentation - Proper indentation for `let` binding vectors and body
- [x] Map key alignment - Each key-value pair on separate line, keys in single column
- [ ] Special form handling - Implement proper indentation for let, if, when, cond, etc.

## Phase 3: Spacing & Whitespace Rules
- [ ] Refactor generate-whitespace-before to multimethod - Convert large case statement to
      multimethod dispatching on context type for better organization
- [x] Strip-and-generate approach - Implement two-phase whitespace handling for clean formatting
- [x] Collection formatting consistency - Unified formatting for vectors, maps, and sets
- [ ] Bracket spacing - Add spaces around brackets when preceded/followed by text
- [x] Comma handling - Remove commas from sequential collections, make optional for maps
- [x] Trailing whitespace removal - Strip whitespace at end of lines
- [x] Leading whitespace removal - Remove whitespace at document start
- [x] Single space normalization - Ensure single spaces between elements

## Phase 4: Line Management
- [ ] 96-character line length - Break long lines appropriately
- [ ] Unix line endings - Convert to LF endings
- [ ] File ending newline - Ensure files end with newline
- [ ] Trailing parentheses grouping - Gather closing parens on single line

## Phase 5: Structural Formatting
- [x] Top-level form separation - Single blank line between top-level forms
- [ ] Related def grouping - No blank lines between related definitions
- [ ] Function definition formatting - No blank lines within function bodies
- [ ] Conditional form formatting - Proper line breaks in `cond`, `case`, etc.

## Phase 6: Testing & Validation
- [x] Comprehensive test suite - Test each rule individually and in combination
- [ ] Integration tests - Test with real Clojure codebases
- [ ] Performance optimization - Ensure formatter is efficient for large files

## Implementation Notes

### Rule Categories from Style Guide:
1. **Indentation and Spacing**: 2 spaces, vertical alignment, let bindings, map keys
2. **Line Length and Formatting**: 96 chars max, Unix endings, file newlines, blank lines
3. **Bracket and Spacing Rules**: spaces around brackets, comma usage, no trailing whitespace
4. **Structural Formatting**: map literals, def grouping, cond forms

### Architecture Decisions:
- Use rewrite-clj for AST manipulation with comment preservation
- Plugin-based rule system for extensibility
- Configuration support for rule customization
- Each rule as a separate, testable function
