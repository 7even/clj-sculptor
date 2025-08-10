# Clojure Style Guide Implementation Plan

This document tracks the implementation of formatting rules based on the
[Clojure Style Guide](https://github.com/bbatsov/clojure-style-guide).

## Phase 1: Architecture & Core Infrastructure
- [x] Design formatting rule architecture - Create a multimethod-based system where rules
      dispatch on node type
- [ ] Add configuration system - Support for customizing rules (even though we start without
      config options)

## Phase 2: Indentation & Alignment Rules
- [ ] 2-space indentation - Convert tabs to spaces, ensure consistent indentation depth
- [ ] Vertical alignment of function arguments - Align multi-line function call arguments
- [ ] Let binding alignment - Vertically align binding pairs in `let` forms
- [ ] Map key alignment - Align keys in map literals

## Phase 3: Spacing & Whitespace Rules
- [ ] Bracket spacing - Add spaces around brackets when preceded/followed by text
- [ ] Comma handling - Remove commas from sequential collections, make optional for maps
- [ ] Trailing whitespace removal - Strip whitespace at end of lines
- [ ] Single space normalization - Ensure single spaces between elements

## Phase 4: Line Management
- [ ] 96-character line length - Break long lines appropriately
- [ ] Unix line endings - Convert to LF endings
- [ ] File ending newline - Ensure files end with newline
- [ ] Trailing parentheses grouping - Gather closing parens on single line

## Phase 5: Structural Formatting
- [ ] Top-level form separation - Single blank line between top-level forms
- [ ] Related def grouping - No blank lines between related definitions
- [ ] Function definition formatting - No blank lines within function bodies
- [ ] Conditional form formatting - Proper line breaks in `cond`, `case`, etc.

## Phase 6: Testing & Validation
- [ ] Comprehensive test suite - Test each rule individually and in combination
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