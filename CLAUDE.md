# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

LISPF (Lisp Interactive Screen Programming Facility) is a Common Lisp framework for building interactive IBM 3270 terminal applications, inspired by IBM's ISPF. It includes a bundled CL3270 terminal emulation library (git submodule) and a mainframe-style file editor (lispf-edit).

## Build and Test Commands

**Loading the framework** (in a running SBCL with Quicklisp):
```lisp
(load "load.lisp")                    ;; loads lispf via ASDF
(asdf:load-system :lispf-edit)        ;; loads the editor component
```

**Running editor tests:**
```lisp
(asdf:load-system :lispf-test)
(asdf:load-system :lispf-edit)
(load "editor/test/editor-tests.lisp")
(lispf-editor-tests:run-all)
```

**Running framework tests** (key-layout, i18n):
```lisp
(load "test/key-layout-tests.lisp")
(lispf-key-layout-tests:run-all)
```

E2E tests require `s3270` on PATH (from x3270 suite). They start a real 3270 session and drive the terminal programmatically.

**Screen editor** (web-based WYSIWYG for .screen files):
```bash
cd screen-editor && make   # standalone binary (needs SBCL, buildapp, Node.js)
```

## Architecture

### ASDF Systems

- **lispf** (`lispf.asd`) - Core framework: screen registry, session management, key dispatch, application loop
- **lispf-edit** (`editor/lispf-edit.asd`) - XEDIT-style file editor, runs as a subapplication within lispf
- **lispf-editor** (`screen-editor/lispf-editor.asd`) - Web-based .screen file editor (Hunchentoot + Vue.js)
- **lispf-test** (`test/lispf-test.asd`) - Test framework with s3270 driver and per-package test registry
- **CL3270** (`CL3270/cl3270.asd`) - 3270 terminal protocol library (git submodule)

### Framework Core (`src/`)

The framework manages a 24x80 3270 screen. By default, rows 0 (title), 21 (command), 22 (error), 23 (key labels) are framework-managed; apps use rows 1-20. Screens with `:full-control t` give the app all 24 rows.

Key dispatch chain: `show-screen-and-read` -> 3270 response -> `dispatch-key` -> `handle-key` (EQL-specialized generic on screen+AID-key). Session state persists in `*current-field-values*` hash table across screen transitions.

Applications are defined with `define-application`, key handlers with `define-key-handler`, screen pre-render with `define-screen-update`. Field values are accessed via `with-field-bindings` (setf-able symbol macros).

### Editor (`editor/src/`)

The editor uses full-control mode and manages its own screen layout via `editor-layout`. Key concepts:
- **Virtual lines**: BOF marker + file lines + EOF marker. `virtual-to-real` converts display indices to file indices.
- **Prefix commands**: Typed over line numbers in 3270 overtype mode. Digit stripping extracts the command.
- **Undo**: Stack of line snapshots. Stack depth = alteration count. `editor-modified` is derived from stack non-emptiness.
- **MDT detection**: Uses 3270 Modified Data Tag to only process fields the user actually changed.

### Test Framework (`test/`)

Tests are self-registering via `define-test` into a per-package registry (`*test-registry*`). `run-tests` with no arguments runs all tests in the current package in definition order. E2E tests use `with-test-app` to start a lispf instance and `s3270` to drive it.

## Key Conventions for Lisp code

- **defclass over defstruct** for editor code
- **case with find-symbol** for command dispatch instead of cond chains
- **Early returns** with `unless`/`when` + `return-from` instead of `(if cond (progn ...) error-value)`
- **Combined setf**: `(setf a 1 b 2 c 3)` instead of separate setf calls
- **when-let** (from alexandria) for bind-and-test patterns
- **Top-Level form separation**: Top level forms and comment blocks are separated by empty lines

## MCP Integration

The lisp-mcp tool provides `eval_swank` and `eval_host_cl` for evaluating Lisp expressions in a running image. The parameter name is `expression` (not `code`). Use `edit_lisp` for paredit-safe structural editing of Lisp source files.

## Git Submodules

CL3270 is a submodule. Clone with `--recurse-submodules` or run `git submodule update --init`.
