# LISPF — Lisp Interactive Screen Programming Facility

LISPF is an application framework for building interactive IBM 3270
terminal applications in Common Lisp, inspired by IBM's
[ISPF](https://en.wikipedia.org/wiki/ISPF) (Interactive System
Productivity Facility).  It builds on top of the
[CL3270](https://github.com/marcoxa/CL3270) terminal emulation
library, which is included as a git submodule.

## How it works

Developing a LISPF application follows two steps:

1. **Design screens** using the visual screen editor — a web-based
   WYSIWYG tool that produces `.screen` files describing field
   layouts, attributes, colors, and key assignments.

2. **Implement handlers** in Lisp — use `define-key-handler` to
   respond to PF keys and `define-screen-update` to populate fields
   before display.  The framework manages screen navigation, session
   state, field validation, and error display automatically.

Screens are loaded lazily from `.screen` files at runtime and
hot-reloaded when modified on disk, so you can iterate on layouts
without restarting the application.

## Getting started

Clone the repository with submodules:

```bash
git clone --recurse-submodules https://github.com/hanshuebner/lispf
```

Or, if already cloned:

```bash
git submodule update --init
```

Load the system:

```lisp
(load "load.lisp")
```

## Example: Guestbook

The included guestbook example (`examples/guestbook/`) demonstrates a
complete multi-screen application with five screens:

- **welcome** — ASCII art splash screen, press Enter to continue
- **no-entries** — shown when the guestbook is empty, prompts to add the first entry
- **browse** — view guestbook entries, PF7/PF8 to page through them
- **new-entry** — form with validated name and multi-line message fields
- **bye** — session summary showing time spent and entries written

The screens are defined as `.screen` files in `examples/guestbook/screens/`.
The application logic in `guestbook.lisp` is roughly 80 lines — session
state, key handlers, and a start function:

```lisp
(asdf:load-system "lispf")
(load (merge-pathnames "examples/guestbook/guestbook.lisp"
                       (asdf:system-source-directory :lispf)))
(lispf-guestbook:start)
```

Connect with a 3270 terminal emulator (e.g. wx3270) on localhost:3270.

## Screen Editor

A web-based WYSIWYG editor for `.screen` files.

### Standalone binary

Build and run:

```bash
cd screen-editor && make
./screen-editor --screen-directory examples/guestbook/screens/
```

This creates a self-contained binary with embedded frontend assets.
Open http://localhost:8080 in a browser.  Options:

- `--port` — HTTP port (default: 8080)
- `--screen-directory` — path to `.screen` files (default: `screens/`)

Building requires SBCL with Quicklisp, `buildapp`, and Node.js/npm.

### Development mode

Development mode runs the Lisp backend and Vite frontend dev server
separately, with hot-reloading on both sides.

Start the Lisp backend:

```lisp
(load "load.lisp")
(asdf:load-system "lispf-editor")
(screen-editor:start-server "examples/guestbook/screens/" :port 8388)
```

In a separate terminal, start the Vite dev server:

```bash
cd screen-editor/frontend
npm install
npm run dev
```

Open http://localhost:5173 — the Vite dev server proxies `/api`
requests to the Lisp backend on port 8388.

## Prerequisites

- SBCL with Quicklisp
- Node.js and npm (for building and developing the screen editor)

## License

MIT
