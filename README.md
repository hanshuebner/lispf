# LISPF — Lisp Interactive Screen Programming Facility

LISPF is an application framework for building interactive IBM 3270
terminal applications in Common Lisp, inspired by IBM's
[ISPF](https://en.wikipedia.org/wiki/ISPF) (Interactive System
Productivity Facility).  It builds on top of the
[CL3270](https://github.com/marcoxa/CL3270) terminal emulation
library.

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

## Example: Guestbook

The included guestbook example (`examples/guestbook/`) demonstrates a
complete multi-screen application with four screens:

- **welcome** — ASCII art splash screen, press Enter to continue
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

A web-based WYSIWYG editor for `.screen` files:

```lisp
(asdf:load-system "screen-editor")
(screen-editor:start-server "path/to/screens/" :port 8388)
```

Or build and run the standalone binary:

```bash
cd screen-editor && make
./screen-editor --screen-directory /path/to/screens/
```

## Prerequisites

- SBCL with Quicklisp
- [CL3270](https://github.com/marcoxa/CL3270) loadable via ASDF
- Node.js and npm (for screen-editor frontend development)

## License

MIT
