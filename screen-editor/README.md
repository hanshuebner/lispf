# Screen Editor

Web-based visual editor for LISPF `.screen` files.

## Development

### Prerequisites

- SBCL with Quicklisp
- Node.js and npm
- LISPF system loadable via ASDF

### Running the Lisp backend

Start SBCL and load the system:

```lisp
(ql:quickload "lispf-editor")
(screen-editor:start-server "screens/" :port 8388)
```

The backend serves the REST API on port 8388 during development.

### Running the frontend dev server

```bash
cd frontend
npm install
npm run dev
```

The Vite dev server runs on http://localhost:5173 and proxies `/api`
requests to the Lisp backend on port 8388.

## Production Build

### Building the standalone binary

```bash
make
```

This builds the frontend and creates a standalone `screen-editor`
binary using buildapp.  Requires `buildapp` to be installed (available
via Quicklisp: `(ql:quickload "buildapp")`).

### Running the binary

The frontend assets are embedded in the binary at build time, so the
binary is self-contained.  By default it looks for a `screens/`
subdirectory in the current directory:

```bash
./screen-editor [--port 8080] [--screen-directory /path/to/screens/]
```

- `--port` defaults to 8080
- `--screen-directory` defaults to `screens/`

The server will exit with an error if the screen directory does not
exist.
