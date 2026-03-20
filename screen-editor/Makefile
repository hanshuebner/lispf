BUILDAPP ?= buildapp
SBCL ?= sbcl
TARGET = screen-editor

FRONTEND_DIR = frontend
FRONTEND_DIST = $(FRONTEND_DIR)/dist

QUICKLISP_SETUP = $(HOME)/quicklisp/setup.lisp
ASDF_DIR = $(shell cd .. && pwd)
SCREENS_DIR ?= $(ASDF_DIR)/examples/guestbook/screens/

.PHONY: all clean frontend frontend-dev backend-dev

all: $(TARGET)

frontend: $(FRONTEND_DIST)

$(FRONTEND_DIST): $(FRONTEND_DIR)/package.json $(wildcard $(FRONTEND_DIR)/src/*) $(wildcard $(FRONTEND_DIR)/src/**/*) $(FRONTEND_DIR)/index.html
	cd $(FRONTEND_DIR) && npm install && npm run build

$(TARGET): $(FRONTEND_DIST) package.lisp parser.lisp emitter.lisp api.lisp static.lisp server.lisp main.lisp
	$(BUILDAPP) \
		--output $(TARGET) \
		--load $(QUICKLISP_SETUP) \
		--eval '(dolist (d (list "" "CL3270/" "screen-editor/")) (push (merge-pathnames d "$(ASDF_DIR)/") asdf:*central-registry*))' \
		--load-system lispf-editor \
		--load main.lisp \
		--entry screen-editor:main

frontend-dev:
	cd $(FRONTEND_DIR) && npm run dev

backend-dev:
	$(SBCL) --load $(QUICKLISP_SETUP) \
		--eval '(dolist (d (list "" "CL3270/" "screen-editor/")) (push (merge-pathnames d "$(ASDF_DIR)/") asdf:*central-registry*))' \
		--eval '(ql:quickload :lispf-editor)' \
		--eval '(screen-editor:start-server "$(SCREENS_DIR)" :port 8388)' \
		--eval '(format t "~%Backend running on port 8388. Use (screen-editor:stop-server) to stop.~%")'

clean:
	rm -f $(TARGET)
	rm -rf $(FRONTEND_DIST)
