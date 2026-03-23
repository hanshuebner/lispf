;;;; load-tests.lisp — Load all LISPF test suites (without running them).
;;;;
;;;; Call from a run-tests.lisp after setting up ASDF registry and Quicklisp.
;;;; After loading, call (lispf-test:run-all-suites) to run.

(ql:quickload :lispf)
(ql:quickload :lispf-edit)
(ql:quickload :lispf-test)
(ql:quickload :lispf-guestbook)

(let ((*default-pathname-defaults* (asdf:system-source-directory :lispf)))
  (load "test/i18n-tests.lisp")
  (load "test/cursor-tests.lisp")
  (load "test/help-tests.lisp")
  (load "editor/test/editor-tests.lisp")
  (load "examples/guestbook/guestbook-tests.lisp"))
