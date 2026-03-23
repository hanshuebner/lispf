;;;; load-tests.lisp — Load all LISPF test suites (without running them).
;;;;
;;;; Call from a run-tests.lisp after setting up ASDF registry and Quicklisp.
;;;; After loading, call (lispf-test:run-all-suites) to run.

(ql:quickload :lispf/tests)
