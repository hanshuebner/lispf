;;;; run-tests.lisp — Load all test suites and run them via the framework.

(load "load.lisp")
(asdf:load-system :lispf-edit)
(asdf:load-system :lispf-test)

(let ((*default-pathname-defaults* (asdf:system-source-directory :lispf)))
  (load "test/i18n-tests.lisp")
  (load "test/cursor-tests.lisp")
  (load "editor/test/editor-tests.lisp")
  (load "examples/guestbook/guestbook-tests.lisp"))

(unless (lispf-test:run-all-suites)
  (uiop:quit 1))
