;;;; run-tests.lisp — CI test runner for all LISPF test suites.

(defpackage #:lispf-run-tests
  (:use #:cl)
  (:export #:run-all))

(in-package #:lispf-run-tests)

(defun load-test-suites ()
  (let ((*default-pathname-defaults* (asdf:system-source-directory :lispf)))
    (load "test/i18n-tests.lisp")
    (load "test/cursor-tests.lisp")
    (load "editor/test/editor-tests.lisp")
    (load "examples/guestbook/guestbook-tests.lisp")))

(defun run-all ()
  "Run all LISPF test suites.  Returns T if all passed."
  (load-test-suites)
  (let ((all-passed t))
    (dolist (suite '(lispf-key-layout-tests:run-all
                     lispf-i18n-tests:run-all
                     lispf-cursor-tests:run-all
                     lispf-editor-tests:run-all
                     lispf-guestbook-tests:run-all))
      (format t "~&~%=== ~A ===~%" (package-name (symbol-package suite)))
      (unless (funcall suite)
        (setf all-passed nil)))
    all-passed))
