;;;; run-tests.lisp — CI test runner for all LISPF test suites.

(load "load.lisp")
(asdf:load-system :lispf-edit)
(asdf:load-system :lispf-test)

(let ((*default-pathname-defaults* (asdf:system-source-directory :lispf)))
  (load "test/i18n-tests.lisp")
  (load "test/cursor-tests.lisp")
  (load "editor/test/editor-tests.lisp")
  (load "examples/guestbook/guestbook-tests.lisp"))

(let ((all-passed t))
  (dolist (suite '(lispf-key-layout-tests:run-all
                   lispf-i18n-tests:run-all
                   lispf-cursor-tests:run-all
                   lispf-editor-tests:run-all
                   lispf-guestbook-tests:run-all))
    (format t "~&~%=== ~A ===~%" (package-name (symbol-package suite)))
    (unless (funcall suite)
      (setf all-passed nil)))
  (unless all-passed
    (format t "~&~%Some test suites failed.~%")
    (uiop:quit 1))
  (format t "~&~%All test suites passed.~%"))
