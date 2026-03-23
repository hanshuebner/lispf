;;;; run-tests.lisp — CI test runner for all LISPF test suites.

(load "load.lisp")
(asdf:load-system :lispf-edit)
(asdf:load-system :lispf-test)

(let ((base (asdf:system-source-directory :lispf)))
  (load (merge-pathnames "test/i18n-tests.lisp" base))
  (load (merge-pathnames "test/cursor-tests.lisp" base))
  (load (merge-pathnames "editor/test/editor-tests.lisp" base))
  (load (merge-pathnames "examples/guestbook/guestbook-tests.lisp" base)))

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
