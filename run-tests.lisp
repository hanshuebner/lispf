;;;; run-tests.lisp — Load all test suites and run them via the framework.

(defun setup-registry (directory-path)
  (mapc (lambda (asd-pathname)
          (pushnew (make-pathname :name nil :type nil :version nil
                                  :defaults asd-pathname)
                   asdf:*central-registry*
                   :test #'equal))
        (directory (merge-pathnames #p"**/*.asd" directory-path))))

(setup-registry (make-pathname :defaults *load-truename* :name nil :type nil))

(ql:quickload :lispf)
(ql:quickload :lispf-edit)
(ql:quickload :lispf-test)
(ql:quickload :lispf-guestbook)

(let ((*default-pathname-defaults* (asdf:system-source-directory :lispf)))
  (load "test/i18n-tests.lisp")
  (load "test/cursor-tests.lisp")
  (load "editor/test/editor-tests.lisp")
  (load "examples/guestbook/guestbook-tests.lisp"))

(unless (lispf-test:run-all-suites)
  (uiop:quit 1))
