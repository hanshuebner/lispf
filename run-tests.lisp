;;;; run-tests.lisp — Load and run all LISPF test suites.

(defun setup-registry (directory-path)
  (mapc (lambda (asd-pathname)
          (pushnew (make-pathname :name nil :type nil :version nil
                                  :defaults asd-pathname)
                   asdf:*central-registry*
                   :test #'equal))
        (directory (merge-pathnames #p"**/*.asd" directory-path))))

(setup-registry (make-pathname :defaults *load-truename* :name nil :type nil))

(ql:quickload :lispf/tests)

(unless (lispf-test:run-all-suites)
  (uiop:quit 1))
