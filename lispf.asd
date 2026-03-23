(asdf:defsystem #:lispf
  :author "Hans Hübner"
  :license "MIT"
  :description "LISPF - Lisp Interactive Screen Programming Facility"
  :serial t
  :pathname "src/"
  :components ((:file "screen")
               (:file "variables")
               (:file "i18n")
               (:file "registry")
               (:file "field-binding")
               (:file "conditions")
               (:file "key-handler")
               (:file "application")
               (:file "help-parser")
               (:file "help")
               (:file "help-viewer"))
  :depends-on (#:cl3270
               #:alexandria
               #:split-sequence
               #:cl-ppcre
               #:bordeaux-threads)
  :in-order-to ((asdf:test-op (asdf:test-op #:lispf/tests))))

(asdf:defsystem #:lispf/tests
  :author "Hans Hübner"
  :license "MIT"
  :description "All tests for LISPF framework, editor, and examples"
  :serial t
  :depends-on (#:lispf #:lispf-edit #:lispf-test #:lispf-guestbook)
  :components ((:module "test"
                :components ((:file "key-layout-tests")
                             (:file "i18n-tests")
                             (:file "cursor-tests")
                             (:file "help-tests")))
               (:module "editor/test"
                :components ((:file "editor-tests")))
               (:module "examples/guestbook"
                :components ((:file "guestbook-tests"))))
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :lispf-test :run-all-suites)))
