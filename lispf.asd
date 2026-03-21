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
               (:file "application"))
  :depends-on (#:cl3270
               #:alexandria
               #:split-sequence
               #:cl-ppcre
               #:bordeaux-threads)
  :in-order-to ((asdf:test-op (asdf:test-op #:lispf/tests))))

(asdf:defsystem #:lispf/tests
  :author "Hans Hübner"
  :license "MIT"
  :description "Unit tests for the LISPF framework"
  :serial t
  :pathname "test/"
  :components ((:file "key-layout-tests"))
  :depends-on (#:lispf #:lispf-test)
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :lispf-key-layout-tests :run-all)))
