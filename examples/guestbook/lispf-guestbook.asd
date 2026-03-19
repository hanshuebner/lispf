(asdf:defsystem #:lispf-guestbook
  :author "Hans Hübner"
  :license "MIT"
  :description "Example guestbook application for LISPF"
  :serial t
  :pathname ""
  :components ((:file "guestbook"))
  :depends-on (#:lispf)
  :in-order-to ((asdf:test-op (asdf:test-op #:lispf-guestbook/tests))))

(asdf:defsystem #:lispf-guestbook/tests
  :author "Hans Hübner"
  :license "MIT"
  :description "Tests for the LISPF guestbook example"
  :serial t
  :pathname ""
  :components ((:file "guestbook-tests"))
  :depends-on (#:lispf-guestbook #:lispf-test)
  :perform (asdf:test-op (op c)
             (uiop:symbol-call :lispf-guestbook-tests :run-all)))
