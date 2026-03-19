(asdf:defsystem #:lispf-test
  :author "Hans Hübner"
  :license "MIT"
  :description "Test framework for LISPF applications using s3270"
  :serial t
  :pathname ""
  :components ((:file "packages")
               (:file "s3270")
               (:file "client")
               (:file "assertions")
               (:file "harness"))
  :depends-on (#:lispf
               #:bordeaux-threads
               #:alexandria
               #:split-sequence))
