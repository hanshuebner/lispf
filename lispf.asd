(asdf:defsystem #:lispf
  :author "Hans Hübner"
  :license "MIT"
  :description "LISPF - Lisp Interactive Screen Programming Facility"
  :serial t
  :pathname "src/"
  :components ((:file "screen")
               (:file "registry")
               (:file "field-binding")
               (:file "conditions")
               (:file "key-handler")
               (:file "application"))
  :depends-on (#:cl3270
               #:alexandria
               #:split-sequence
               #:cl-ppcre
               #:bordeaux-threads))
