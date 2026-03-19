(asdf:defsystem #:lispf-editor
  :author "Hans Huebner"
  :license "MIT"
  :description "Web-based visual editor for LISPF .screen files"
  :serial t
  :components ((:file "package")
               (:file "parser")
               (:file "emitter")
               (:file "api")
               (:file "static")
               (:file "server"))
  :depends-on (#:lispf #:hunchentoot #:yason #:alexandria))
