(asdf:defsystem #:screen-editor
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
  :depends-on (#:cl3270 #:lispf #:hunchentoot #:yason #:alexandria))
