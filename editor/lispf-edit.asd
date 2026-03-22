(asdf:defsystem #:lispf-edit
  :author "Hans Hübner"
  :license "MIT"
  :description "LISPF Editor - Mainframe-style file editor inspired by ISPF/XEDIT"
  :serial t
  :pathname "src/"
  :components ((:file "packages")
               (:file "session")
               (:file "file-io")
               (:file "undo")
               (:file "buffer")
               (:file "prefix")
               (:file "commands")
               (:file "screen")
               (:file "application"))
  :depends-on (#:lispf
               #:alexandria
               #:split-sequence
               #:cl-ppcre))
