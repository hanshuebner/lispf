(let ((here (make-pathname :defaults *load-truename* :name nil :type nil)))
  (pushnew here asdf:*central-registry* :test #'equal)
  (pushnew (merge-pathnames "CL3270/" here) asdf:*central-registry* :test #'equal))

(asdf:load-system "lispf")
