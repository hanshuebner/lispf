;;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;;; Entry point for standalone screen-editor binary

(in-package #:screen-editor)

(defun main-usage ()
  (format *error-output*
          "Usage: screen-editor [OPTIONS]~%~
           ~%  --port PORT                HTTP port (default: 8080)~
           ~%  --screen-directory DIR     Directory containing .screen files~
           ~%                             (default: screens/)~%"))

(defun parse-args (args)
  "Parse command-line arguments. Returns (values port screen-directory)."
  (let ((port 8080)
        (screen-directory nil))
    (loop for (key value) on args by #'cddr
          do (cond
               ((string= key "--port")
                (setf port (parse-integer value)))
               ((string= key "--screen-directory")
                (setf screen-directory (uiop:ensure-directory-pathname value)))
               (t
                (format *error-output* "Unknown option: ~A~%" key)
                (main-usage)
                (uiop:quit 1))))
    (unless screen-directory
      (setf screen-directory (merge-pathnames "screens/" (uiop:getcwd))))
    (values port screen-directory)))

(defun main (args)
  "Entry point for the standalone binary."
  (multiple-value-bind (port screen-directory)
      (parse-args (rest args))
    (unless (uiop:directory-exists-p screen-directory)
      (format *error-output* "Error: screen directory not found: ~A~%" screen-directory)
      (uiop:quit 1))
    (start-server screen-directory :port port)
    (format t "Press Ctrl-C to stop.~%")
    (handler-case
        (loop (sleep 3600))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #-(or sbcl ccl) condition
       ()
        (format t "~%Shutting down...~%")
        (stop-server)
        (uiop:quit 0)))))
