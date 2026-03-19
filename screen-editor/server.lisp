;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(in-package #:screen-editor)

(defvar *acceptor* nil)

(defun start-server (screen-directory &key (port 8080))
  "Start the screen editor web server on PORT.
SCREEN-DIRECTORY is the directory containing .screen files."
  (let ((dir (uiop:ensure-directory-pathname screen-directory)))
    (unless (uiop:directory-exists-p dir)
      (error "Screen directory does not exist: ~A" dir)))
  (when *acceptor*
    (stop-server))
  (setf *screen-directory* (truename screen-directory))
  (setf *acceptor*
        (make-instance 'hunchentoot:easy-acceptor
                       :port port))
  (push (create-static-dispatcher) hunchentoot:*dispatch-table*)
  (define-api-routes)
  (hunchentoot:start *acceptor*)
  (format t "Screen editor started: http://localhost:~D/~%" port)
  *acceptor*)

(defun stop-server ()
  "Stop the screen editor web server."
  (when *acceptor*
    (hunchentoot:stop *acceptor*)
    (setf *acceptor* nil)
    (format t "Screen editor stopped~%")))
