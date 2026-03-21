;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; file-io.lisp - File reading and writing

(in-package #:lispf-editor)

(defun read-file-lines (path)
  "Read a file into a list of strings (one per line)."
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (loop for line = (read-line s nil nil)
            while line collect line))))

(defun write-file-lines (path lines)
  "Write a list of strings to a file (one per line)."
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (line lines)
      (write-line line s))))
