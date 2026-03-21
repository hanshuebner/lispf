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

(defun save-editor-file (session)
  "Save the editor buffer to disk."
  (let ((path (editor-filepath session)))
    (when path
      (write-file-lines path (editor-lines session))
      (setf (editor-modified session) nil))))

(defun revert (session)
  "Reload the file from disk, discarding all changes and undo history.
Returns a message string."
  (let ((path (editor-filepath session)))
    (if (and path (probe-file path))
        (let ((lines (read-file-lines path)))
          (setf (editor-lines session) (or lines (list "")))
          (setf (editor-modified session) nil)
          (setf (editor-undo-stack session) nil)
          (setf (editor-pending-block session) nil)
          (format nil "Reverted to ~A" (file-namestring path)))
        "No file to revert to")))
