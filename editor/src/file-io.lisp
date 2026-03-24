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

(defun open-file (session path &key display-name restricted)
  "Open PATH into SESSION, resetting all editor state.
Reads the file (or starts with one empty line if new), clears undo, scroll,
pending block, and sets display-name/restricted as given."
  (let ((lines (read-file-lines path)))
    (setf (editor-filepath session) path
          (editor-filename session) (file-namestring path)
          (editor-display-name session) display-name
          (editor-restricted-p session) restricted
          (editor-lines session) (or lines (list ""))
          (editor-undo-stack session) nil
          (editor-pending-block session) nil
          (editor-justify-range session) nil
          (editor-top-line session) 0
          (editor-col-offset session) 0
          (editor-current-line session) 0)))

(defun save-editor-file (session)
  "Save the editor buffer to disk."
  (let ((path (editor-filepath session)))
    (when path
      (write-file-lines path (editor-lines session))
      (setf (editor-undo-stack session) nil))))

(defun revert (session)
  "Reload the file from disk, discarding all changes and undo history.
Returns a message string."
  (let ((path (editor-filepath session)))
    (if (and path (probe-file path))
        (let ((lines (read-file-lines path)))
          (setf (editor-lines session) (or lines (list ""))
                (editor-undo-stack session) nil
                (editor-pending-block session) nil)
          "Last saved version loaded")
        "No file to revert to")))
