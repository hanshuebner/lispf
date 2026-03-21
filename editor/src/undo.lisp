;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; undo.lisp - Undo stack management

(in-package #:lispf-editor)

(defun save-undo-state (session)
  "Save current lines and modified flag for undo."
  (push (cons (copy-list (editor-lines session))
              (editor-modified session))
        (editor-undo-stack session))
  (when (> (length (editor-undo-stack session)) +max-undo+)
    (setf (editor-undo-stack session)
          (subseq (editor-undo-stack session) 0 +max-undo+))))

(defun undo (session)
  "Restore the previous state. Returns a message string."
  (let ((state (pop (editor-undo-stack session))))
    (if state
        (progn
          (setf (editor-lines session) (car state))
          (setf (editor-modified session) (cdr state))
          "UNDO completed")
        "Nothing to undo")))
