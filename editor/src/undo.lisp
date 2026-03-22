;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; undo.lisp - Undo stack management
;;;;
;;;; The undo stack depth serves as the alteration count.
;;;; editor-modified is true when the stack is non-empty.

(in-package #:lispf-editor)

(defun save-undo-state (session)
  "Save current lines for undo."
  (push (copy-list (editor-lines session))
        (editor-undo-stack session))
  (when (> (length (editor-undo-stack session)) +max-undo+)
    (setf (editor-undo-stack session)
          (subseq (editor-undo-stack session) 0 +max-undo+))))

(defun undo (session)
  "Restore the previous state. Returns a message string."
  (let ((state (pop (editor-undo-stack session))))
    (if state
        (progn
          (setf (editor-lines session) state)
          "UNDO completed")
        "Nothing to undo")))
