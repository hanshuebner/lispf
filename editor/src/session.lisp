;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; session.lisp - Editor session class and constants

(in-package #:lispf-editor)

;;; Constants

(defconstant +page-size+ 17
  "Number of data lines visible on the edit screen.")

(defconstant +data-width+ 72
  "Width of the data area on each line.")

(defconstant +prefix-width+ 6
  "Width of the prefix area on each line.")

(defconstant +max-undo+ 50
  "Maximum number of undo states to keep.")

;;; Editor session class

(defclass editor-session (lspf:session)
  ((lines :initform (list "") :accessor editor-lines
          :documentation "List of strings, one per line.")
   (filename :initform nil :accessor editor-filename)
   (filepath :initform nil :accessor editor-filepath)
   (modified :initform nil :accessor editor-modified)
   (top-line :initform 0 :accessor editor-top-line
             :documentation "0-based virtual index of the first visible row.
Virtual 0 = Top-of-Data marker, 1..N = file lines, N+1 = Bottom-of-Data marker.")
   (col-offset :initform 0 :accessor editor-col-offset
               :documentation "0-based column offset for horizontal scrolling.")
   (last-find :initform nil :accessor editor-last-find
              :documentation "Last FIND search string for RFind.")
   (last-find-line :initform 0 :accessor editor-last-find-line
                   :documentation "Line where last FIND matched, for advancing on RFind.")
   (last-change :initform nil :accessor editor-last-change
                :documentation "Last CHANGE arguments (from to all-p) for RChange.")
   (pending-block :initform nil :accessor editor-pending-block
                  :documentation "Pending block command: (cmd start-line [count]) or nil.")
   (undo-stack :initform nil :accessor editor-undo-stack
               :documentation "List of (lines-copy . modified-flag) for undo.")))

(defun make-test-session (lines)
  "Create an editor session for testing (no application binding needed)."
  (let ((s (make-instance 'editor-session)))
    (setf (editor-lines s) (copy-list lines))
    s))
