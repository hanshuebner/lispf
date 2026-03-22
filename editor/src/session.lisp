;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; session.lisp - Editor session class, layout configuration, and constants

(in-package #:lispf-editor)

;;; Constants

(defconstant +data-width+ 72
  "Width of the data area on each line.")

(defconstant +prefix-width+ 6
  "Width of the prefix area on each line.")

(defconstant +max-undo+ 50
  "Maximum number of undo states to keep.")

;;; Editor layout configuration

(defclass editor-layout ()
  ((status-row :initarg :status-row :initform 0 :accessor layout-status-row
               :documentation "Screen row for status line.")
   (message-row :initarg :message-row :initform 1 :accessor layout-message-row
                :documentation "Screen row for error/info messages.")
   (data-start-row :initarg :data-start-row :initform 2 :accessor layout-data-start-row
                   :documentation "First screen row with file data.")
   (data-end-row :initarg :data-end-row :initform 22 :accessor layout-data-end-row
                 :documentation "Last screen row with file data.")
   (command-row :initarg :command-row :initform 23 :accessor layout-command-row
                :documentation "Screen row for command input.")
   (command-prompt :initarg :command-prompt :initform "====>" :accessor layout-command-prompt
                   :documentation "Command prompt string.")
   (scale-row :initarg :scale-row :initform nil :accessor layout-scale-row
              :documentation "Screen row for column scale, or NIL to disable.")
   (key-labels-row :initarg :key-labels-row :initform nil :accessor layout-key-labels-row
                   :documentation "Screen row for key labels, or NIL to disable.")
   (data-col-start :initarg :data-col-start :initform 7 :accessor layout-data-col-start
                   :documentation "Column where data field begins (after prefix + attr byte).")
   (prefix-width :initarg :prefix-width :initform 6 :accessor layout-prefix-width
                 :documentation "Width of the prefix area.")))

(defgeneric page-size (object)
  (:documentation "Return the number of visible data lines."))

(defmethod page-size ((layout editor-layout))
  (1+ (- (layout-data-end-row layout) (layout-data-start-row layout))))

(defun validate-layout (layout)
  "Validate an editor-layout for consistency. Returns T or signals an error."
  (let ((status (layout-status-row layout))
        (message (layout-message-row layout))
        (data-start (layout-data-start-row layout))
        (data-end (layout-data-end-row layout))
        (command (layout-command-row layout))
        (scale (layout-scale-row layout))
        (keys (layout-key-labels-row layout)))
    (flet ((check-range (name row)
             (unless (and (integerp row) (<= 0 row 23))
               (error "~A row ~A is out of range 0-23" name row))))
      ;; Check ranges
      (check-range "Status" status)
      (check-range "Message" message)
      (check-range "Data start" data-start)
      (check-range "Data end" data-end)
      (check-range "Command" command)
      (when scale (check-range "Scale" scale))
      (when keys (check-range "Key labels" keys))
      ;; Check data area
      (unless (< data-start data-end)
        (error "Data start row ~D must be less than data end row ~D" data-start data-end))
      ;; Check no overlaps with fixed rows
      (let ((fixed (list (cons "Status" status) (cons "Message" message)
                         (cons "Command" command))))
        (when scale (push (cons "Scale" scale) fixed))
        (when keys (push (cons "Key labels" keys) fixed))
        ;; Check fixed rows don't overlap each other
        (loop for (a . rest) on fixed
              do (loop for b in rest
                       when (= (cdr a) (cdr b))
                         do (error "~A and ~A both on row ~D" (car a) (car b) (cdr a))))
        ;; Check fixed rows don't overlap data area
        (dolist (f fixed)
          (when (and (>= (cdr f) data-start) (<= (cdr f) data-end))
            (error "~A row ~D overlaps with data area (~D-~D)"
                   (car f) (cdr f) data-start data-end))))))
  t)

(defun make-default-layout ()
  "Create the default XEDIT-style layout."
  (let ((layout (make-instance 'editor-layout)))
    (validate-layout layout)
    layout))

;;; Editor session class

(defclass editor-session (lspf:session)
  ((layout :initform (make-default-layout) :accessor editor-layout
           :documentation "Screen layout configuration.")
   (lines :initform (list "") :accessor editor-lines
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
               :documentation "List of (lines-copy . modified-flag) for undo.")
   (display-name :initform nil :accessor editor-display-name
                 :documentation "Override display name for the info line (e.g. 'New Message').")
   (restricted :initform nil :accessor editor-restricted-p
               :documentation "When T, FILE/SUBMIT/CANCEL/REVERT commands are disabled.")
   (next-cursor :initform nil :accessor editor-next-cursor
                :documentation "When set to (row . col), overrides cursor positioning on next display.")
   (justify-range :initform nil :accessor editor-justify-range
                  :documentation "When set to (start . count), the JJ-marked range for JUSTIFY command.")
   (current-line :initform 0 :accessor editor-current-line
                 :documentation "0-based real line index of the current (focus) line.")
   (alteration-count :initform 0 :accessor editor-alteration-count
                     :documentation "Number of modifications since last save.")))

(defmethod page-size ((session editor-session))
  (page-size (editor-layout session)))

(defun make-test-session (lines &key layout)
  "Create an editor session for testing (no application binding needed)."
  (let ((s (make-instance 'editor-session)))
    (setf (editor-lines s) (copy-list lines))
    (when layout
      (setf (editor-layout s) layout))
    s))
