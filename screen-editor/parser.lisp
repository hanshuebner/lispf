;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(in-package #:screen-editor)

(defconstant +screen-rows+ 24)
(defconstant +screen-columns+ 80)

;;; Constants

(defconstant +default-app-rows+ 20
  "Number of application rows for screens with command line.")

(defconstant +no-command-app-rows+ 21
  "Number of application rows for no-command screens.")

(defconstant +title-row+ 0)
(defconstant +command-row+ 21)
(defconstant +error-row+ 22)
(defconstant +keys-row+ 23)

(defparameter *month-names* #("JAN" "FEB" "MAR" "APR" "MAY" "JUN"
                                "JUL" "AUG" "SEP" "OCT" "NOV" "DEC"))

(defun format-editor-title (screen-name)
  "Format the title line: screen name left, app placeholder center, date/time right."
  (multiple-value-bind (sec min hour day month)
      (get-decoded-time)
    (declare (ignore sec))
    (format nil "~A~35T~A~68,1T~2,'0D-~A ~2,'0D:~2,'0D"
            (string-upcase (or screen-name ""))
            "Awesome App"
            day (aref *month-names* (1- month)) hour min)))

(defun split-screen-string (screen-string &optional screen-name no-command)
  "Split a .screen file's screen string into 24 display rows.
Adds framework rows: title (row 0), command (row 21 unless NO-COMMAND),
error (row 22), keys (row 23)."
  (let* ((app-rows (if no-command +no-command-app-rows+ +default-app-rows+))
         (lines (rest (split-sequence:split-sequence #\Newline screen-string)))
         ;; Remove trailing empty string from the final newline
         (lines (if (and lines (string= "" (car (last lines))))
                    (butlast lines)
                    lines))
         (rows (make-array +screen-rows+ :initial-element (format nil "~80A" ""))))
    ;; Row 0: title
    (setf (aref rows +title-row+)
          (format-editor-title (or screen-name "")))
    ;; App content rows
    (dotimes (i (min (length lines) app-rows))
      (setf (aref rows (1+ i)) (format nil "~80A" (nth i lines))))
    ;; Row 21: command line (unless no-command)
    (unless no-command
      (setf (aref rows +command-row+)
            (format nil "~80A" " Command ==>")))
    ;; Row 22: error message (empty)
    (setf (aref rows +error-row+) (format nil "~80A" ""))
    ;; Row 23: standard key labels
    (setf (aref rows +keys-row+)
          (format nil "~80A" " PF3 Exit  Enter Submit"))
    (coerce rows 'list)))

;;; Sexp screen data file reading

(defun parse-sexp-field (field-plist)
  "Parse a field plist from .screen format into a JSON-friendly alist.
Offsets fromRow by +1 to account for the framework title row."
  (let ((from (getf field-plist :from))
        (len (getf field-plist :len))
        (name-val (getf field-plist :name))
        (write-val (getf field-plist :write))
        (intense-val (getf field-plist :intense))
        (hidden-val (getf field-plist :hidden))
        (numeric-only-val (getf field-plist :numeric-only))
        (autoskip-val (getf field-plist :autoskip))
        (color-val (getf field-plist :color))
        (highlighting-val (getf field-plist :highlighting))
        (keepspaces-val (getf field-plist :keepspaces))
        (position-only-val (getf field-plist :position-only))
        (default-val (getf field-plist :default))
        (transient-val (getf field-plist :transient))
        (repeat-val (getf field-plist :repeat)))
    (list (cons "fromRow" (1+ (first from)))  ; offset for framework title row
          (cons "fromCol" (second from))
          (cons "len" (or len 1))
          (cons "name" (when name-val (string-downcase (string name-val))))
          (cons "anonymous" (if name-val nil t))
          (cons "write" (if write-val t nil))
          (cons "intense" (if intense-val t nil))
          (cons "hidden" (if hidden-val t nil))
          (cons "numericOnly" (if numeric-only-val t nil))
          (cons "autoskip" (if autoskip-val t nil))
          (cons "color" (if color-val (string-downcase (string color-val)) "default"))
          (cons "highlighting" (if highlighting-val (string-downcase (string highlighting-val)) "default"))
          (cons "keepspaces" (if keepspaces-val t nil))
          (cons "positionOnly" (if position-only-val t nil))
          (cons "default" (if default-val t nil))
          (cons "transient" (if transient-val t nil))
          (cons "repeat" (or repeat-val 1)))))

(defun parse-key-spec (spec)
  "Parse a key spec from .screen format into a JSON-friendly alist.
Input: (:enter \"Login\") or (:pf3 \"Exit\" :back t) or (:pf5 \"Register\" :goto register)
       (:pf7 \"Prev\" :hidden t)"
  (destructuring-bind (aid-kw label &rest rest) spec
    (let ((result (list (cons "aidKey" (string-downcase (string aid-kw)))
                        (cons "label" (string label)))))
      (when (getf rest :hidden)
        (setf result (append result (list (cons "hidden" t)))))
      (cond
        ((getf rest :back)
         (append result (list (cons "action" "back"))))
        ((getf rest :goto)
         (append result (list (cons "action" "goto")
                              (cons "gotoScreen" (string-downcase (string (getf rest :goto)))))))
        (t
         (append result (list (cons "action" "handler"))))))))

(defun parse-dynamic-area (area-plist)
  "Parse a dynamic area plist into a JSON-friendly alist.
Offsets rows by +1 to account for the framework title row."
  (destructuring-bind (from-row from-col) (getf area-plist :from)
    (destructuring-bind (to-row to-col) (getf area-plist :to)
      (list (cons "name" (string-downcase (string (getf area-plist :name))))
            (cons "fromRow" (1+ from-row))
            (cons "fromCol" from-col)
            (cons "toRow" (1+ to-row))
            (cons "toCol" to-col)))))

(defun parse-screen-data (data)
  "Parse a screen data plist into a JSON-friendly alist.
Expands app rows to 24 display rows with framework rows."
  (let* ((name (getf data :name))
         (screen-string (getf data :screen))
         (has-command (getf data :command))
         (menu-name (let ((m (getf data :menu)))
                      (when m (string-downcase (string m)))))
         (aliases (mapcar (lambda (a) (string-downcase (string a)))
                          (getf data :aliases)))
         (fields-raw (getf data :fields))
         (keys-raw (getf data :keys))
         (dynamic-areas-raw (getf data :dynamic-areas))
         (anonymous (getf data :anonymous))
         (navigable (getf data :navigable))
         (no-command (and (not has-command) (not menu-name)))
         (rows (split-screen-string screen-string name no-command))
         (fields (mapcar #'parse-sexp-field fields-raw))
         (keys (when keys-raw (mapcar #'parse-key-spec keys-raw)))
         (dynamic-areas (when dynamic-areas-raw
                          (mapcar #'parse-dynamic-area dynamic-areas-raw))))
    (append (list (cons "name" name)
                  (cons "rows" rows)
                  (cons "fields" fields))
            (when has-command (list (cons "command" t)))
            (when anonymous (list (cons "anonymous" t)))
            (when navigable (list (cons "navigable" t)))
            (when menu-name (list (cons "menu" menu-name)))
            (when aliases (list (cons "aliases" (coerce aliases 'vector))))
            (when keys (list (cons "keys" keys)))
            (when dynamic-areas (list (cons "dynamicAreas" dynamic-areas))))))

(defun parse-screen-file (path)
  "Parse a .sexp screen data file into a JSON-friendly alist."
  (let ((data (with-open-file (s path)
                (let ((*package* (find-package :lispf)))
                  (read s)))))
    (parse-screen-data data)))

