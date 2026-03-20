;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(in-package #:screen-editor)

;;; Sexp screen data file writing

(defun jref (alist key)
  "Look up KEY (a string) in a JSON-style alist."
  (cdr (assoc key alist :test #'string=)))

(defun screen-rows-to-string (rows &key no-command)
  "Convert 24 display rows to a .screen file string.
Strips framework rows (0, 21 command, 22, 23), keeps only app rows.
Without NO-COMMAND: rows 1-20 (20 app rows).
With NO-COMMAND: rows 1-21 (21 app rows).
Strips trailing spaces from each row."
  (let* ((last-app-row (if no-command 22 21))
         (app-rows (subseq rows 1 last-app-row))
         (trimmed (mapcar (lambda (row) (string-right-trim " " row)) app-rows))
         (end (1+ (or (position-if-not (lambda (r) (string= r "")) trimmed :from-end t)
                      -1))))
    (format nil "~%~{~A~%~}" (subseq trimmed 0 end))))

(defun field-alist-to-plist (field)
  "Convert a JSON-style field alist to a .screen plist for writing.
Offsets fromRow by -1 to convert from display coordinates to app coordinates."
  (let ((from-row (1- (jref field "fromRow")))
        (from-col (jref field "fromCol"))
        (len (jref field "len"))
        (name (jref field "name"))
        (intense-p (jref field "intense"))
        (write-p (jref field "write"))
        (hidden-p (jref field "hidden"))
        (numeric-only-p (jref field "numericOnly"))
        (autoskip-p (jref field "autoskip"))
        (color (jref field "color"))
        (highlighting (jref field "highlighting"))
        (keepspaces-p (jref field "keepspaces"))
        (position-only-p (jref field "positionOnly"))
        (default-p (jref field "default"))
        (transient-p (jref field "transient"))
        (repeat-val (jref field "repeat")))
    (append
     (list :from (list from-row from-col) :len len)
     (when name (list :name (intern (string-upcase name) :lispf)))
     (when intense-p (list :intense t))
     (when write-p (list :write t))
     (when hidden-p (list :hidden t))
     (when numeric-only-p (list :numeric-only t))
     (when autoskip-p (list :autoskip t))
     (when (and color (not (string-equal color "default")))
       (list :color (intern (string-upcase color) :lispf)))
     (when (and highlighting (not (string-equal highlighting "default")))
       (list :highlighting (intern (string-upcase highlighting) :lispf)))
     (when keepspaces-p (list :keepspaces t))
     (when position-only-p (list :position-only t))
     (when default-p (list :default t))
     (when transient-p (list :transient t))
     (when (and repeat-val (> repeat-val 1)) (list :repeat repeat-val)))))

(defun key-alist-to-plist (key-alist)
  "Convert a JSON-style key alist to a .screen plist for writing."
  (let ((aid-key (jref key-alist "aidKey"))
        (label (jref key-alist "label"))
        (action (jref key-alist "action"))
        (goto-screen (jref key-alist "gotoScreen"))
        (hidden (jref key-alist "hidden")))
    (let ((kw (intern (string-upcase aid-key) :keyword)))
      (append (list kw label)
              (when hidden (list :hidden t))
              (cond
                ((string-equal action "back") (list :back t))
                ((and (string-equal action "goto") goto-screen)
                 (list :goto (intern (string-upcase goto-screen) :lispf)))
                (t nil))))))

(defun dynamic-area-alist-to-plist (area)
  "Convert a JSON-style dynamic area alist to a .screen plist for writing.
Offsets rows by -1 to convert from display coordinates to app coordinates."
  (list :name (intern (string-upcase (jref area "name")) :lispf)
        :from (list (1- (jref area "fromRow")) (jref area "fromCol"))
        :to (list (1- (jref area "toRow")) (jref area "toCol"))))

(defun emit-screen-data (screen)
  "Convert a JSON-style screen alist to a sexp plist string."
  (let* ((name (jref screen "name"))
         (rows (jref screen "rows"))
         (fields (jref screen "fields"))
         (keys (jref screen "keys"))
         (has-command (jref screen "command"))
         (anonymous (jref screen "anonymous"))
         (navigable (jref screen "navigable"))
         (menu-name (jref screen "menu"))
         (aliases-raw (jref screen "aliases"))
         (aliases (when aliases-raw (coerce aliases-raw 'list)))
         (dynamic-areas (jref screen "dynamicAreas"))
         (no-command (and (not has-command) (not menu-name)))
         (screen-string (screen-rows-to-string rows :no-command no-command))
         (field-plists (mapcar #'field-alist-to-plist fields))
         (key-plists (when keys (mapcar #'key-alist-to-plist keys)))
         (dynamic-area-plists (when dynamic-areas
                                (mapcar #'dynamic-area-alist-to-plist dynamic-areas)))
         (*package* (find-package :lispf))
         (*print-case* :downcase))
    (with-output-to-string (s)
      (format s "(:name ~S" name)
      (when anonymous
        (format s "~% :anonymous t"))
      (when has-command
        (format s "~% :command t"))
      (when navigable
        (format s "~% :navigable t"))
      (when (and menu-name (not (equal menu-name :null)))
        (format s "~% :menu ~A" menu-name))
      (when aliases
        (format s "~% :aliases (")
        (loop for (a . rest) on aliases
              do (format s "~S" a)
              when rest do (format s " "))
        (format s ")"))
      (format s "~% :screen ~S~% :fields (" screen-string)
      (loop for (field . rest) on field-plists
            do (prin1 field s)
            when rest do (format s "~%          "))
      (format s ")")
      (when key-plists
        (format s "~% :keys (")
        (loop for (key-plist . rest) on key-plists
              do (prin1 key-plist s)
              when rest do (format s "~%        "))
        (format s ")"))
      (when dynamic-area-plists
        (format s "~% :dynamic-areas (")
        (loop for (area . rest) on dynamic-area-plists
              do (prin1 area s)
              when rest do (format s "~%                "))
        (format s ")"))
      (format s ")~%"))))

(defun write-screen-file (screen path)
  "Write a screen alist to a .sexp file at PATH."
  (let ((content (emit-screen-data screen))
        (temp-path (format nil "~A.tmp" path)))
    (write-string-into-file content temp-path :if-exists :supersede)
    (rename-file temp-path path)))
