;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(in-package #:screen-editor)

;;; Sexp screen data file writing

(defun screen-rows-to-string (rows)
  "Convert 24 display rows to a .screen file string (21 app rows).
Strips framework rows (0, 22, 23), keeps only app rows 1-21.
Strips trailing spaces from each row."
  (let* ((app-rows (subseq rows 1 22))  ; rows 1-21 (21 app rows)
         (trimmed (mapcar (lambda (row) (string-right-trim " " row)) app-rows))
         (end (1+ (or (position-if-not (lambda (r) (string= r "")) trimmed :from-end t)
                      -1))))
    (format nil "~%~{~A~%~}" (subseq trimmed 0 end))))

(defun field-alist-to-plist (field)
  "Convert a JSON-style field alist to a .screen plist for writing.
Offsets fromRow by -1 to convert from display coordinates to app coordinates."
  (let ((from-row (1- (cdr (assoc "fromRow" field :test #'string=))))  ; offset for framework title row
        (from-col (cdr (assoc "fromCol" field :test #'string=)))
        (len (cdr (assoc "len" field :test #'string=)))
        (name (cdr (assoc "name" field :test #'string=)))
        (intense-p (cdr (assoc "intense" field :test #'string=)))
        (write-p (cdr (assoc "write" field :test #'string=)))
        (hidden-p (cdr (assoc "hidden" field :test #'string=)))
        (numeric-only-p (cdr (assoc "numericOnly" field :test #'string=)))
        (autoskip-p (cdr (assoc "autoskip" field :test #'string=)))
        (color (cdr (assoc "color" field :test #'string=)))
        (highlighting (cdr (assoc "highlighting" field :test #'string=)))
        (keepspaces-p (cdr (assoc "keepspaces" field :test #'string=)))
        (position-only-p (cdr (assoc "positionOnly" field :test #'string=)))
        (default-p (cdr (assoc "default" field :test #'string=)))
        (transient-p (cdr (assoc "transient" field :test #'string=)))
        (repeat-val (cdr (assoc "repeat" field :test #'string=))))
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
  (let ((aid-key (cdr (assoc "aidKey" key-alist :test #'string=)))
        (label (cdr (assoc "label" key-alist :test #'string=)))
        (action (cdr (assoc "action" key-alist :test #'string=)))
        (goto-screen (cdr (assoc "gotoScreen" key-alist :test #'string=))))
    (let ((kw (intern (string-upcase aid-key) :keyword)))
      (append (list kw label)
              (cond
                ((string-equal action "back") (list :back t))
                ((and (string-equal action "goto") goto-screen)
                 (list :goto (intern (string-upcase goto-screen) :lispf)))
                (t nil))))))

(defun emit-screen-data (screen)
  "Convert a JSON-style screen alist to a sexp plist string."
  (let* ((name (cdr (assoc "name" screen :test #'string=)))
         (rows (cdr (assoc "rows" screen :test #'string=)))
         (fields (cdr (assoc "fields" screen :test #'string=)))
         (keys (cdr (assoc "keys" screen :test #'string=)))
         (screen-string (screen-rows-to-string rows))
         (field-plists (mapcar #'field-alist-to-plist fields))
         (key-plists (when keys (mapcar #'key-alist-to-plist keys)))
         (*package* (find-package :lispf))
         (*print-case* :downcase))
    (with-output-to-string (s)
      (format s "(:name ~S~% :screen ~S~% :fields (" name screen-string)
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
      (format s ")~%"))))

(defun write-screen-file (screen path)
  "Write a screen alist to a .sexp file at PATH."
  (let ((content (emit-screen-data screen))
        (temp-path (format nil "~A.tmp" path)))
    (write-string-into-file content temp-path :if-exists :supersede)
    (rename-file temp-path path)))
