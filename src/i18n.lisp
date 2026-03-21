;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; i18n.lisp
;;;;
;;;; Internationalization support for lispf applications.
;;;; Message catalogs are s-expression plists keyed by English default strings.
;;;; Format strings in translations are validated for matching directive counts.

(in-package #:lispf)

;;; Message catalog

(defvar *message-catalogs* (make-hash-table :test 'eq)
  "Per-application message catalogs. Keys are application objects,
values are hash tables mapping English strings to translations.")

(defun count-format-directives (format-string)
  "Count the number of format directives that consume arguments in FORMAT-STRING.
Counts ~A, ~S, ~D, ~B, ~O, ~X, ~E, ~F, ~G, ~$, ~R, ~P, ~C, ~W and their
variants. Does not count ~%, ~&, ~|, ~~, ~T, ~*, ~?, ~{, ~}, ~[, ~], ~(, ~).
Handles ~:P and ~:@P (which don't consume but modify previous)."
  (let ((count 0)
        (i 0)
        (len (length format-string)))
    (loop while (< i len)
          do (cond
               ((and (char= (char format-string i) #\~)
                     (< (1+ i) len))
                (let ((next-pos (1+ i)))
                  ;; Skip prefix parameters (digits, commas, #, v, ')
                  (loop while (and (< next-pos len)
                                   (let ((ch (char format-string next-pos)))
                                     (or (digit-char-p ch)
                                         (member ch '(#\, #\# #\v #\V #\')))))
                        do (when (char= (char format-string next-pos) #\')
                             (incf next-pos)) ; skip char after '
                           (incf next-pos))
                  ;; Skip : and @ modifiers
                  (loop while (and (< next-pos len)
                                   (member (char format-string next-pos) '(#\: #\@)))
                        do (incf next-pos))
                  (when (< next-pos len)
                    (let ((directive (char-upcase (char format-string next-pos))))
                      (when (member directive '(#\A #\S #\D #\B #\O #\X #\E #\F
                                                #\G #\$ #\R #\C #\W))
                        (incf count))))
                  (setf i (1+ next-pos))))
               (t (incf i))))
    count))

(defun validate-translation (english translation)
  "Check that TRANSLATION has the same number of format directives as ENGLISH.
Returns T if valid, signals a warning if not."
  (let ((en-count (count-format-directives english))
        (tr-count (count-format-directives translation)))
    (if (= en-count tr-count)
        t
        (progn
          (warn "Translation format mismatch for ~S: ~
                 English has ~D directive~:P, translation has ~D"
                english en-count tr-count)
          nil))))

(defun load-message-catalog (path)
  "Load a message catalog from PATH. The file contains a plist of
English-string/translation pairs as s-expressions.

Example file:
  (\"Command ==>\" \"Kommando ==>\"
   \"~A: unknown command\" \"~A: unbekannter Befehl\"
   \"Prev\" \"Zur\\\"uck\"
   \"Next\" \"Weiter\")

Returns a hash table mapping English strings to translations."
  (let ((catalog (make-hash-table :test 'equal)))
    (with-open-file (s path :direction :input :if-does-not-exist nil)
      (when s
        (let ((data (read s nil nil)))
          (loop for (key value) on data by #'cddr
                when (and (stringp key) (stringp value))
                  do (validate-translation key value)
                     (setf (gethash key catalog) value)))))
    catalog))

(defun set-message-catalog (application catalog-or-path)
  "Set the message catalog for APPLICATION.
CATALOG-OR-PATH is either a hash table (from load-message-catalog)
or a pathname to a message catalog file."
  (let ((catalog (etypecase catalog-or-path
                   (hash-table catalog-or-path)
                   (pathname (load-message-catalog catalog-or-path))
                   (string (load-message-catalog (pathname catalog-or-path))))))
    (setf (gethash application *message-catalogs*) catalog)))

(defun get-message-catalog (application)
  "Get the message catalog for APPLICATION, or nil."
  (gethash application *message-catalogs*))

(defun msg (english &rest format-args)
  "Look up ENGLISH in the current application's message catalog.
If a translation exists, use it; otherwise use ENGLISH.
If FORMAT-ARGS are provided, the result string is used as a format string."
  (let* ((catalog (when *application*
                    (get-message-catalog *application*)))
         (template (or (when catalog (gethash english catalog))
                       english)))
    (if format-args
        (apply #'format nil template format-args)
        template)))

;;; Tooling: extract translatable strings

(defun extract-messages-from-catalog (path)
  "Read a message catalog and return the list of English keys."
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (let ((data (read s nil nil)))
        (loop for (key value) on data by #'cddr
              when (stringp key) collect key)))))

(defun write-translation-template (messages output-path &key target-language)
  "Write a translation template file with all MESSAGES as keys
and empty string values. TARGET-LANGUAGE is a comment string."
  (with-open-file (s output-path :direction :output
                                  :if-exists :supersede
                                  :if-does-not-exist :create)
    (format s ";;; Message catalog~@[ for ~A~]~%" target-language)
    (format s ";;; Fill in translations for each English string.~%")
    (format s ";;; Format directives (~~A, ~~D, etc.) must match the English string.~%~%")
    (format s "(~%")
    (dolist (msg messages)
      (format s " ~S~% ~S~%~%" msg ""))
    (format s ")~%")))
