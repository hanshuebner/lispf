;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; field-binding.lisp
;;;;
;;;; Macros for ergonomic field access and screen handling:
;;;; with-field-values, make-field-values, aid-dispatch, with-screen,
;;;; with-transaction-screen.

(in-package #:lispf)

;;; AID keyword mapping

(defun aid-keyword-to-constant (keyword)
  "Map a keyword like :ENTER to the corresponding cl3270 AID constant symbol."
  (ecase keyword
    (:none   'cl3270:+aid-none+)
    (:enter  'cl3270:+aid-enter+)
    (:clear  'cl3270:+aid-clear+)
    (:pa1    'cl3270:+aid-pa1+)
    (:pa2    'cl3270:+aid-pa2+)
    (:pa3    'cl3270:+aid-pa3+)
    (:pf1    'cl3270:+aid-pf1+)
    (:pf2    'cl3270:+aid-pf2+)
    (:pf3    'cl3270:+aid-pf3+)
    (:pf4    'cl3270:+aid-pf4+)
    (:pf5    'cl3270:+aid-pf5+)
    (:pf6    'cl3270:+aid-pf6+)
    (:pf7    'cl3270:+aid-pf7+)
    (:pf8    'cl3270:+aid-pf8+)
    (:pf9    'cl3270:+aid-pf9+)
    (:pf10   'cl3270:+aid-pf10+)
    (:pf11   'cl3270:+aid-pf11+)
    (:pf12   'cl3270:+aid-pf12+)
    (:pf13   'cl3270:+aid-pf13+)
    (:pf14   'cl3270:+aid-pf14+)
    (:pf15   'cl3270:+aid-pf15+)
    (:pf16   'cl3270:+aid-pf16+)
    (:pf17   'cl3270:+aid-pf17+)
    (:pf18   'cl3270:+aid-pf18+)
    (:pf19   'cl3270:+aid-pf19+)
    (:pf20   'cl3270:+aid-pf20+)
    (:pf21   'cl3270:+aid-pf21+)
    (:pf22   'cl3270:+aid-pf22+)
    (:pf23   'cl3270:+aid-pf23+)
    (:pf24   'cl3270:+aid-pf24+)))

(defun aid-keyword-display-name (keyword)
  "Return the display name for an AID keyword, e.g. :PF3 -> \"PF3\", :ENTER -> \"Enter\"."
  (let ((name (symbol-name keyword)))
    (if (member keyword '(:enter :clear))
        (string-capitalize name)
        name)))

;;; Clause parsing

(defun parse-with-screen-clause (clause)
  "Parse a with-screen clause. Returns (key label . body).
LABEL is a string if the second element is a string literal, otherwise nil."
  (destructuring-bind (key &rest rest) clause
    (if (and rest (stringp (first rest)))
        (list* key (first rest) (rest rest))
        (list* key nil rest))))

(defun format-key-labels (parsed-clauses)
  "Format key labels from parsed clauses into a display string for line 24."
  (let ((labels (loop for (key label . nil) in parsed-clauses
                      when label
                        collect (format nil "~A ~A" (aid-keyword-display-name key) label))))
    (when labels
      (format nil "~{~A~^  ~}" labels))))

;;; Field value macros

(defmacro with-field-values ((response &rest field-names) &body body)
  "Bind FIELD-NAMES as setf-able accessors into RESPONSE's field values.
Each field name symbol is converted to a lowercase string key at macro-expansion time.

Example:
  (with-field-values (resp username password)
    (authenticate username password))"
  `(symbol-macrolet
       (,@(mapcar (lambda (field-name)
                    `(,field-name
                      (gethash ,(string-downcase (symbol-name field-name))
                               (cl3270:response-vals ,response))))
                  field-names))
     ,@body))

(defmacro make-field-values (&rest pairs)
  "Create a field-values hash table from name-value pairs.
Field name symbols are converted to lowercase strings at macro-expansion time.

Example:
  (make-field-values username \"john\" errormsg \"\")
  ;; => hash-table {\"username\" -> \"john\", \"errormsg\" -> \"\"}"
  (let ((ht (gensym "HT")))
    `(let ((,ht (cl3270:make-dict :test #'equal)))
       ,@(loop for (name value) on pairs by #'cddr
               collect `(setf (gethash ,(string-downcase (symbol-name name)) ,ht)
                              ,value))
       ,ht)))

;;; AID dispatch

(defmacro aid-dispatch (aid-expr &body clauses)
  "Dispatch on an AID value using keyword names.
Each clause is (keyword body...) where keyword is :ENTER, :PF3, :CLEAR, etc.

Example:
  (aid-dispatch (cl3270:response-aid resp)
    (:enter (do-something))
    (:pf3   (exit)))"
  `(cond
     ,@(mapcar (lambda (clause)
                 (destructuring-bind (key &body body) clause
                   `((cl3270:is-key ,aid-expr ,(aid-keyword-to-constant key))
                     ,@body)))
               clauses)))

;;; with-screen

(defmacro with-screen ((screen-name
                         &key conn devinfo codepage rules
                         exit-keys cursor response initial-values)
                        field-bindings
                        &body clauses)
  "High-level screen handling macro.

Looks up SCREEN-NAME from the registry, displays it with handle-screen,
binds field values, and dispatches on AID keys.

Framework automatically manages:
  Line 1  (row 0):  title (screen name, *application-name*, date/time)
  Line 23 (row 22): error messages from validation
  Line 24 (row 23): key labels from clause definitions

SCREEN-NAME is a bare symbol resolved to a .screen file via the registry.
CONN is the network connection (required).
DEVINFO is the device-info object (optional, nil for default 24x80).
CODEPAGE is the codepage to use (optional).
RULES is an application-provided rules hash table (merged with screen rules).
EXIT-KEYS is a list of AID keywords that bypass validation (e.g. (:pf3 :clear)).
  All other clause keys trigger validation.
CURSOR is (row col) for initial cursor position.
RESPONSE is a symbol to bind the raw cl3270:response object.
INITIAL-VALUES is a field-values hash table for initial field overrides.

FIELD-BINDINGS is a list of symbols bound as field accessors via with-field-values.

CLAUSES are (keyword [\"label\"] body...) forms dispatched by AID key.
  An optional string after the keyword becomes a label on line 24.

Example:
  (with-screen (welcome
                :conn conn :devinfo devinfo
                :codepage (cl3270:codepage devinfo)
                :exit-keys (:pf3)
                :cursor (17 21)
                :initial-values (make-field-values errormsg \"\"))
               (name message)
    (:pf3   \"Exit\"  (handle-exit))
    (:enter \"Submit\" (process name message)))"
  (let* ((parsed-clauses (mapcar #'parse-with-screen-clause clauses))
         (all-keys (mapcar #'first parsed-clauses))
         (exit-key-set exit-keys)
         (pf-key-set (set-difference all-keys exit-key-set))
         (pf-key-forms (mapcar #'aid-keyword-to-constant pf-key-set))
         (exit-key-forms (mapcar #'aid-keyword-to-constant exit-key-set))
         (key-label-string (format-key-labels parsed-clauses))
         (screen-var (gensym "SCREEN"))
         (screen-rules-var (gensym "SCREEN-RULES"))
         (merged-rules-var (gensym "MERGED-RULES"))
         (vals-var (gensym "VALS"))
         (resp-var (or response (gensym "RESP")))
         (err-var (gensym "ERR"))
         (cursor-row (if cursor (first cursor) 0))
         (cursor-col (if cursor (second cursor) 0))
         (dispatch-clauses (mapcar (lambda (pc)
                                     (destructuring-bind (key label &rest body) pc
                                       (declare (ignore label))
                                       `(,key ,@body)))
                                   parsed-clauses)))
    `(let* ((,screen-var (get-screen ',screen-name))
            (,screen-rules-var (get-screen-rules ',screen-name))
            (,merged-rules-var (merge-screen-rules ,screen-rules-var ,rules))
            (,vals-var (or ,initial-values (cl3270:make-dict :test #'equal))))
       ;; Framework: set title and key labels
       (setf (gethash "title" ,vals-var) (format-title-line ',screen-name))
       ,@(when key-label-string
           `((setf (gethash "keys" ,vals-var) ,key-label-string)))
       ,@(when field-bindings
           `((check-field-names ,screen-var
                                ,@(mapcar (lambda (f) `',f) field-bindings))))
       (multiple-value-bind (,resp-var ,err-var)
           (cl3270:handle-screen-alt ,screen-var
                                      ,merged-rules-var
                                      ,vals-var
                                      (vector ,@pf-key-forms)
                                      (vector ,@exit-key-forms)
                                      "errormsg"
                                      ,cursor-row ,cursor-col
                                      ,conn
                                      ,devinfo
                                      ,@(when codepage (list codepage)))
         (when ,err-var (error ,err-var))
         (with-field-values (,resp-var ,@field-bindings)
           (aid-dispatch (cl3270:response-aid ,resp-var)
             ,@dispatch-clauses))))))

;;; with-transaction-screen

(defmacro with-transaction-screen ((screen-name
                                     &key session conn devinfo
                                     codepage rules exit-keys
                                     cursor response initial-values
                                     self)
                                    field-bindings
                                    &body clauses)
  "Transaction-aware variant of with-screen.
Clauses should return (values session next-tx data error).
Provides a local RETRY function: (retry error-message) returns from the
form with (values session self error-message nil), causing the transaction
loop to re-invoke this screen with the error message as data.

SELF is a form evaluating to the current transaction function.
SESSION is the session variable name (used by RETRY).
All other parameters are as in with-screen.

Example:
  (with-transaction-screen (login
                             :self #'login-tx
                             :session s :conn conn :devinfo devinfo
                             :exit-keys (:pf3))
                            (username password)
    (:pf3   \"Exit\"  (values s nil nil nil))
    (:enter \"Submit\"
      (if (authenticate username password)
          (values s #'main-menu nil nil)
          (retry \"Invalid credentials\"))))"
  (let ((block-name (gensym "TX-SCREEN")))
    `(block ,block-name
       (flet ((retry (error-message)
                (return-from ,block-name
                  (values ,session ,self error-message nil))))
         (declare (ignorable #'retry))
         (with-screen (,screen-name
                       :conn ,conn :devinfo ,devinfo
                       :codepage ,codepage :rules ,rules
                       :exit-keys ,exit-keys :cursor ,cursor
                       :response ,response
                       :initial-values ,initial-values)
                      ,field-bindings
           ,@clauses)))))
