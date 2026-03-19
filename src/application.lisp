;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; application.lisp
;;;;
;;;; Application and session abstractions for lispf.
;;;; Provides define-application macro, session class, and main loop.

(in-package #:lispf)

;;; Application class

(defclass application ()
  ((name :initarg :name :accessor application-name)
   (entry-screen :initarg :entry-screen :accessor application-entry-screen)
   (screen-directory :initarg :screen-directory :accessor application-screen-directory)
   (package :initarg :package :accessor application-package)
   (session-class :initarg :session-class :accessor application-session-class
                  :initform 'session)))

;;; Session class

(defclass session ()
  ((application :initarg :application :reader session-application)
   (current-screen :accessor session-current-screen)
   (screen-stack :initform nil :accessor session-screen-stack)
   (context :initform (make-hash-table :test 'equal) :accessor session-context)
   (properties :initform (make-hash-table) :accessor session-properties)))

(defun session-property (session key &optional default)
  "Get a session property by key."
  (gethash key (session-properties session) default))

(defun (setf session-property) (value session key &optional default)
  "Set a session property by key."
  (declare (ignore default))
  (setf (gethash key (session-properties session)) value))

;;; Dynamic variables (bound by run-application)

(defvar *application* nil
  "The current application object.")

(defvar *session* nil
  "The current session object.")

(defvar *connection* nil
  "The current 3270 connection.")

(defvar *device-info* nil
  "The current device info.")

(defvar *current-field-values* nil
  "The session context hash table. Used by with-field-bindings in key handlers.
Field values from user input are automatically merged here (except transient fields).
Handlers can read and modify values; changes persist across screen transitions.")

;;; Screen name handling
;;;
;;; Screen names are symbols in the application's package. This avoids
;;; conflicts when multiple applications coexist in the same Lisp image.
;;; The registry uses lowercase strings; we intern into the app package
;;; for handle-key EQL dispatch.

(defun intern-screen-name (name-string package)
  "Intern a screen name string as a symbol in PACKAGE."
  (intern (string-upcase name-string) package))

;;; define-application macro

(defmacro define-application (name &body options)
  "Define a 3270 application.

Screen names are symbols in the current package (the package where
define-application is expanded). This means define-key-handler calls
must be in the same package.

Options:
  :entry-screen symbol   - first screen to display (required)
  :screen-directory path - directory containing .screen files (required)
  :session-class symbol  - custom session class (default: session)

Example:
  (define-application *my-app*
    :entry-screen welcome
    :screen-directory #p\"screens/\")"
  (let ((entry-screen (getf options :entry-screen))
        (screen-directory (getf options :screen-directory))
        (session-class (getf options :session-class ''session)))
    (unless entry-screen
      (error "define-application requires :entry-screen"))
    (unless screen-directory
      (error "define-application requires :screen-directory"))
    ;; Intern entry-screen in the current package at expansion time
    (let ((entry-sym (intern (string-upcase (string entry-screen)) *package*)))
      `(defvar ,name
         (make-instance 'application
                        :name ,(string-downcase (string name))
                        :entry-screen ',entry-sym
                        :screen-directory ,screen-directory
                        :package (find-package ,(package-name *package*))
                        :session-class ,session-class)))))

;;; AID constant to keyword mapping (inverse of aid-keyword-to-constant)

(defun aid-to-keyword (aid-value)
  "Convert a numeric AID value to a keyword like :ENTER, :PF3, etc."
  (cond
    ((cl3270:is-key aid-value cl3270:+aid-enter+) :enter)
    ((cl3270:is-key aid-value cl3270:+aid-clear+) :clear)
    ((cl3270:is-key aid-value cl3270:+aid-pa1+) :pa1)
    ((cl3270:is-key aid-value cl3270:+aid-pa2+) :pa2)
    ((cl3270:is-key aid-value cl3270:+aid-pa3+) :pa3)
    ((cl3270:is-key aid-value cl3270:+aid-pf1+) :pf1)
    ((cl3270:is-key aid-value cl3270:+aid-pf2+) :pf2)
    ((cl3270:is-key aid-value cl3270:+aid-pf3+) :pf3)
    ((cl3270:is-key aid-value cl3270:+aid-pf4+) :pf4)
    ((cl3270:is-key aid-value cl3270:+aid-pf5+) :pf5)
    ((cl3270:is-key aid-value cl3270:+aid-pf6+) :pf6)
    ((cl3270:is-key aid-value cl3270:+aid-pf7+) :pf7)
    ((cl3270:is-key aid-value cl3270:+aid-pf8+) :pf8)
    ((cl3270:is-key aid-value cl3270:+aid-pf9+) :pf9)
    ((cl3270:is-key aid-value cl3270:+aid-pf10+) :pf10)
    ((cl3270:is-key aid-value cl3270:+aid-pf11+) :pf11)
    ((cl3270:is-key aid-value cl3270:+aid-pf12+) :pf12)
    ((cl3270:is-key aid-value cl3270:+aid-pf13+) :pf13)
    ((cl3270:is-key aid-value cl3270:+aid-pf14+) :pf14)
    ((cl3270:is-key aid-value cl3270:+aid-pf15+) :pf15)
    ((cl3270:is-key aid-value cl3270:+aid-pf16+) :pf16)
    ((cl3270:is-key aid-value cl3270:+aid-pf17+) :pf17)
    ((cl3270:is-key aid-value cl3270:+aid-pf18+) :pf18)
    ((cl3270:is-key aid-value cl3270:+aid-pf19+) :pf19)
    ((cl3270:is-key aid-value cl3270:+aid-pf20+) :pf20)
    ((cl3270:is-key aid-value cl3270:+aid-pf21+) :pf21)
    ((cl3270:is-key aid-value cl3270:+aid-pf22+) :pf22)
    ((cl3270:is-key aid-value cl3270:+aid-pf23+) :pf23)
    ((cl3270:is-key aid-value cl3270:+aid-pf24+) :pf24)
    (t :none)))

;;; Key spec processing

(defun build-key-vectors (key-specs)
  "Build pf-keys and exit-keys vectors from key specs.
Returns (values pf-keys-vector exit-keys-vector).
Keys with :back t become exit-keys; all others become pf-keys."
  (let (pf-keys exit-keys)
    (dolist (spec key-specs)
      (destructuring-bind (aid-keyword label &key back goto) spec
        (declare (ignore label goto))
        (let ((constant (symbol-value (aid-keyword-to-constant aid-keyword))))
          (if back
              (push constant exit-keys)
              (push constant pf-keys)))))
    (values (coerce (nreverse pf-keys) 'vector)
            (coerce (nreverse exit-keys) 'vector))))

(defun format-key-labels-from-specs (key-specs)
  "Build row 23 key label string from key specs. Keys with empty labels are omitted."
  (let ((labels (loop for (aid-keyword label . rest) in key-specs
                      when (and label (string/= label ""))
                      collect (format nil "~A ~A"
                                      (aid-keyword-display-name aid-keyword) label))))
    (when labels
      (format nil "~{~A~^  ~}" labels))))

(defun find-key-spec (key-specs aid-keyword)
  "Find the key spec for AID-KEYWORD in KEY-SPECS. Returns the spec or nil."
  (find aid-keyword key-specs :key #'first))

;;; Framework field names (excluded from context merge)

(defparameter *framework-fields* '("title" "errormsg" "keys")
  "Field names managed by the framework, excluded from context.")

;;; Screen key handler validation

(defun check-screen-key-handlers (screen-sym key-specs)
  "Check that all key-specs without :back or :goto have a matching handle-key method.
Signals a warning for any key that would fall through to the default handler."
  (let ((default-method (find-method #'handle-key '()
                                     (list (find-class t) (find-class t)))))
    (dolist (spec key-specs)
      (destructuring-bind (aid-keyword label &key back goto) spec
        (declare (ignore label))
        (unless (or back goto)
          (let* ((methods (compute-applicable-methods
                           #'handle-key (list screen-sym aid-keyword)))
                 (has-specific (find-if (lambda (m) (not (eq m default-method)))
                                        methods)))
            (unless has-specific
              (warn "Screen ~S: no handler for ~S (define with define-key-handler)"
                    screen-sym aid-keyword))))))))

(defvar *validated-screens* (make-hash-table :test 'eq)
  "Screens whose key handlers have been validated.")

;;; Server and connection handling

(defun handle-connection (application socket)
  "Handle a single 3270 connection for APPLICATION on SOCKET."
  (declare (type usocket:stream-usocket socket))
  (handler-case
      (multiple-value-bind (devinfo err)
          (cl3270:negotiate-telnet socket)
        (when err
          (format *error-output* "~&;;; ~A: negotiation error: ~A~%"
                  (application-name application) err)
          (return-from handle-connection))
        (unwind-protect
             (run-application application socket devinfo)
          (cl3270:unnegotiate-telnet socket 1)))
    (error (e)
      (format *error-output* "~&;;; ~A: connection error: ~A~%"
              (application-name application) e))))

(defun start-application (application &key (port 3270) (host "127.0.0.1"))
  "Start APPLICATION, listening for 3270 connections on HOST:PORT.
Serves multiple users concurrently using threads."
  (setf *application-name* (application-name application))
  (format t "~&;;; ~A: listening on ~A:~D~%" (application-name application) host port)
  (usocket:with-socket-listener (listener host port
                                          :element-type '(unsigned-byte 8)
                                          :reuse-address t)
    (loop
      (let ((c (usocket:socket-accept listener)))
        (bt:make-thread
         (lambda ()
           (unwind-protect
                (handle-connection application c)
             (ignore-errors (usocket:socket-close c))))
         :name (format nil "~A-~A"
                        (application-name application)
                        (usocket:get-peer-address c)))))))

;;; Dynamic key label management

(defvar *current-screen-keys* nil
  "Current key display specs for the screen being prepared.
Bound by run-application for each iteration. Modified by show-key/hide-key.")

(defun show-key (aid-keyword label)
  "Make AID-KEYWORD visible in the key labels with LABEL.
Call from within a define-screen-update body."
  (let ((entry (assoc aid-keyword *current-screen-keys*)))
    (if entry
        (setf (second entry) label)
        (push (list aid-keyword label) *current-screen-keys*))))

(defun hide-key (aid-keyword)
  "Hide AID-KEYWORD from the key labels (it remains accepted).
Call from within a define-screen-update body."
  (let ((entry (assoc aid-keyword *current-screen-keys*)))
    (when entry
      (setf (second entry) ""))))

;;; Main application loop

(defun run-application (application conn devinfo)
  "Run an application's main loop for a single connection.
Binds dynamic variables, creates a session, and loops through screens.
Field values are automatically persisted in the session context across
screen transitions. Fields marked :transient in .screen files are excluded."
  (let* ((*application* application)
         (*connection* conn)
         (*device-info* devinfo)
         (*session* (make-instance (application-session-class application)
                                   :application application))
         (*application-name* (application-name application))
         (app-package (application-package application)))
    ;; Register the screen directory
    (register-screen-directory (application-screen-directory application))
    ;; Set initial screen
    (setf (session-current-screen *session*)
          (application-entry-screen application))
    ;; Main loop
    (loop
      (let* ((screen-sym (session-current-screen *session*))
             (name-string (screen-name-string screen-sym))
             (screen (get-screen screen-sym))
             (screen-rules (get-screen-rules screen-sym))
             (key-specs (get-screen-keys screen-sym))
             (transient-fields (get-screen-transient-fields screen-sym))
             (repeat-groups (get-screen-repeat-fields screen-sym))
             (context (session-context *session*))
             (dispatch-sym (intern-screen-name name-string app-package))
             (field-values (cl3270:make-dict :test #'equal)))
        ;; Validate key handlers (once per screen)
        (unless (gethash dispatch-sym *validated-screens*)
          (setf (gethash dispatch-sym *validated-screens*) t)
          (check-screen-key-handlers dispatch-sym key-specs))
        ;; Bind context for prepare-screen
        (setf *current-field-values* context)
        ;; Call prepare-screen with dynamic key management
        (let ((*current-screen-keys* (mapcar (lambda (spec)
                                               (list (first spec) (second spec)))
                                             key-specs)))
          (prepare-screen dispatch-sym)
          ;; Copy context into field values for display
          (maphash (lambda (k v) (setf (gethash k field-values) v)) context)
          ;; Expand repeat field values for display
          (when repeat-groups
            (split-repeat-field-value
             repeat-groups
             (lambda (base-name) (repeat-field-length screen base-name))
             field-values))
          ;; Set framework fields
          (setf (gethash "title" field-values) (format-title-line screen-sym))
          (setf (gethash "errormsg" field-values)
                (or (gethash "errormsg" field-values) ""))
          ;; Render key labels from *current-screen-keys*
          (let ((key-labels (format-key-labels-from-specs *current-screen-keys*)))
            (when key-labels
              (setf (gethash "keys" field-values) key-labels)))
          ;; Auto-position cursor on first writable field
          (let* ((first-write (find-if (lambda (f) (cl3270::field-write f))
                                       (cl3270:screen-fields screen)))
                 (cursor-row (if first-write (cl3270:field-row first-write) 0))
                 (cursor-col (if first-write (1+ (cl3270:field-col first-write)) 0)))
            ;; Build key vectors from specs
            (multiple-value-bind (pf-keys exit-keys)
                (build-key-vectors key-specs)
              ;; Display screen and get response
              (multiple-value-bind (response err)
                  (cl3270:handle-screen-alt screen
                                            screen-rules
                                            field-values
                                            pf-keys
                                            exit-keys
                                            "errormsg"
                                            cursor-row cursor-col
                                            conn
                                            nil)
                (when err (error err))
                ;; Merge response values into context (skip transient + framework)
                (let ((resp-vals (cl3270:response-vals response)))
                  (when resp-vals
                    (maphash (lambda (k v)
                               (unless (or (member k *framework-fields* :test #'string=)
                                           (member k transient-fields :test #'string=))
                                 (setf (gethash k context) v)))
                             resp-vals)))
                ;; Join repeat field values back into single context values
                (when repeat-groups
                  (join-repeat-field-values repeat-groups context))
                ;; Clear error message for next iteration
                (remhash "errormsg" context)
                ;; Determine AID keyword and dispatch
                (let* ((aid-kw (aid-to-keyword (cl3270:response-aid response)))
                       (key-spec (find-key-spec key-specs aid-kw))
                       (result
                         (restart-case
                             (handler-bind
                                 ((application-error
                                    (lambda (c)
                                      (setf (gethash "errormsg" context)
                                            (application-error-message c))
                                      (invoke-restart 'redisplay)))
                                  (error
                                    (lambda (c)
                                      (let ((id (generate-incident-id)))
                                        (log-incident id c)
                                        (setf (gethash "errormsg" context)
                                              (format nil "Internal error. Incident: ~A" id))
                                        (invoke-restart 'redisplay)))))
                               (cond
                                 ((and key-spec (getf (cddr key-spec) :back))
                                  :back)
                                 ((and key-spec (getf (cddr key-spec) :goto))
                                  (intern-screen-name
                                   (string (getf (cddr key-spec) :goto))
                                   app-package))
                                 (t (handle-key dispatch-sym aid-kw))))
                           (redisplay () :stay))))
                  ;; Interpret result
                  (cond
                    ((eq result :logoff)
                     (return))
                    ((eq result :stay)
                     nil)
                    ((eq result :back)
                     (let ((prev (pop (session-screen-stack *session*))))
                       (if prev
                           (setf (session-current-screen *session*) prev)
                           (return))))
                    ((symbolp result)
                     (push screen-sym (session-screen-stack *session*))
                     (setf (session-current-screen *session*) result))
                    (t
                     (warn "Unexpected handle-key return value ~S on screen ~S, treating as :stay"
                           result dispatch-sym))))))))))))

