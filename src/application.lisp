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
                  :initform 'session)
   (screens :initform (make-hash-table :test 'equal) :accessor application-screens
            :documentation "Screen cache: name-string -> screen-info struct.")
   (screen-directories :initform '() :accessor application-screen-directories
                       :documentation "List of directories to search for .screen files.")
   (validated-screens :initform (make-hash-table :test 'eq)
                      :accessor application-validated-screens
                      :documentation "Screens whose key handlers have been validated.")
   (sessions :initform '() :accessor application-sessions
             :documentation "List of active sessions (one per connection).")
   (sessions-lock :initform (bt:make-lock "sessions") :reader application-sessions-lock)))

(defmethod initialize-instance :after ((app application) &key)
  (let ((dir (application-screen-directory app)))
    (when dir
      (pushnew (truename dir) (application-screen-directories app) :test #'equal))))

;;; Session class

(defclass session ()
  ((application :initarg :application :reader session-application)
   (current-screen :accessor session-current-screen)
   (screen-stack :initform nil :accessor session-screen-stack)
   (context :initform (make-hash-table :test 'equal) :accessor session-context)
   (properties :initform (make-hash-table) :accessor session-properties)
   (list-state :initform (make-hash-table) :accessor session-list-state
               :documentation "Framework-managed per-screen list state.
Keys are screen symbols, values are plists of (:offset N :data-count N :cursor-row N).")
   (write-lock :initform (bt:make-lock "conn-write") :reader session-write-lock)
   (indicators :initform (make-hash-table :test 'equal) :reader session-indicators)
   (indicators-dirty :initform nil :accessor session-indicators-dirty)
   (update-lock :initform (bt:make-lock "update-wake") :reader session-update-lock)
   (update-cond :initform (bt:make-condition-variable :name "update-wake")
                :reader session-update-cond)))

(defun session-property (session key &optional default)
  "Get a session property by key."
  (gethash key (session-properties session) default))

(defun (setf session-property) (value session key &optional default)
  "Set a session property by key."
  (declare (ignore default))
  (setf (gethash key (session-properties session)) value))

;;; Indicator API

(defun set-indicator (name text)
  "Set or update a status line indicator. TEXT=NIL clears it.
Indicators appear as (TEXT) in the title line and persist across screen transitions."
  (if text
      (setf (gethash name (session-indicators *session*)) text)
      (remhash name (session-indicators *session*)))
  (bt:with-lock-held ((session-update-lock *session*))
    (setf (session-indicators-dirty *session*) t)
    (bt:condition-notify (session-update-cond *session*))))

(defun clear-indicator (name)
  "Clear a status line indicator."
  (set-indicator name nil))

(defun broadcast (function)
  "Call FUNCTION once for each active session of the current application.
*SESSION* is bound to each session in turn. Use this for cross-session
updates like broadcasting indicators, chat messages, or system notifications."
  (bt:with-lock-held ((application-sessions-lock *application*))
    (dolist (session (application-sessions *application*))
      (let ((*session* session))
        (funcall function)))))

(defun broadcast-indicator (name text)
  "Set an indicator on ALL active sessions of the current application.
Each session's update thread is woken to push the change immediately."
  (broadcast (lambda () (set-indicator name text))))

(defun session-indicator-texts ()
  "Return list of indicator text strings from the current session, or NIL."
  (when *session*
    (let ((texts '()))
      (maphash (lambda (name text)
                 (declare (ignore name))
                 (when text (push text texts)))
               (session-indicators *session*))
      (nreverse texts))))

;;; Dynamic variables (bound by run-application)

(defvar *application* nil
  "The current application object.")

(defvar *session* nil
  "The current session object.")

(defvar *connection* nil
  "The current 3270 connection.")

(defvar *device-info* nil
  "The current device info.")

(defvar *cursor-row* 0
  "The cursor row position from the last 3270 response.
Available in key handler bodies.")

(defvar *cursor-col* 0
  "The cursor column position from the last 3270 response.
Available in key handler bodies.")

(defvar *current-field-values* nil
  "The session context hash table. Used by with-field-bindings in key handlers.
Field values from user input are automatically merged here (except transient fields).
Handlers can read and modify values; changes persist across screen transitions.")

;;; Framework-managed per-screen list state

(defun list-state (session screen-name)
  "Get the list state plist for SCREEN-NAME."
  (gethash screen-name (session-list-state session)))

(defun (setf list-state-value) (value session screen-name key)
  "Set KEY in the list state plist for SCREEN-NAME."
  (let ((state (gethash screen-name (session-list-state session))))
    (setf (getf state key) value)
    (setf (gethash screen-name (session-list-state session)) state)
    value))

(defun list-offset (session screen-name)
  "Get the list offset for SCREEN-NAME."
  (or (getf (list-state session screen-name) :offset) 0))

(defun (setf list-offset) (value session screen-name)
  "Set the list offset for SCREEN-NAME."
  (setf (list-state-value session screen-name :offset) value))

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

(defun has-list-data-getter-p (screen-sym)
  "Return T if SCREEN-SYM has a non-default get-list-data method."
  (let ((default-method (find-method #'get-list-data '()
                                     (list (find-class t) (find-class t) (find-class t)))))
    (find-if (lambda (m) (not (eq m default-method)))
             (compute-applicable-methods #'get-list-data (list screen-sym 0 0)))))

(defun check-screen-key-handlers (screen-sym key-specs)
  "Check that all key-specs without :back or :goto have a matching handle-key method.
Signals a warning for any key that would fall through to the default handler."
  (let ((default-method (find-method #'handle-key '()
                                     (list (find-class t) (find-class t))))
        (list-screen-p (has-list-data-getter-p screen-sym)))
    (dolist (spec key-specs)
      (destructuring-bind (aid-keyword label &key back goto) spec
        (declare (ignore label))
        (unless (or back goto
                    (and list-screen-p (member aid-keyword '(:pf7 :pf8))))
          (let* ((methods (compute-applicable-methods
                           #'handle-key (list screen-sym aid-keyword)))
                 (has-specific (find-if (lambda (m) (not (eq m default-method)))
                                        methods)))
            (unless has-specific
              (warn "Screen ~S: no handler for ~S (define with define-key-handler)"
                    screen-sym aid-keyword))))))))


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

(defun start-application (application &key (port 3270) (host "127.0.0.1")
                                          listener-callback)
  "Start APPLICATION, listening for 3270 connections on HOST:PORT.
Serves multiple users concurrently using threads.
When LISTENER-CALLBACK is provided, it is called with the listener socket
after binding but before the accept loop. Useful for test harnesses."
  (format t "~&;;; ~A: listening on ~A:~D~%" (application-name application) host port)
  (usocket:with-socket-listener (listener host port
                                          :element-type '(unsigned-byte 8)
                                          :reuse-address t)
    (when listener-callback
      (funcall listener-callback listener))
    (loop
      (let ((c (usocket:socket-accept listener))
            (out *standard-output*)
            (err *error-output*))
        (bt:make-thread
         (lambda ()
           (let ((*standard-output* out)
                 (*error-output* err))
             (unwind-protect
                  (handle-connection application c)
               (ignore-errors (usocket:socket-close c)))))
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

;;; Cursor position override

(defvar *next-cursor-row* nil
  "When non-nil, overrides automatic cursor positioning for the next screen display.
Set via SET-CURSOR from within a define-screen-update body.")

(defvar *next-cursor-col* nil
  "When non-nil, overrides automatic cursor column for the next screen display.
Set via SET-CURSOR from within a define-screen-update body.")

(defun set-cursor (row col)
  "Override cursor position for the next screen display.
ROW and COL are 0-based screen coordinates (including title row).
Call from within a define-screen-update body."
  (setf *next-cursor-row* row
        *next-cursor-col* col))

;;; Update context and background thread

(defstruct update-context
  "State for the background update thread."
  (running nil :type boolean)
  (thread nil)
  (screen-sym nil)
  (last-minute -1 :type fixnum))

(defun merge-field-values (original current)
  "Merge two field-value hash tables. CURRENT values take precedence over ORIGINAL."
  (let ((result (make-hash-table :test (hash-table-test original))))
    (maphash (lambda (k v) (setf (gethash k result) v)) original)
    (when current
      (maphash (lambda (k v) (setf (gethash k result) v)) current))
    result))

(defun validate-fields (rules my-vals orig-values error-field)
  "Check field validation rules. Returns T if all pass, NIL if failed.
On failure, sets the error message in MY-VALS under ERROR-FIELD."
  (or (null rules)
      (loop for field being the hash-key of rules using (hash-value fr)
          for value = (nth-value 0 (gethash field my-vals))
          for present = (nth-value 1 (gethash field my-vals))
          when present do
            (cond
              ((and (cl3270:field-rules-must-change fr)
                    (string= value (gethash field orig-values)))
               (setf (gethash error-field my-vals)
                     (cl3270:field-rules-error-text fr))
               (return nil))
              ((and (cl3270:field-rules-validator fr)
                    (not (funcall (cl3270:field-rules-validator fr) value)))
               (setf (gethash error-field my-vals)
                     (format nil "Value for ~S is not valid" field))
               (return nil)))
          finally (return t))))

(defun send-title-overlay (ctx)
  "Send a title-only overlay to update the clock and indicators.
Skips the send if the update thread has been asked to stop, to avoid
unlocking the 3270 keyboard after the main thread received a response."
  (unless (update-context-running ctx)
    (return-from send-title-overlay))
  (let* ((title (format-title-line (update-context-screen-sym ctx)
                                   (session-indicator-texts)))
         (screen (cl3270:make-screen "title-overlay"
                   (cl3270:make-field :row 0 :col 0 :name "title")))
         (vals (cl3270:make-dict :test #'equal)))
    (setf (gethash "title" vals) title)
    (bt:with-lock-held ((session-write-lock *session*))
      ;; Double-check running inside the lock -- stop-updates acquires
      ;; this lock after setting running=nil to create a fence.
      (when (update-context-running ctx)
        (cl3270:show-screen-opts screen vals *connection*
          (cl3270:make-screen-opts :no-clear t :no-response t))))))

(defun send-dynamic-area-overlays (ctx)
  "Call dynamic area updaters for the current screen and send overlays."
  (unless (update-context-running ctx)
    (return-from send-dynamic-area-overlays))
  (let* ((screen-sym (update-context-screen-sym ctx))
         (areas (get-screen-dynamic-areas screen-sym))
         (app-package (application-package *application*)))
    (dolist (area areas)
      (unless (update-context-running ctx) (return))
      (let* ((area-sym (intern (string-upcase (dynamic-area-name area)) app-package))
             (content (update-dynamic-area screen-sym area-sym)))
        (when content
          (let* ((from-row (1+ (dynamic-area-from-row area)))
                 (from-col (dynamic-area-from-col area))
                 (to-row (1+ (dynamic-area-to-row area)))
                 (content-width (- (dynamic-area-to-col area) from-col))
                 (fields '())
                 (vals (cl3270:make-dict :test #'equal)))
            (loop for row from from-row to to-row
                  for i from 0
                  for name = (format nil "dyn-~A-~D" (dynamic-area-name area) i)
                  do (push (cl3270:make-field :row row :col from-col :name name)
                           fields)
                     (let* ((line (if (< i (length content)) (nth i content) ""))
                            (padded (if (> (length line) content-width)
                                        (subseq line 0 content-width)
                                        (concatenate 'string line
                                                     (make-string
                                                      (max 0 (- content-width (length line)))
                                                      :initial-element #\Space)))))
                       (setf (gethash name vals) padded)))
            (let ((screen (apply #'cl3270:make-screen "dynamic-overlay"
                                 (nreverse fields))))
              (bt:with-lock-held ((session-write-lock *session*))
                (when (update-context-running ctx)
                  (cl3270:show-screen-opts screen vals *connection*
                    (cl3270:make-screen-opts :no-clear t :no-response t)))))))))))

(defun update-thread-fn (ctx)
  "Main function for the background update thread.
Sleeps first, then checks for changes. This avoids an immediate overlay
send on startup which would race with the main thread's response processing."
  (handler-case
      (loop while (update-context-running ctx)
            do ;; Wait for signal or timeout (sleep first to avoid startup race)
               (bt:with-lock-held ((session-update-lock *session*))
                 (bt:condition-wait (session-update-cond *session*)
                                    (session-update-lock *session*)
                                    :timeout 1))
               (unless (update-context-running ctx) (return))
               (let ((needs-title-update nil))
                 ;; Check if minute changed
                 (let ((current-minute
                         (nth-value 1 (decode-universal-time (get-universal-time)))))
                   (when (/= current-minute (update-context-last-minute ctx))
                     (setf (update-context-last-minute ctx) current-minute
                           needs-title-update t)))
                 ;; Check if indicators dirty
                 (bt:with-lock-held ((session-update-lock *session*))
                   (when (session-indicators-dirty *session*)
                     (setf (session-indicators-dirty *session*) nil
                           needs-title-update t)))
                 ;; Send title overlay if needed
                 (when needs-title-update
                   (send-title-overlay ctx))
                 ;; Send dynamic area overlays
                 (send-dynamic-area-overlays ctx)))
    (error (e)
      (format *error-output* "~&;;; Update thread error: ~A~%" e))))

(defun start-updates (ctx)
  "Start the background update thread."
  (setf (update-context-running ctx) t)
  (let ((session *session*)
        (connection *connection*)
        (device-info *device-info*)
        (application *application*)
        (field-values *current-field-values*))
    (setf (update-context-thread ctx)
          (bt:make-thread
           (lambda ()
             (let ((*session* session)
                   (*connection* connection)
                   (*device-info* device-info)
                   (*application* application)
                   (*current-field-values* field-values))
               (update-thread-fn ctx)))
           :name "lispf-update"))))

(defun stop-updates (ctx)
  "Stop the background update thread and wait for it to exit.
Acquires the write-lock after setting running=nil to ensure no in-flight
overlay send completes after this function returns (which would prematurely
unlock the 3270 keyboard)."
  (setf (update-context-running ctx) nil)
  ;; Fence: acquire and release the write-lock to ensure any in-flight
  ;; overlay send (which holds this lock) completes, and the thread sees
  ;; running=nil on its next double-check inside the lock.
  (bt:with-lock-held ((session-write-lock *session*)) nil)
  ;; Wake the thread so it can exit
  (bt:with-lock-held ((session-update-lock *session*))
    (bt:condition-notify (session-update-cond *session*)))
  (when (update-context-thread ctx)
    (bt:join-thread (update-context-thread ctx))
    (setf (update-context-thread ctx) nil)))

;;; display-and-read: validation loop with background update support

(defun display-and-read (screen rules vals pf-keys exit-keys error-field
                          cursor-row cursor-col conn
                          &key screen-sym devinfo codepage)
  "Display a screen and read response with field validation.
Reimplements cl3270:handle-screen-alt's validation loop using only exported
cl3270 symbols, adding background update thread support via post-send-callback."
  (let ((orig-values (cl3270:make-dict :test #'equal))
        (my-vals (cl3270:make-dict :test #'equal)))
    ;; Save original field values for must-change checks
    (dolist (f (cl3270:screen-fields screen))
      (when (string/= "" (cl3270:field-name f))
        (setf (gethash (cl3270:field-name f) orig-values) (cl3270:field-content f))))
    (when vals
      (maphash (lambda (k v) (setf (gethash k my-vals) v)) vals))
    ;; Validation loop
    (loop
      ;; Reset fields with :reset flag
      (when rules
        (maphash (lambda (field rule)
                   (when (cl3270:field-rules-reset rule)
                     (if vals
                         (multiple-value-bind (v foundp) (gethash field vals)
                           (if foundp
                               (setf (gethash field my-vals) v)
                               (remhash field my-vals)))
                         (remhash field my-vals))))
                 rules))
      ;; Show screen with update thread
      (let ((update-ctx (when screen-sym
                          (make-update-context
                           :screen-sym screen-sym
                           :last-minute (nth-value 1
                                          (decode-universal-time
                                           (get-universal-time)))))))
        (multiple-value-bind (resp err)
            (cl3270:show-screen-opts screen my-vals conn
              (cl3270:make-screen-opts
               :cursor-row cursor-row :cursor-col cursor-col
               :altscreen devinfo :codepage codepage
               :post-send-callback (when update-ctx
                                     (lambda (data)
                                       (declare (ignore data))
                                       (start-updates update-ctx)
                                       nil))))
          (when update-ctx (stop-updates update-ctx))
          (when err (return (values resp err)))
          (cond
            ;; Exit key - return without validation
            ((cl3270:aid-in-set (cl3270:response-aid resp) exit-keys)
             (return (values resp nil)))
            ;; Unexpected key - show error, loop
            ((not (cl3270:aid-in-set (cl3270:response-aid resp) pf-keys))
             (unless (or (cl3270:is-clear-key (cl3270:response-aid resp))
                         (cl3270:is-key (cl3270:response-aid resp) cl3270:+aid-pa1+)
                         (cl3270:is-key (cl3270:response-aid resp) cl3270:+aid-pa2+)
                         (cl3270:is-key (cl3270:response-aid resp) cl3270:+aid-pa3+))
               (setf my-vals (merge-field-values my-vals (cl3270:response-vals resp))))
             (setf (gethash error-field my-vals)
                   (format nil "~A: unknown key"
                           (cl3270:aid-to-string (cl3270:response-aid resp)))))
            ;; Clear/PA in expected set - return immediately
            ((or (cl3270:is-clear-key (cl3270:response-aid resp))
                 (cl3270:is-key (cl3270:response-aid resp) cl3270:+aid-pa1+)
                 (cl3270:is-key (cl3270:response-aid resp) cl3270:+aid-pa2+)
                 (cl3270:is-key (cl3270:response-aid resp) cl3270:+aid-pa3+))
             (return (values resp nil)))
            ;; Normal key - merge values and validate
            (t
             (setq my-vals (merge-field-values my-vals (cl3270:response-vals resp)))
             (remhash error-field my-vals)
             (when (validate-fields rules my-vals orig-values error-field)
               (return (values resp nil))))))))))

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
         (app-package (application-package application)))
    ;; Register session
    (bt:with-lock-held ((application-sessions-lock application))
      (push *session* (application-sessions application)))
    (unwind-protect
     (progn
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
        (unless (gethash dispatch-sym (application-validated-screens *application*))
          (setf (gethash dispatch-sym (application-validated-screens *application*)) t)
          (check-screen-key-handlers dispatch-sym key-specs))
        ;; Bind context for prepare-screen
        (setf *current-field-values* context)
        ;; Call prepare-screen with dynamic key management
        (let ((*current-screen-keys* (mapcar (lambda (spec)
                                               (list (first spec) (second spec)))
                                             key-specs))
              (*next-cursor-row* nil)
              (*next-cursor-col* nil))
          (prepare-screen dispatch-sym)
          ;; Copy context into field values for display
          (maphash (lambda (k v) (setf (gethash k field-values) v)) context)
          ;; Distribute list data or expand repeat field values
          (let ((list-data-count nil)
                (list-data-total nil)
                (has-list-data nil))
            (when repeat-groups
              (multiple-value-bind (count total)
                  (distribute-list-data dispatch-sym repeat-groups
                                        context field-values)
                (when count
                  (setf list-data-count count
                        list-data-total total
                        has-list-data t)
                  ;; Auto-manage PF7/PF8 visibility
                  (let* ((page-size (reduce #'max repeat-groups
                                            :key #'second :initial-value 0))
                         (offset (list-offset *session* dispatch-sym)))
                    (when (> offset 0)
                      (show-key :pf7 "Prev"))
                    (when (< (+ offset page-size) (or total 0))
                      (show-key :pf8 "Next"))))))
            (when (and repeat-groups (not has-list-data))
              (split-repeat-field-value
               repeat-groups
               (lambda (base-name) (repeat-field-length screen base-name))
               field-values))
            ;; Set framework fields
            (setf (gethash "title" field-values)
                  (format-title-line screen-sym (session-indicator-texts)))
            (setf (gethash "errormsg" field-values)
                  (or (gethash "errormsg" field-values) ""))
            ;; Render key labels from *current-screen-keys*
            (let ((key-labels (format-key-labels-from-specs *current-screen-keys*)))
              (when key-labels
                (setf (gethash "keys" field-values) key-labels)))
            ;; Filter screen to hide empty repeat rows
            (let* ((display-screen (if has-list-data
                                       (filter-screen-fields screen repeat-groups
                                                             list-data-count)
                                       screen))
                   ;; Cursor: override > saved list cursor (clamped) > first writable field
                   (saved-cursor
                     (when (and has-list-data (not *next-cursor-row*))
                       (let ((raw (getf (list-state *session* dispatch-sym)
                                        :cursor-row)))
                         (when raw
                           (let* ((first-row (list-data-row screen repeat-groups))
                                  (max-row (+ first-row (1- list-data-count))))
                             (min raw max-row))))))
                   (first-write (unless (or *next-cursor-row* saved-cursor)
                                  (find-if (lambda (f) (cl3270::field-write f))
                                           (cl3270:screen-fields display-screen))))
                   (cursor-row (or *next-cursor-row* saved-cursor
                                   (if first-write (cl3270:field-row first-write) 0)))
                   (cursor-col (or *next-cursor-col*
                                   (when saved-cursor
                                     (getf (list-state *session* dispatch-sym)
                                           :cursor-col))
                                   (if first-write (1+ (cl3270:field-col first-write)) 0))))
              ;; Build key vectors from specs
              (multiple-value-bind (pf-keys exit-keys)
                  (build-key-vectors key-specs)
                ;; Display screen and get response
                (multiple-value-bind (response err)
                    (display-and-read display-screen
                                      screen-rules
                                      field-values
                                      pf-keys
                                      exit-keys
                                      "errormsg"
                                      cursor-row cursor-col
                                      conn
                                      :screen-sym dispatch-sym)
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
                  ;; (skip for list-data screens -- framework manages those fields)
                  (when (and repeat-groups (not has-list-data))
                    (join-repeat-field-values repeat-groups context))
                  ;; Clear error message for next iteration
                  (remhash "errormsg" context)
                  ;; Expose cursor position to key handlers
                  (setf *cursor-row* (cl3270:response-row response)
                        *cursor-col* (cl3270:response-col response))
                  ;; Auto-save cursor position for list screens
                  (when has-list-data
                    (setf (list-state-value *session* dispatch-sym :cursor-row)
                          *cursor-row*)
                    (setf (list-state-value *session* dispatch-sym :cursor-col)
                          *cursor-col*))
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
                                   ;; Auto-handle PF7/PF8 for list-data screens
                                   ((and has-list-data (eq aid-kw :pf7))
                                    (let* ((page-size (reduce #'max repeat-groups
                                                              :key #'second
                                                              :initial-value 0))
                                           (offset (list-offset *session* dispatch-sym)))
                                      (setf (list-offset *session* dispatch-sym)
                                            (max 0 (* page-size (1- (floor offset page-size))))))
                                    :stay)
                                   ((and has-list-data (eq aid-kw :pf8))
                                    (let* ((page-size (reduce #'max repeat-groups
                                                              :key #'second
                                                              :initial-value 0))
                                           (total (or list-data-total 0))
                                           (next (* page-size (1+ (floor (list-offset *session* dispatch-sym)
                                                                         page-size)))))
                                      (when (< next total)
                                        (setf (list-offset *session* dispatch-sym) next)))
                                    :stay)
                                   (t (handle-key dispatch-sym aid-kw))))
                             (redisplay () :stay))))
                    ;; Interpret result
                    (cond
                      ((eq result :logoff)
                       (return))
                      ((or (eq result :stay) (null result))
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
                             result dispatch-sym)))))))))))))) ;; end progn
      ;; Unregister session
      (bt:with-lock-held ((application-sessions-lock application))
        (setf (application-sessions application)
              (remove *session* (application-sessions application)))))
