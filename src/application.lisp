;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; application.lisp
;;;;
;;;; Application and session abstractions for lispf.
;;;; Provides define-application macro, session class, and main loop.

(in-package #:lispf)

;;; Application class

(defclass application ()
  ((name :initarg :name :accessor application-name)
   (title :initarg :title :accessor application-title :initform nil)
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
   (menus :initform (make-hash-table :test 'equal) :accessor application-menus
          :documentation "Menu data: name-string -> menu plist from .menu files.")
   (menu-timestamps :initform (make-hash-table :test 'equal)
                     :accessor application-menu-timestamps
                     :documentation "Menu file timestamps: name-string -> (path . write-date).")
   (menu-entries :initform nil :accessor application-menu-entries
                 :documentation "Alist of (key-string . screen-symbol) from all loaded menus.")
   (sessions :initform '() :accessor application-sessions
             :documentation "List of active sessions (one per connection).")
   (sessions-lock :initform (bt:make-lock "sessions") :reader application-sessions-lock)))

(defgeneric unknown-key-message (application key-name)
  (:documentation "Return the error message to display when an unknown key is pressed.
KEY-NAME is the string name of the key (e.g. \"PF5\").")
  (:method ((app application) key-name)
    (format nil "~A: unknown key" key-name)))

(defmethod initialize-instance :after ((app application) &key)
  (let ((dir (application-screen-directory app)))
    (when dir
      (pushnew (truename dir) (application-screen-directories app) :test #'equal))))

;;; Session class

(defclass session ()
  ((application :initarg :application :reader session-application)
   (active-application :accessor session-active-application :initform nil)
   (current-screen :accessor session-current-screen)
   (last-activity :initform (get-universal-time) :accessor session-last-activity)
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
                :reader session-update-cond)
   (current-response :initform nil :accessor session-current-response)
   (cursor-row :initform 0 :accessor session-cursor-row)
   (cursor-col :initform 0 :accessor session-cursor-col)
   (tls-p :initform nil :accessor session-tls-p
          :documentation "True when this session's connection is TLS-encrypted.")))

(defun session-property (session key &optional default)
  "Get a session property by key."
  (gethash key (session-properties session) default))

(defun (setf session-property) (value session key &optional default)
  "Set a session property by key."
  (declare (ignore default))
  (setf (gethash key (session-properties session)) value))

;;; Per-session state accessors

(defun current-response ()
  "Return the raw 3270 response from the last key press."
  (session-current-response *session*))

(defun (setf current-response) (value)
  (setf (session-current-response *session*) value))

(defun cursor-row ()
  "Return the cursor row from the last 3270 response."
  (session-cursor-row *session*))

(defun (setf cursor-row) (value)
  (setf (session-cursor-row *session*) value))

(defun cursor-col ()
  "Return the cursor column from the last 3270 response."
  (session-cursor-col *session*))

(defun (setf cursor-col) (value)
  (setf (session-cursor-col *session*) value))

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

;;; Screen alias management

(defun collect-screen-aliases (app)
  "Scan all loaded screens and build an alist of (alias . screen-symbol) pairs."
  (let ((aliases nil)
        (pkg (application-package app)))
    (maphash (lambda (name-string info)
               (let ((screen-sym (intern (string-upcase name-string) pkg)))
                 (dolist (alias (screen-info-aliases info))
                   (push (cons alias screen-sym) aliases))))
             (application-screens app))
    aliases))

(defun screen-matches-command-p (name-string info command)
  "Return T if a navigable screen matches COMMAND by name or alias."
  (and (screen-info-navigable info)
       (or (string-equal command name-string)
           (member command (screen-info-aliases info) :test #'string-equal))))

(defun find-screen-alias (app command)
  "Look up COMMAND against navigable screen names and aliases.
Returns a screen symbol or NIL. Lazily loads screens from disk."
  (maphash (lambda (name-string info)
             (when (screen-matches-command-p name-string info command)
               (return-from find-screen-alias
                 (intern (string-upcase name-string) (application-package app)))))
           (application-screens app))
  ;; Try loading screens from disk that aren't cached yet
  (dolist (dir (application-screen-directories app))
    (dolist (path (directory (merge-pathnames "*.screen" dir)))
      (let ((name (pathname-name path)))
        (unless (gethash name (application-screens app))
          (let ((info (load-and-register-screen name)))
            (when (screen-matches-command-p name info command)
              (return-from find-screen-alias
                (intern (string-upcase name) (application-package app)))))))))
  nil)

;;; Menu loading

(defun load-menu-file (path)
  "Load a .menu file and return the data plist."
  (with-open-file (s path)
    (let ((*package* (find-package :lispf)))
      (read s))))

(defun collect-menu-entries (items pkg &optional prefix)
  "Recursively collect (key . screen-symbol) pairs from menu ITEMS.
PREFIX is prepended with a dot for nested items (e.g. \"5.1\")."
  (let ((entries nil))
    (dolist (item items)
      (let* ((key (getf item :key))
             (full-key (if prefix (format nil "~A.~A" prefix key) key))
             (screen (getf item :screen))
             (sub-items (getf item :items)))
        (when (and key screen)
          (push (cons full-key (intern (string-upcase (string screen)) pkg))
                entries))
        (when sub-items
          (setf entries (nconc (nreverse (collect-menu-entries sub-items pkg full-key))
                               entries)))))
    (nreverse entries)))

(defun load-application-menus (app)
  "Load all .menu files from the application's screen directories."
  (dolist (dir (application-screen-directories app))
    (dolist (path (directory (merge-pathnames "*.menu" dir)))
      (let* ((data (load-menu-file path))
             (name (getf data :name)))
        (setf (gethash name (application-menus app)) data)
        (setf (gethash name (application-menu-timestamps app))
              (cons path (file-write-date path))))))
  (rebuild-menu-entries app))

(defun rebuild-menu-entries (app)
  "Rebuild the flattened menu-entries alist from all loaded menus."
  (let ((pkg (application-package app))
        (entries nil))
    (maphash (lambda (name data)
               (declare (ignore name))
               (setf entries
                     (nconc (collect-menu-entries (getf data :items) pkg)
                            entries)))
             (application-menus app))
    (setf (application-menu-entries app) entries)))

(defun find-menu-file (name-string)
  "Search the current application's directories for NAME-STRING.menu."
  (loop for dir in (application-screen-directories *application*)
        for path = (merge-pathnames (make-pathname :name name-string :type "menu") dir)
        when (probe-file path) return path))

(defun menu-file-changed-p (name-string)
  "Check if the .menu file on disk is newer than the cached version."
  (let ((cached (when *application*
                  (gethash name-string (application-menu-timestamps *application*)))))
    (when cached
      (let ((disk-time (file-write-date (car cached))))
        (and disk-time (> disk-time (cdr cached)))))))

(defun ensure-menu-loaded (name-string)
  "Check if a menu file has changed on disk and reload it if so."
  (when (and *application* (menu-file-changed-p name-string))
    (let* ((cached (gethash name-string (application-menu-timestamps *application*)))
           (path (car cached))
           (data (load-menu-file path)))
      (setf (gethash name-string (application-menus *application*)) data)
      (setf (gethash name-string (application-menu-timestamps *application*))
            (cons path (file-write-date path)))
      (rebuild-menu-entries *application*)
      ;; Clear the cached generated screen so it gets regenerated
      (remhash name-string (app-screens))
      t)))

(defun find-menu-entry (app key)
  "Look up KEY in the application's menu entries. Returns a screen symbol or NIL."
  (cdr (assoc key (application-menu-entries app) :test #'string-equal)))

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
  :title string          - display title for the title line (default: name)

Example:
  (define-application *my-app*
    :title \"My App\"
    :entry-screen welcome
    :screen-directory #p\"screens/\")"
  (let ((entry-screen (getf options :entry-screen))
        (screen-directory (getf options :screen-directory))
        (session-class (getf options :session-class ''session))
        (title (getf options :title)))
    (unless entry-screen
      (error "define-application requires :entry-screen"))
    (unless screen-directory
      (error "define-application requires :screen-directory"))
    ;; Intern entry-screen in the current package at expansion time
    (let ((entry-sym (intern (string-upcase (string entry-screen)) *package*)))
      `(defvar ,name
         (make-instance 'application
                        :name ,(string-downcase (string name))
                        ,@(when title `(:title ,title))
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
      (destructuring-bind (aid-keyword label &rest rest) spec
        (declare (ignore label))
        (let ((constant (symbol-value (aid-keyword-to-constant aid-keyword))))
          (if (getf rest :back)
              (push constant exit-keys)
              (push constant pf-keys)))))
    (values (coerce (nreverse pf-keys) 'vector)
            (coerce (nreverse exit-keys) 'vector))))

(defun format-key-labels-from-specs (key-specs key-layout)
  "Build row 23 key label string with fixed positions from KEY-LAYOUT.
Keys are placed at their pre-computed column positions. Hidden keys leave
blank space. Keys not in the layout (added at runtime) are appended."
  (let ((line (make-string 80 :initial-element #\Space))
        (appended nil))
    (dolist (entry key-specs)
      (destructuring-bind (aid-keyword label . rest) entry
        (declare (ignore rest))
        (when (and label (string/= label ""))
          (let ((slot (assoc aid-keyword key-layout)))
            (if slot
                (destructuring-bind (col width) (rest slot)
                  (let* ((text (format nil "~A ~A"
                                       (aid-keyword-display-name aid-keyword) label))
                         (len (min (length text) width)))
                    (replace line text :start1 col :end2 len)))
                (push (format nil "~A ~A"
                              (aid-keyword-display-name aid-keyword) label)
                      appended))))))
    (when appended
      ;; Find the end of laid-out content and append extra keys
      (let ((pos (1+ (or (position #\Space line :from-end t :test #'char/=)
                         -1))))
        (dolist (text (nreverse appended))
          (let* ((start (min (+ pos 2) 79))
                 (len (min (length text) (- 80 start))))
            (when (> len 0)
              (replace line text :start1 start :end2 len))
            (setf pos (+ start len))))))
    (unless (every (lambda (c) (char= c #\Space)) line)
      line)))

(defun find-key-spec (key-specs aid-keyword)
  "Find the key spec for AID-KEYWORD in KEY-SPECS. Returns the spec or nil."
  (find aid-keyword key-specs :key #'first))

;;; Framework field names (excluded from context merge)

(defparameter *framework-fields* '("title" "cmdlabel" "command" "errormsg" "keys")
  "Field names managed by the framework, excluded from context.")

(defconstant +command-field-width+ 65
  "Width of the command input field (col 14 through col 78).")

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
      (destructuring-bind (aid-keyword label &rest rest) spec
        (declare (ignore label))
        (unless (or (getf rest :back) (getf rest :goto)
                    (and list-screen-p (member aid-keyword '(:pf7 :pf8))))
          (let* ((methods (compute-applicable-methods
                           #'handle-key (list screen-sym aid-keyword)))
                 (has-specific (find-if (lambda (m) (not (eq m default-method)))
                                        methods)))
            (unless has-specific
              (warn "Screen ~S: no handler for ~S (define with define-key-handler)"
                    screen-sym aid-keyword))))))))


;;; Server and connection handling

(defun handle-connection (application socket &key tls-config tls-immediate-p)
  "Handle a single 3270 connection for APPLICATION on SOCKET.
When TLS-IMMEDIATE-P is true, perform TLS handshake immediately (dedicated TLS port).
When TLS-CONFIG is provided without TLS-IMMEDIATE-P, STARTTLS is offered during
telnet negotiation."
  (declare (type usocket:stream-usocket socket))
  (handler-case
      (progn
        (when tls-immediate-p
          (cl3270:wrap-socket-with-tls socket tls-config))
        (multiple-value-bind (devinfo err starttls-established)
            (cl3270:negotiate-telnet socket
                                     :tls-config (unless tls-immediate-p tls-config))
          (when err
            (format *error-output* "~&;;; ~A: negotiation error: ~A~%"
                    (application-name application) err)
            (return-from handle-connection))
          (unwind-protect
               (run-application application socket devinfo
                                :tls-p (or tls-immediate-p starttls-established))
            (cl3270:unnegotiate-telnet socket 1))))
    (error (e)
      (format *error-output* "~&;;; ~A: connection error: ~A~%"
              (application-name application) e))))

;;; Default command processing (menu entries + screen aliases)

(defmethod process-command ((application application) (command string))
  "Default: look up command in menu entries and screen aliases.
'=' prefix triggers a jump (resets screen stack to entry screen)."
  (let* ((trimmed (string-trim '(#\Space) command))
         (jump-p (and (plusp (length trimmed))
                      (char= (char trimmed 0) #\=)))
         (key (if jump-p (subseq trimmed 1) trimmed))
         (target (or (find-menu-entry application key)
                     (find-screen-alias application key))))
    (when target
      (if jump-p
          (cons :jump target)
          target))))

;;; Menu screen support via list data

(defun menu-items-flat (items)
  "Flatten nested menu items into a single list for display."
  (let ((result nil))
    (dolist (item items)
      (push item result)
      ;; Don't recurse into sub-items for the current menu display
      )
    (nreverse result)))

(defmethod get-list-data :around ((screen-name symbol) start end)
  "Auto-populate repeat fields for menu screens from menu definitions."
  (let* ((name-string (screen-name-string screen-name))
         (info (gethash name-string (app-screens))))
    (if (and info (screen-info-menu info))
        (let* ((menu-name (screen-info-menu info))
               (menu (gethash menu-name (application-menus *application*)))
               (items (when menu (menu-items-flat (getf menu :items))))
               (total (length items))
               (page (subseq items start (min end total))))
          (values
           (mapcar (lambda (item)
                     (list :key (getf item :key)
                           :label (getf item :label)
                           :description (or (getf item :description) "")))
                   page)
           total))
        (call-next-method))))

;;; Menu item selection via Enter key

(defun menu-screen-select (screen-name)
  "Handle Enter key on a menu screen. Returns screen symbol or NIL."
  (let* ((name-string (screen-name-string screen-name))
         (info (gethash name-string (app-screens)))
         (menu-name (when info (screen-info-menu info))))
    (when menu-name
      (let* ((menu (gethash menu-name (application-menus *application*)))
             (items (when menu (menu-items-flat (getf menu :items))))
             (index (selected-list-index)))
        (when (and index (< index (length items)))
          (let ((item (nth index items))
                (pkg (application-package *application*)))
            (when (getf item :screen)
              (intern (string-upcase (string (getf item :screen))) pkg))))))))

(defun start-application (application &key (port 3270) (host "127.0.0.1")
                                          tls-port certificate-file key-file
                                          key-password (starttls t)
                                          listener-callback)
  "Start APPLICATION, listening for 3270 connections on HOST:PORT.
Serves multiple users concurrently using threads.
When CERTIFICATE-FILE and KEY-FILE are provided, TLS is available.
TLS-PORT enables a dedicated TLS listener on that port.
STARTTLS (default T) offers STARTTLS negotiation on the plain port.
When LISTENER-CALLBACK is provided, it is called with the listener socket
after binding but before the accept loop. Useful for test harnesses."
  (let ((tls-config (when certificate-file
                      (make-instance 'cl3270:tls-config
                                     :certificate-file certificate-file
                                     :key-file key-file
                                     :key-password key-password
                                     :starttls-p starttls)))
        (tls-listener-socket nil))
    (load-application-menus application)
    ;; Start dedicated TLS listener in background thread
    (when (and tls-config tls-port)
      (bt:make-thread
       (lambda ()
         (let ((out *standard-output*)
               (err *error-output*))
           (usocket:with-socket-listener (tls-listener host tls-port
                                                       :element-type '(unsigned-byte 8)
                                                       :reuse-address t)
             (setf tls-listener-socket tls-listener)
             (format out "~&;;; ~A: TLS listening on ~A:~D~%"
                     (application-name application) host tls-port)
             (handler-case
                 (loop
                   (let ((c (usocket:socket-accept tls-listener)))
                     (bt:make-thread
                      (lambda ()
                        (let ((*standard-output* out)
                              (*error-output* err))
                          (unwind-protect
                               (handle-connection application c
                                                  :tls-config tls-config
                                                  :tls-immediate-p t)
                            (ignore-errors (usocket:socket-close c)))))
                      :name (format nil "~A-tls-~A"
                                    (application-name application)
                                    (usocket:get-peer-address c)))))
               (error ()
                 (format err "~&;;; ~A: TLS listener stopped~%"
                         (application-name application)))))))
       :name (format nil "~A-tls-listener" (application-name application))))
    ;; Plain port listener (main thread)
    (format t "~&;;; ~A: listening on ~A:~D~%" (application-name application) host port)
    (unwind-protect
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
                         (handle-connection application c :tls-config tls-config)
                      (ignore-errors (usocket:socket-close c)))))
                :name (format nil "~A-~A"
                               (application-name application)
                               (usocket:get-peer-address c))))))
      (when tls-listener-socket
        (ignore-errors (usocket:socket-close tls-listener-socket))))))

;;; Dynamic key label management

(defvar *current-screen-keys* nil
  "Current key display specs for the screen being prepared.
Bound by run-application for each iteration. Modified by show-key/hide-key.")

(defvar *current-key-layout* nil
  "Key layout for the current screen. Set by run-screen-loop each iteration.")

(defun show-key (aid-keyword label)
  "Make AID-KEYWORD visible in the key labels with LABEL.
Call from within a define-screen-update body.
Warns if AID-KEYWORD is not declared in the screen's key specs."
  (let ((entry (assoc aid-keyword *current-screen-keys*)))
    (if entry
        (setf (second entry) label)
        (progn
          (unless (assoc aid-keyword *current-key-layout*)
            (warn "show-key: ~A is not declared in the screen's key specs" aid-keyword))
          (setf *current-screen-keys*
                (append *current-screen-keys* (list (list aid-keyword label))))))))

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

(defun check-field-rule (field fr value orig-values)
  "Check a single field rule. Returns an error message string, or NIL if valid."
  (cond
    ((and (cl3270:field-rules-must-change fr)
          (string= value (gethash field orig-values)))
     (cl3270:field-rules-error-text fr))
    ((and (cl3270:field-rules-validator fr)
          (not (funcall (cl3270:field-rules-validator fr) value)))
     (format nil "Value for ~S is not valid" field))))

(defun validate-fields (rules my-vals orig-values error-field)
  "Check field validation rules. Returns T if all pass, NIL if failed.
On failure, sets the error message in MY-VALS under ERROR-FIELD."
  (or (null rules)
      (maphash (lambda (field fr)
                 (multiple-value-bind (value present) (gethash field my-vals)
                   (when-let (err (and present
                                       (check-field-rule field fr value orig-values)))
                     (setf (gethash error-field my-vals) err)
                     (return-from validate-fields nil))))
               rules)
      t))

(defun send-title-overlay (ctx)
  "Send a title-only overlay to update the clock and indicators.
Skips the send if the update thread has been asked to stop, to avoid
unlocking the 3270 keyboard after the main thread received a response."
  (unless (update-context-running ctx)
    (return-from send-title-overlay))
  (let* ((title (format-title-line (update-context-screen-sym ctx)
                                   (session-indicator-texts)))
         (screen (cl3270:make-screen "title-overlay"
                   (cl3270:make-field :row 0 :col 0 :name "title"
                                      :position-only t)
                   (cl3270:make-field :row 0 :col 79 :name "")))
         (vals (cl3270:make-dict :test #'equal)))
    (setf (gethash "title" vals) title)
    (bt:with-lock-held ((session-write-lock *session*))
      ;; Double-check running inside the lock -- stop-updates acquires
      ;; this lock after setting running=nil to create a fence.
      (when (update-context-running ctx)
        (cl3270:show-screen-opts screen vals *connection*
          (cl3270:make-screen-opts :no-clear t :no-response t))))))

(defun pad-or-truncate (string width)
  "Pad STRING with spaces or truncate it to exactly WIDTH characters."
  (let ((len (length string)))
    (cond ((= len width) string)
          ((> len width) (subseq string 0 width))
          (t (concatenate 'string string
                          (make-string (- width len) :initial-element #\Space))))))

;;; Inline attribute parsing for dynamic area strings
;;;
;;; Attribute codes (after *attribute-intro-char*, default ^):
;;;   Colors:       r red, b blue, g green, p pink, t turquoise, y yellow, w white, 0 default
;;;   Highlighting: _ underscore, ! blink, ~ reverse-video, . default
;;;   Intensity:    * intense, - normal
;;;   Protection:   + unprotected (writable), = protected (default)
;;;   Literal:      ^^ produces a single ^

(defparameter *attribute-code-map*
  `((#\r :color ,cl3270:+red+)
    (#\b :color ,cl3270:+blue+)
    (#\g :color ,cl3270:+green+)
    (#\p :color ,cl3270:+pink+)
    (#\t :color ,cl3270:+turquoise+)
    (#\y :color ,cl3270:+yellow+)
    (#\w :color ,cl3270:+white+)
    (#\0 :color 0)
    (#\_ :highlighting ,cl3270:+underscore+)
    (#\! :highlighting ,cl3270:+blink+)
    (#\~ :highlighting ,cl3270:+reverse-video+)
    (#\. :highlighting 0)
    (#\* :intense t)
    (#\- :intense nil)
    (#\+ :write t)
    (#\= :write nil)))

(defun lookup-attribute-code (char)
  "Return (key value) for an attribute code character, or NIL."
  (let ((entry (assoc char *attribute-code-map*)))
    (when entry (rest entry))))

(defun parse-attributed-string (string)
  "Parse STRING with inline attribute codes into a list of segments.
Each segment is (text . plist) where plist contains :color, :highlighting, :intense
as accumulated from attribute codes. Returns a single segment with no attributes
if STRING contains no attribute intro characters."
  (let ((intro *attribute-intro-char*)
        (segments '())
        (buf (make-string-output-stream))
        (color 0)
        (highlighting 0)
        (intense nil)
        (writable nil)
        (i 0)
        (len (length string)))
    (unless (position intro string)
      (return-from parse-attributed-string (list (cons string nil))))
    (labels ((flush-segment ()
               (let ((text (get-output-stream-string buf)))
                 (when (plusp (length text))
                   (let ((attrs (append
                                 (unless (= color 0)
                                   (list :color color))
                                 (unless (= highlighting 0)
                                   (list :highlighting highlighting))
                                 (when intense
                                   (list :intense t))
                                 (when writable
                                   (list :write t)))))
                     (push (cons text attrs) segments))))))
      (loop while (< i len)
            for ch = (char string i)
            do (cond
                 ((and (char= ch intro) (< (1+ i) len))
                  (let* ((code-char (char string (1+ i)))
                         (attr (lookup-attribute-code code-char)))
                    (cond
                      ((char= code-char intro)
                       ;; Doubled intro = literal
                       (write-char intro buf)
                       (incf i 2))
                      (attr
                       (flush-segment)
                       (ecase (first attr)
                         (:color (setf color (second attr)))
                         (:highlighting (setf highlighting (second attr)))
                         (:intense (setf intense (second attr)))
                         (:write (setf writable (second attr))))
                       (incf i 2))
                      (t
                       ;; Unknown code, pass through literally
                       (write-char ch buf)
                       (incf i)))))
                 (t
                  (write-char ch buf)
                  (incf i))))
      (flush-segment))
    (or (nreverse segments) (list (cons "" nil)))))

(defun build-dynamic-area-overlay (area content)
  "Build a screen and vals for overlaying CONTENT onto a dynamic AREA.
Each element of CONTENT may be:
  - A string, optionally containing inline attribute codes (^r, ^b, ^_, etc.)
  - A plist (:content STRING :color C :highlighting H :intense T)
  - NIL for a blank row.
When the area starts at column 0, the attribute byte is placed at the end of
the previous row so content fills the full row width.
Returns (values screen vals)."
  (let* ((from-row (1+ (dynamic-area-from-row area)))
         (raw-from-col (dynamic-area-from-col area))
         (full-width-p (zerop raw-from-col))
         (attr-col (if full-width-p (1- +screen-columns+) raw-from-col))
         (content-col (if full-width-p 0 (1+ raw-from-col)))
         (to-row (1+ (dynamic-area-to-row area)))
         (content-width (if full-width-p
                            +screen-columns+
                            (- (dynamic-area-to-col area) raw-from-col)))
         (fields '())
         (vals (cl3270:make-dict :test #'equal))
         (field-idx 0))
    (flet ((add-field (row col name attrs text width)
             (push (apply #'cl3270:make-field :row row :col col :name name attrs) fields)
             (setf (gethash name vals) (pad-or-truncate text width)))
           (attr-row (row)
             (if full-width-p (1- row) row)))
      (loop for row from from-row to to-row
            for i from 0
            for entry = (if (< i (length content)) (nth i content) nil)
            ;; Last row of full-width area: use 79 chars to preserve the
            ;; next field's attribute byte at (to-row, 79)
            for row-width = (if (and full-width-p (= row to-row))
                                (1- content-width)
                                content-width)
            do (cond
                 ;; Plist entry: single field with explicit attributes
                 ((consp entry)
                  (let ((name (format nil "dyn-~A-~D" (dynamic-area-name area) (incf field-idx)))
                        (line (or (getf entry :content) ""))
                        (attrs (remove-from-plist entry :content)))
                    (add-field (attr-row row) attr-col name attrs line row-width)))
                 ;; String entry: parse for inline attribute codes
                 ((stringp entry)
                  (let ((segments (parse-attributed-string entry))
                        (col content-col)
                        (remaining row-width)
                        (first-seg t))
                    (dolist (seg segments)
                      (when (plusp remaining)
                        (let* ((text (car seg))
                               (attrs (cdr seg))
                               (name (format nil "dyn-~A-~D" (dynamic-area-name area) (incf field-idx)))
                               ;; Non-first segments lose 1 char for the attribute byte
                               (attr-overhead (if first-seg 0 1))
                               (seg-width (min (length text) (- remaining attr-overhead)))
                               (field-row (if first-seg (attr-row row) row))
                               (field-col (if first-seg attr-col col)))
                          (when (plusp seg-width)
                            (add-field field-row field-col name attrs
                                       (subseq text 0 seg-width) seg-width)
                            (setf first-seg nil)
                            (incf col (+ seg-width attr-overhead))
                            (decf remaining (+ seg-width attr-overhead))))))
                    ;; Pad remainder of the row if segments didn't fill it
                    (when (plusp remaining)
                      (let ((name (format nil "dyn-~A-~D" (dynamic-area-name area) (incf field-idx))))
                        (add-field row col name nil "" remaining)))))
                 ;; NIL: blank row
                 (t
                  (let ((name (format nil "dyn-~A-~D" (dynamic-area-name area) (incf field-idx))))
                    (add-field (attr-row row) attr-col name nil "" row-width))))))
    (values (apply #'cl3270:make-screen "dynamic-overlay" (nreverse fields))
            vals)))

(defun merge-initial-dynamic-areas (screen vals update-ctx)
  "Merge dynamic area content into SCREEN for the initial display.
Calls dynamic area updaters and combines their fields with the main screen fields,
so everything is sent in a single 3270 write. Returns the merged screen."
  (let* ((screen-sym (update-context-screen-sym update-ctx))
         (areas (get-screen-dynamic-areas screen-sym))
         (app-package (application-package *application*))
         (extra-fields '()))
    (dolist (area areas)
      (let ((area-sym (intern (string-upcase (dynamic-area-name area)) app-package)))
        (when-let (content (update-dynamic-area screen-sym area-sym))
          (multiple-value-bind (overlay-screen overlay-vals)
              (build-dynamic-area-overlay area content)
            ;; Merge overlay field values into main vals
            (maphash (lambda (k v) (setf (gethash k vals) v)) overlay-vals)
            ;; Collect overlay fields
            (dolist (f (cl3270:screen-fields overlay-screen))
              (push f extra-fields))))))
    (if extra-fields
        (apply #'cl3270:make-screen
               (cl3270:screen-name screen)
               (append (cl3270:screen-fields screen) (nreverse extra-fields)))
        screen)))

(defun send-dynamic-area-overlays (ctx)
  "Call dynamic area updaters for the current screen and send overlays."
  (unless (update-context-running ctx)
    (return-from send-dynamic-area-overlays))
  (let* ((screen-sym (update-context-screen-sym ctx))
         (areas (get-screen-dynamic-areas screen-sym))
         (app-package (application-package *application*)))
    (dolist (area areas)
      (unless (update-context-running ctx) (return))
      (let ((area-sym (intern (string-upcase (dynamic-area-name area)) app-package)))
        (when-let (content (update-dynamic-area screen-sym area-sym))
          (multiple-value-bind (screen vals) (build-dynamic-area-overlay area content)
            (bt:with-lock-held ((session-write-lock *session*))
              (when (update-context-running ctx)
                (cl3270:show-screen-opts screen vals *connection*
                  (cl3270:make-screen-opts :no-clear t :no-response t))))))))))

(defun wait-for-update-signal (ctx)
  "Block until the update thread is signaled or 1 second elapses.
Returns NIL if the thread should exit."
  (bt:with-lock-held ((session-update-lock *session*))
    (bt:condition-wait (session-update-cond *session*)
                        (session-update-lock *session*)
                        :timeout 1))
  (update-context-running ctx))

(defun minute-changed-p (ctx)
  "Return T and update last-minute if the clock minute has changed."
  (let ((current (nth-value 1 (decode-universal-time (get-universal-time)))))
    (when (/= current (update-context-last-minute ctx))
      (setf (update-context-last-minute ctx) current)
      t)))

(defun consume-dirty-indicators-p ()
  "Return T and clear the dirty flag if indicators were changed."
  (bt:with-lock-held ((session-update-lock *session*))
    (when (session-indicators-dirty *session*)
      (setf (session-indicators-dirty *session*) nil)
      t)))

(defun title-needs-update-p (ctx)
  "Return T if the title overlay should be re-sent."
  (or (minute-changed-p ctx)
      (consume-dirty-indicators-p)))

(defgeneric session-idle-timeout (application session)
  (:documentation "Return the idle timeout in seconds for SESSION, or NIL for no timeout.
Called by the background update thread. When the session has been idle longer
than the returned value, the connection is closed.")
  (:method (application session)
    (declare (ignore application session))
    nil))

(defun check-idle-timeout (ctx)
  "Check if the session has exceeded its idle timeout. Closes the connection if so."
  (let ((timeout (session-idle-timeout *application* *session*)))
    (when (and timeout
               (> (- (get-universal-time) (session-last-activity *session*))
                  timeout))
      (setf (update-context-running ctx) nil)
      (ignore-errors (usocket:socket-close *connection*))
      t)))

(defun update-thread-fn (ctx)
  "Background thread: pushes title and dynamic area overlays periodically.
First update fires immediately, subsequent updates wait up to 1 second."
  (handler-case
      (progn
        ;; Immediate first update for dynamic areas
        (when (update-context-running ctx)
          (send-dynamic-area-overlays ctx))
        (loop while (wait-for-update-signal ctx)
              when (check-idle-timeout ctx) do (return)
              when (title-needs-update-p ctx)
                do (send-title-overlay ctx)
              do (send-dynamic-area-overlays ctx)))
    (error (e)
      (format *error-output* "~&;;; Update thread error: ~A~%" e))))

(defun start-updates (ctx)
  "Start the background update thread."
  (setf (update-context-running ctx) t)
  (let ((session *session*)
        (connection *connection*)
        (device-info *device-info*)
        (application *application*))
    (setf (update-context-thread ctx)
          (bt:make-thread
           (lambda ()
             (let ((*session* session)
                   (*connection* connection)
                   (*device-info* device-info)
                   (*application* application))
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
                          &key screen-sym devinfo codepage no-clear)
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
      ;; Merge dynamic area content into the main screen for the initial display,
      ;; so the complete screen arrives in a single write (no race with the client).
      (let* ((update-ctx (when screen-sym
                           (make-update-context
                            :screen-sym screen-sym
                            :last-minute (nth-value 1
                                           (decode-universal-time
                                            (get-universal-time))))))
             (display-screen (if update-ctx
                                 (merge-initial-dynamic-areas screen my-vals update-ctx)
                                 screen)))
        (multiple-value-bind (resp err)
            (cl3270:show-screen-opts display-screen my-vals conn
              (cl3270:make-screen-opts
               :cursor-row cursor-row :cursor-col cursor-col
               :altscreen devinfo :codepage codepage
               :no-clear no-clear
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
                   (unknown-key-message *application*
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


;;; Field attribute overrides

(defun copy-field-with-overrides (field attrs)
  "Return a copy of FIELD with attributes from ATTRS plist applied."
  (let ((new (copy-structure field)))
    (loop for (key val) on attrs by #'cddr
          do (case key
               (:write (setf (cl3270::field-write new) val))
               (:intense (setf (cl3270:field-intense new) val))
               (:hidden (setf (cl3270:field-hidden new) val))
               (:color (setf (cl3270:field-color new) (map-symbolic-value val)))
               (:highlighting (setf (cl3270:field-highlighting new)
                                    (map-symbolic-value val)))))
    new))

(defun apply-field-attribute-overrides (screen)
  "If *field-attribute-overrides* is non-nil, return a new screen with overrides applied.
Otherwise return SCREEN unchanged."
  (if (null *field-attribute-overrides*)
      screen
      (apply #'cl3270:make-screen
             (cl3270:screen-name screen)
             (mapcar (lambda (f)
                       (let ((override (assoc (cl3270:field-name f)
                                              *field-attribute-overrides*
                                              :test #'string-equal)))
                         (if override
                             (copy-field-with-overrides f (cdr override))
                             f)))
                     (cl3270:screen-fields screen)))))

;;; Main application loop — helper functions

(defun ensure-key-handlers-validated (dispatch-sym key-specs)
  "Validate key handlers for DISPATCH-SYM once per application lifetime."
  (unless (gethash dispatch-sym (application-validated-screens *application*))
    (setf (gethash dispatch-sym (application-validated-screens *application*)) t)
    (check-screen-key-handlers dispatch-sym key-specs)))

(defun populate-field-values (context field-values screen dispatch-sym
                              repeat-groups)
  "Copy context into FIELD-VALUES and distribute list/repeat data.
Returns (values list-data-count list-data-total has-list-data)."
  (maphash (lambda (k v) (setf (gethash k field-values) v)) context)
  (let ((list-data-count nil)
        (list-data-total nil)
        (has-list-data nil))
    (when repeat-groups
      (multiple-value-bind (count total)
          (distribute-list-data dispatch-sym repeat-groups context field-values)
        (when count
          (setf list-data-count count
                list-data-total total
                has-list-data t)
          (let* ((page-size (reduce #'max repeat-groups
                                    :key #'second :initial-value 0))
                 (offset (list-offset *session* dispatch-sym)))
            (multiple-value-bind (prev-label next-label)
                (paging-labels *application*)
              (when (> offset 0)
                (show-key :pf7 prev-label))
              (when (< (+ offset page-size) (or total 0))
                (show-key :pf8 next-label)))))))
    (when (and repeat-groups (not has-list-data))
      (split-repeat-field-value
       repeat-groups
       (lambda (base-name) (repeat-field-length screen base-name))
       field-values))
    (values list-data-count list-data-total has-list-data)))

(defun set-framework-fields (screen-sym field-values &key no-command is-menu
                                                              full-control)
  "Set title, command line, errormsg, and key label fields in FIELD-VALUES.
With FULL-CONTROL, do nothing (app manages all fields)."
  (when full-control
    (return-from set-framework-fields))
  (setf (gethash "title" field-values)
        (format-title-line screen-sym (session-indicator-texts)))
  ;; Command line (only on screens with command field)
  (unless no-command
    (unless (gethash "cmdlabel" field-values)
      (setf (gethash "cmdlabel" field-values)
            (if is-menu
                (menu-command-label *application*)
                (default-command-label *application*))))
    (setf (gethash "command" field-values)
          (make-string +command-field-width+ :initial-element #\Space)))
  (setf (gethash "errormsg" field-values)
        (format nil "~80A" (or (gethash "errormsg" field-values) "")))
  (let ((key-labels (format-key-labels-from-specs *current-screen-keys*
                                                   *current-key-layout*)))
    (when key-labels
      (setf (gethash "keys" field-values) key-labels))))

(defun compute-cursor-position (screen display-screen dispatch-sym
                                repeat-groups has-list-data list-data-count)
  "Determine cursor row and column for the screen display.
Returns (values cursor-row cursor-col).
Uses set-cursor override if set, otherwise falls back to the first writable field."
  (declare (ignore screen repeat-groups has-list-data list-data-count dispatch-sym))
  (let* ((first-write (unless *next-cursor-row*
                        (find-if (lambda (f) (cl3270::field-write f))
                                 (cl3270:screen-fields display-screen))))
         (cursor-row (or *next-cursor-row*
                         (if first-write (cl3270:field-row first-write) 23)))
         (cursor-col (or *next-cursor-col*
                         (if first-write (1+ (cl3270:field-col first-write)) 0))))
    ;; Handle wrapping attribute byte at col 79: content starts on next row
    (when (and first-write (not *next-cursor-row*) (>= cursor-col 80))
      (setf cursor-row (1+ cursor-row)
            cursor-col 0))
    (values cursor-row cursor-col)))

(defun merge-response-into-context (response context transient-fields)
  "Merge response field values into CONTEXT, skipping framework and transient fields."
  (let ((resp-vals (cl3270:response-vals response)))
    (when resp-vals
      (maphash (lambda (k v)
                 (unless (or (member k *framework-fields* :test #'string=)
                             (member k transient-fields :test #'string=))
                   (setf (gethash k context) v)))
               resp-vals))))

(defun page-backward (dispatch-sym repeat-groups)
  "Move list offset one page backward for DISPATCH-SYM."
  (let* ((page-size (reduce #'max repeat-groups
                            :key #'second :initial-value 0))
         (offset (list-offset *session* dispatch-sym)))
    (setf (list-offset *session* dispatch-sym)
          (max 0 (* page-size (1- (floor offset page-size)))))))

(defun page-forward (dispatch-sym repeat-groups list-data-total)
  "Move list offset one page forward for DISPATCH-SYM if more data exists."
  (let* ((page-size (reduce #'max repeat-groups
                            :key #'second :initial-value 0))
         (total (or list-data-total 0))
         (next (* page-size (1+ (floor (list-offset *session* dispatch-sym)
                                       page-size)))))
    (when (< next total)
      (setf (list-offset *session* dispatch-sym) next))))

(defun has-custom-key-handler-p (screen-sym aid-kw)
  "Return T if SCREEN-SYM has a non-default handle-key method for AID-KW."
  (let ((default-method (find-method #'handle-key '()
                                     (list (find-class t) (find-class t)))))
    (find-if (lambda (m) (not (eq m default-method)))
             (compute-applicable-methods #'handle-key (list screen-sym aid-kw)))))

(defun dispatch-key (context dispatch-sym aid-kw key-spec
                     has-list-data repeat-groups list-data-total app-package
                     command)
  "Dispatch AID-KW with error handling. Returns a navigation result.
COMMAND is the trimmed value from the command field (extracted from response)."
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
          ;; :back and :goto are defaults that custom handlers can override
          ((and key-spec (getf (cddr key-spec) :back))
           (if (has-custom-key-handler-p dispatch-sym aid-kw)
               (handle-key dispatch-sym aid-kw)
               :back))
          ((and key-spec (getf (cddr key-spec) :goto))
           (if (has-custom-key-handler-p dispatch-sym aid-kw)
               (handle-key dispatch-sym aid-kw)
               (intern-screen-name
                (string (getf (cddr key-spec) :goto))
                app-package)))
          ((and has-list-data (eq aid-kw :pf7))
           (page-backward dispatch-sym repeat-groups)
           :stay)
          ((and has-list-data (eq aid-kw :pf8))
           (page-forward dispatch-sym repeat-groups list-data-total)
           :stay)
          ;; Command field processing: check before normal key dispatch
          ((and (eq aid-kw :enter) (plusp (length command)))
           (let ((result (or (process-screen-command dispatch-sym command)
                             (process-command *application* command))))
             (cond
               (result
                ;; Command triggered navigation: save cursor at command field start
                (setf (cursor-row) 21 (cursor-col) 14)
                result)
               (t
                (setf (gethash "errormsg" context)
                      (unknown-command-message *application* command))
                :stay))))
          ;; Enter with empty command
          ((eq aid-kw :enter)
           (let* ((name-string (screen-name-string dispatch-sym))
                  (info (gethash name-string (app-screens)))
                  (is-menu (and info (screen-info-menu info))))
             (cond
               ;; Menu screen: cursor-based item selection or stay
               (is-menu (let ((target (menu-screen-select dispatch-sym)))
                          (when target
                            (setf (cursor-row) 21 (cursor-col) 14))
                          (or target :stay)))
               ;; Has an Enter key spec: dispatch normally
               (key-spec (handle-key dispatch-sym aid-kw))
               ;; No Enter handler: just stay
               (t :stay))))
          (t (handle-key dispatch-sym aid-kw))))
    (redisplay () :stay)))

(defun apply-screen-transition (result screen-sym dispatch-sym)
  "Interpret a key dispatch RESULT and update session state.
Returns :exit when the application loop should terminate.
Screen stack entries are (symbol cursor-row . cursor-col) to restore cursor on :back."
  (declare (ignore dispatch-sym))
  (cond
    ((eq result :logoff)
     :exit)
    ((or (eq result :stay) (null result))
     nil)
    ((eq result :back)
     (let ((prev (pop (session-screen-stack *session*))))
       (if prev
           (progn
             (setf (session-current-screen *session*) (car prev))
             ;; Return saved cursor position as secondary values
             (values nil (cadr prev) (cddr prev)))
           :exit)))
    ((and (consp result) (eq (car result) :jump))
     ;; Jump navigation: reset stack to just the entry screen, navigate to target
     (let ((root (application-entry-screen *application*)))
       (setf (session-screen-stack *session*)
             (list (cons root (cons 0 0))))
       (setf (session-current-screen *session*) (cdr result))))
    ((symbolp result)
     ;; Save current screen with cursor position
     (push (cons screen-sym (cons (cursor-row) (cursor-col)))
           (session-screen-stack *session*))
     (setf (session-current-screen *session*) result))
    (t
     (warn "Unexpected handle-key return value ~S on screen ~S, treating as :stay"
           result screen-sym))))

;;; Main application loop

(defun show-screen-and-read (screen display-screen screen-rules key-specs
                             field-values dispatch-sym repeat-groups
                             has-list-data list-data-count
                             &key no-command full-control no-clear)
  "Prepare the display screen, compute cursor, and read user input.
When NO-CLEAR is true, the screen is rewritten without erasing first.
Returns the 3270 response."
  (multiple-value-bind (cursor-row cursor-col)
      (compute-cursor-position screen display-screen dispatch-sym
                               repeat-groups has-list-data list-data-count)
    (multiple-value-bind (pf-keys exit-keys)
        (build-key-vectors key-specs)
      ;; Command screens always accept Enter (for the command field)
      (unless (or no-command
                  (find cl3270:+aid-enter+ pf-keys)
                  (find cl3270:+aid-enter+ exit-keys))
        (setf pf-keys (concatenate 'vector pf-keys
                                    (vector cl3270:+aid-enter+))))
      (multiple-value-bind (response err)
          (display-and-read display-screen screen-rules field-values
                            pf-keys exit-keys "errormsg"
                            cursor-row cursor-col *connection*
                            :screen-sym (unless full-control dispatch-sym)
                            :no-clear no-clear)
        (when err (error err))
        response))))

(defun process-response (response context dispatch-sym transient-fields
                         repeat-groups has-list-data)
  "Merge response into context, rejoin repeat fields, and update cursor state."
  (merge-response-into-context response context transient-fields)
  (when (and repeat-groups (not has-list-data))
    (join-repeat-field-values repeat-groups context))
  (remhash "errormsg" context)
  (setf (cursor-row) (cl3270:response-row response)
        (cursor-col) (cl3270:response-col response)
        (current-response) response)
  (when has-list-data
    (setf (list-state-value *session* dispatch-sym :cursor-row) (cursor-row))
    (setf (list-state-value *session* dispatch-sym :cursor-col) (cursor-col))))

(defun run-screen-loop (app-package)
  "Execute the main screen loop. Returns when the session ends."
  (let ((restored-cursor-row nil)
        (restored-cursor-col nil)
        (no-clear nil))
  (loop
    (block next-screen
    (let* ((screen-sym (session-current-screen *session*))
           (name-string (screen-name-string screen-sym))
           (screen-info (ensure-screen-loaded screen-sym))
           (screen (screen-info-screen screen-info))
           (screen-rules (screen-info-rules screen-info))
           (key-specs (screen-info-keys screen-info))
           (transient-fields (screen-info-transient-fields screen-info))
           (repeat-groups (screen-info-repeat-groups screen-info))
           (no-command (screen-info-no-command screen-info))
           (is-menu (screen-info-menu screen-info))
           (handler-package (screen-info-handler-package screen-info))
           (full-control (screen-info-full-control screen-info))
           (context (session-context *session*))
           (dispatch-sym (intern-screen-name name-string
                                             (or handler-package app-package)))
           (field-values (cl3270:make-dict :test #'equal)))
      (ensure-key-handlers-validated dispatch-sym key-specs)
      ;; Clear cmdlabel from context so stale overrides don't persist
      (remhash "cmdlabel" context)
      (let ((*current-screen-keys*
              (mapcar (lambda (spec)
                        (destructuring-bind (aid-kw label &rest rest) spec
                          (list aid-kw (if (getf rest :hidden) "" label))))
                      key-specs))
            (*current-key-layout* (get-screen-key-layout screen-sym))
            (*next-cursor-row* (prog1 restored-cursor-row
                                  (setf restored-cursor-row nil)))
            (*next-cursor-col* (prog1 restored-cursor-col
                                  (setf restored-cursor-col nil)))
            (*field-attribute-overrides* nil))
        (let ((prep-result (prepare-screen dispatch-sym)))
          (when (and prep-result (symbolp prep-result))
            (let ((transition (apply-screen-transition
                               prep-result screen-sym dispatch-sym)))
              (when (eq transition :exit)
                (return-from run-screen-loop))
              (when transition
                (multiple-value-bind (tr r c) transition
                  (declare (ignore tr))
                  (when r (setf restored-cursor-row r restored-cursor-col c)))))
            (return-from next-screen)))
        ;; Menu screens: auto-set cursor to command field
        (when (and is-menu (not *next-cursor-row*))
          (setf *next-cursor-row* 21
                *next-cursor-col* 14))
        (multiple-value-bind (list-data-count list-data-total has-list-data)
            (populate-field-values context field-values screen
                                   dispatch-sym repeat-groups)
          (set-framework-fields screen-sym field-values
                                :no-command no-command :is-menu is-menu
                                :full-control full-control)
          (let* ((display-screen (apply-field-attribute-overrides
                                 (if has-list-data
                                     (filter-screen-fields screen repeat-groups
                                                           list-data-count)
                                     screen)))
                 (response (prog1
                               (show-screen-and-read screen display-screen
                                                 screen-rules key-specs
                                                 field-values dispatch-sym
                                                 repeat-groups has-list-data
                                                 list-data-count
                                                 :no-command no-command
                                                 :full-control full-control
                                                 :no-clear no-clear)
                             (setf no-clear nil))))
            (setf (session-last-activity *session*) (get-universal-time))
            (process-response response context dispatch-sym transient-fields
                              repeat-groups has-list-data)
            ;; Temporarily inject transient field values so key handlers can
            ;; access them, then remove after dispatch so they don't persist.
            (let ((resp-vals (cl3270:response-vals response)))
              (when resp-vals
                (dolist (k transient-fields)
                  (multiple-value-bind (v present-p) (gethash k resp-vals)
                    (when present-p
                      (setf (gethash k context) v))))))
            (let* ((aid-kw (aid-to-keyword (cl3270:response-aid response)))
                   (key-spec (find-key-spec key-specs aid-kw))
                   (resp-command
                     (let ((rv (cl3270:response-vals response)))
                       (when rv
                         (string-trim '(#\Space) (or (gethash "command" rv) "")))))
                   (result (prog1
                               (dispatch-key context dispatch-sym aid-kw
                                             key-spec has-list-data
                                             repeat-groups list-data-total
                                             app-package
                                             (or resp-command ""))
                             (dolist (k transient-fields)
                               (remhash k context)))))
              ;; Check anonymous access before transitioning
              (let* ((target-sym (cond ((and (consp result) (eq (car result) :jump))
                                        (cdr result))
                                       ((symbolp result) result)))
                     (checked-result
                      (if (and target-sym
                               (not (eq target-sym :stay))
                               (not (eq target-sym :back))
                               (not (eq target-sym :logoff))
                               (not (session-authenticated-p *application* *session*)))
                          (let* ((target-name (screen-name-string target-sym))
                                 (target-info (gethash target-name (app-screens)))
                                 (target-info (or target-info
                                                  (when (find-screen-file target-name)
                                                    (load-and-register-screen target-name)))))
                            (if (and target-info
                                     (not (screen-info-anonymous target-info)))
                                (progn
                                  (setf (gethash "errormsg" context)
                                        (anonymous-access-denied-message *application*))
                                  :stay)
                                result))
                          result)))
              (setf no-clear (and (or (eq checked-result :stay)
                                       (null checked-result))
                                   (not (session-property *session* :force-redraw))))
              (setf (session-property *session* :force-redraw) nil)
              ;; Propagate set-cursor from key handler to next :stay iteration
              (when (and no-clear *next-cursor-row*)
                (setf restored-cursor-row *next-cursor-row*
                      restored-cursor-col *next-cursor-col*))
              (multiple-value-bind (transition-result saved-row saved-col)
                  (apply-screen-transition checked-result screen-sym dispatch-sym)
                (when saved-row
                  (setf restored-cursor-row saved-row
                        restored-cursor-col saved-col))
                (when (eq transition-result :exit)
                  (return)))))))))))))

(defun run-application (application conn devinfo &key tls-p)
  "Run an application's main loop for a single connection.
Binds dynamic variables, creates a session, and loops through screens.
TLS-P indicates the connection is TLS-encrypted."
  (let* ((*application* application)
         (*connection* conn)
         (*device-info* devinfo)
         (*session* (make-instance (application-session-class application)
                                   :application application))
         (*field-attribute-overrides* nil)
         (app-package (application-package application)))
    (bt:with-lock-held ((application-sessions-lock application))
      (push *session* (application-sessions application)))
    (setf (session-active-application *session*) application
          (session-tls-p *session*) tls-p)
    (unwind-protect
         (progn
           (setf (session-current-screen *session*)
                 (application-entry-screen application))
           (run-screen-loop app-package))
      (bt:with-lock-held ((application-sessions-lock application))
        (setf (application-sessions application)
              (remove *session* (application-sessions application)))))))

(defun invoke-subapplication (subapp entry-screen)
  "Switch to SUBAPP starting at ENTRY-SCREEN within the current session.
Call from a key handler or screen-update body. Saves the parent application's
screen stack and context, rebinds *application* to SUBAPP, and runs a nested
screen loop. When the subapplication exits, restores parent state.
Returns :stay so the parent screen loop redisplays the current screen.

The current session object remains the same -- its class must be compatible
with the subapplication's expected session class."
  (let ((saved-stack (session-screen-stack *session*))
        (saved-context (session-context *session*))
        (saved-screen (session-current-screen *session*)))
    (load-application-menus subapp)
    (setf (session-active-application *session*) subapp)
    (unwind-protect
         (let ((*application* subapp))
           (setf (session-screen-stack *session*) nil
                 (session-context *session*) (make-hash-table :test 'equal)
                 (session-current-screen *session*) entry-screen)
           (run-screen-loop (application-package subapp)))
      (setf (session-active-application *session*) *application*
            (session-screen-stack *session*) saved-stack
            (session-context *session*) saved-context
            (session-current-screen *session*) saved-screen)))
  ;; Force full erase/write on next display since subapp used different screen layout
  (setf (session-property *session* :force-redraw) t)
  :stay)
