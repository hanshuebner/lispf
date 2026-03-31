;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; key-handler.lisp
;;;;
;;;; Generic key handler dispatch and screen update for lispf applications.
;;;; Provides handle-key, prepare-screen generic functions and
;;;; define-key-handler, define-screen-update macros.

(in-package #:lispf)

;;; Field binding on hash tables (not response objects)

(defmacro with-field-bindings ((hash-table &rest field-names) &body body)
  "Bind FIELD-NAMES as setf-able accessors into a field-values hash table.
Like with-field-values but works on a plain hash table rather than a response.
When a handler does (setf field-name value), it modifies the hash table
that will be used for redisplay."
  `(symbol-macrolet
       (,@(mapcar (lambda (field-name)
                    `(,field-name
                      (gethash ,(string-downcase (symbol-name field-name))
                               ,hash-table)))
                  field-names))
     ,@body))

;;; Generic key dispatch

(defgeneric handle-key (screen-name aid-key)
  (:documentation "Called when AID-KEY is pressed on SCREEN-NAME.
SCREEN-NAME is an EQL-specialized symbol in the application's package.
AID-KEY is an EQL-specialized keyword (:enter, :pf3, etc.).

Return value controls navigation:
  symbol   - go to that screen (pushes current onto stack)
  :logoff  - exit application
  :stay    - redisplay current screen (field values preserved from setf)
  :back    - pop screen stack"))

(defmethod handle-key (screen-name aid-key)
  "Default method: signal an application error for unhandled keys."
  (application-error "Key ~A is not handled on screen ~A"
                     aid-key screen-name))

;;; define-key-handler macro

(defmacro define-key-handler (screen-name aid-key (&rest field-names) &body body)
  "Define a key handler for SCREEN-NAME and AID-KEY.

SCREEN-NAME is a symbol, interned in *package* at macro-expansion time.
This must match the package used in define-application.

FIELD-NAMES are bound as setf-able accessors into the session context.
Values set here persist across screen transitions.

The handler body should return a navigation value (see handle-key):
  'some-screen - navigate to that screen (symbol in app package)
  :stay        - redisplay current screen
  :back        - pop screen stack
  :logoff      - exit application

Example:
  (define-key-handler welcome :enter (username password)
    (unless (authenticate username password)
      (application-error \"Invalid credentials\"))
    'main-menu)"
  ;; Intern screen-name in the current package at expansion time
  (let ((screen-sym (intern (string-upcase (string screen-name)) *package*))
        (aid-kw (intern (string aid-key) :keyword)))
    `(defmethod handle-key ((screen-name (eql ',screen-sym)) (aid-key (eql ,aid-kw)))
       (with-field-bindings ((session-context *session*) ,@field-names)
         ,@body))))

;;; List data getter for repeat field population

(defgeneric get-list-data (screen-name start end)
  (:documentation "Called by the framework to pull structured data for repeat fields.
START and END are 0-based indices for the page window.
Return (values records total-count) where RECORDS is a list of plists
with keys matching repeat field base names, or NIL if this screen
has no list data.")
  (:method (screen-name start end)
    (declare (ignore screen-name start end))
    nil))

(defmacro define-list-data-getter (screen-name (start end) &body body)
  "Define a data getter for repeat field population on SCREEN-NAME.
The framework calls this with START and END indices (0-based).
Body should return (values records total-count) where RECORDS is a list
of plists with keys matching repeat field base names."
  (let ((screen-sym (intern (string-upcase (string screen-name)) *package*)))
    `(defmethod get-list-data ((screen-name (eql ',screen-sym)) ,start ,end)
       (declare (ignorable ,start ,end))
       ,@body)))

;;; Screen lifecycle generic functions

(defgeneric enter-screen (screen-name)
  (:documentation "Called once when first navigating to SCREEN-NAME.
NOT called on :stay redisplays. Use define-screen-enter to define methods.
This is where data loading (DB reads) should happen — field values set here
persist across :stay redisplays without being overwritten.")
  (:method (screen-name)
    (declare (ignore screen-name))
    nil))

(defgeneric prepare-screen (screen-name)
  (:documentation "Called before each display of SCREEN-NAME, including :stay redisplays.
Use define-screen-update to define methods. Within the body, use
show-key/hide-key to control key label visibility, and field bindings
to set display values. Avoid overwriting field values that were set by
enter-screen or by the user.")
  (:method (screen-name)
    (declare (ignore screen-name))
    nil))

(defgeneric leave-screen (screen-name result)
  (:documentation "Called once when navigating away from SCREEN-NAME.
RESULT is the navigation value (:back, screen symbol, :logoff, etc.).
NOT called for :stay. Use define-screen-leave to define methods.
Return value is ignored.")
  (:method (screen-name result)
    (declare (ignore screen-name result))
    nil))

;;; define-screen-enter macro

(defmacro define-screen-enter (screen-name (&rest field-names) &body body)
  "Define an initialization function called once when entering SCREEN-NAME.

SCREEN-NAME is a symbol, interned in *package* at macro-expansion time.
FIELD-NAMES are bound as setf-able accessors into the session context.

Use this for loading data from the database and populating field values.
Values set here persist across :stay redisplays.

Example:
  (define-screen-enter edit-item (name price)
    (let ((item (load-item-from-db)))
      (setf name (item-name item)
            price (item-price item))))"
  (let ((screen-sym (intern (string-upcase (string screen-name)) *package*)))
    `(defmethod enter-screen ((screen-name (eql ',screen-sym)))
       (with-field-bindings ((session-context *session*) ,@field-names)
         ,@body))))

;;; define-screen-update macro

(defmacro define-screen-update (screen-name (&rest field-names) &body body)
  "Define a preparation function called before SCREEN-NAME is displayed.

SCREEN-NAME is a symbol, interned in *package* at macro-expansion time.
FIELD-NAMES are bound as setf-able accessors into the session context.

Within the body, use SHOW-KEY and HIDE-KEY to control which keys
appear in the key label line. The framework handles rendering.

Example:
  (define-screen-update browse (author date)
    (setf author \"John\" date \"2026-01-01\")
    (when has-entries-p (show-key :pf7 \"Prev\")))"
  (let ((screen-sym (intern (string-upcase (string screen-name)) *package*)))
    `(defmethod prepare-screen ((screen-name (eql ',screen-sym)))
       (with-field-bindings ((session-context *session*) ,@field-names)
         ,@body))))

;;; define-screen-leave macro

(defmacro define-screen-leave (screen-name (result &rest field-names) &body body)
  "Define a cleanup function called once when leaving SCREEN-NAME.

SCREEN-NAME is a symbol, interned in *package* at macro-expansion time.
RESULT is bound to the navigation value (:back, screen symbol, :logoff, etc.).
FIELD-NAMES are bound as setf-able accessors into the session context.

Example:
  (define-screen-leave edit-item (result)
    (declare (ignore result))
    (cleanup-temp-data))"
  (let ((screen-sym (intern (string-upcase (string screen-name)) *package*)))
    `(defmethod leave-screen ((screen-name (eql ',screen-sym)) ,result)
       (with-field-bindings ((session-context *session*) ,@field-names)
         ,@body))))

;;; Anonymous access control

(defgeneric session-authenticated-p (application session)
  (:documentation "Return T if SESSION represents an authenticated user.
Override in your application to check login state.")
  (:method ((application t) (session t))
    t))

(defgeneric anonymous-access-denied-message (application)
  (:documentation "Return the error message shown when an anonymous user tries
to access a restricted screen. Override to localize.")
  (:method ((application t))
    (msg "Login required")))

;;; Command field processing

(defgeneric process-screen-command (screen-name command)
  (:documentation "Called when Enter is pressed with a non-empty command field.
Dispatches on the screen name symbol (EQL specialized).
Return a navigation result, or NIL to fall through to process-command.
Use this for screen-specific commands (e.g., editor commands on the edit screen).")
  (:method (screen-name command)
    (declare (ignore screen-name command))
    nil))

(defgeneric process-command (application command)
  (:documentation "Called when Enter is pressed and the 'command' field is non-empty.
Return a navigation result (screen symbol, :stay, :back, :logoff, (:jump . sym)),
or NIL if the command is not recognized.
Default implementation checks menu entries and screen aliases.
The '=' prefix triggers a jump (resets screen stack)."))

(defgeneric unknown-command-message (application command)
  (:documentation "Return the error message for an unrecognized command.")
  (:method ((application t) (command t))
    (msg "~A: unknown command" command)))

(defgeneric invalid-menu-selection-message (application selection)
  (:documentation "Return the error message for an invalid menu selection.")
  (:method ((application t) (selection t))
    (msg "~A: invalid selection" selection)))

;;; Command label customization

(defgeneric default-command-label (application)
  (:documentation "Return the default command line label for screens.
Override to localize, e.g. return \"Kommando ==>\" for German.")
  (:method ((application t))
    (msg "Command ==>")))

(defgeneric menu-command-label (application)
  (:documentation "Return the command line label for menu screens.
Override to localize, e.g. return \"Auswahl ==>\" for German.")
  (:method ((application t))
    (msg "Option  ==>")))

(defgeneric paging-labels (application)
  (:documentation "Return (values prev-label next-label) for list paging keys.
Override to localize.")
  (:method ((application t))
    (values (msg "Prev") (msg "Next"))))

(defgeneric menu-key-labels (application &optional menu-name)
  (:documentation "Return (values enter-label pf3-label) for auto-generated menu screens.
MENU-NAME is the string name of the menu being generated.
Override to localize or to return different labels per menu.")
  (:method ((application t) &optional menu-name)
    (declare (ignore menu-name))
    (values (msg "Select") (msg "Exit"))))

;;; Role-based access control

(defgeneric session-user-roles (application session)
  (:documentation "Return a list of role strings for the current session user.")
  (:method ((application t) (session t))
    nil))

(defgeneric role-access-denied-message (application)
  (:documentation "Return the error message shown when a user lacks required roles.")
  (:method ((application t))
    "Permission denied"))

;;; Screen transition hook

(defgeneric check-screen-transition (application result)
  (:documentation "Called before a screen transition is applied. RESULT is the
navigation value (:back, :logoff, a screen symbol, or (:jump . screen)).
Return RESULT to allow the transition, or :stay to block it.
Default method passes through unchanged.")
  (:method ((application t) result)
    result))

;;; Update cycle hook

(defgeneric update-cycle-hook (application)
  (:documentation "Called by the update thread each iteration, with *session* and
*connection* bound. Use for application-specific background work such as
delivering notifications. Default method does nothing.")
  (:method (application)
    (declare (ignore application))
    nil))

;;; Message line (row 22)
;;;
;;; The message line supports three types with different colors and timeouts:
;;;   :error        — red, no timeout (persists until next interaction)
;;;   :confirmation — yellow, default 15s timeout
;;;   :notification — yellow, default 60s timeout
;;; Use set-message to display a message. application-error sets :error type.

(defun set-message (type format-string &rest format-args)
  "Display a message on the message line.
TYPE is :error (red, permanent), :confirmation (yellow, 15s), or :notification (yellow, 60s).
FORMAT-STRING and FORMAT-ARGS are passed to FORMAT to produce the message text.
If no FORMAT-ARGS, FORMAT-STRING is used as-is."
  (let ((text (if format-args
                  (apply #'format nil format-string format-args)
                  format-string))
        (timeout (case type
                   (:confirmation 15)
                   (:notification 60)
                   (otherwise nil))))
    (setf (session-property *session* :message-line)
          (list :text text :type type :timeout timeout
                :timestamp (get-universal-time)))))

(defun clear-message ()
  "Clear the message line."
  (setf (session-property *session* :message-line) nil))

(defgeneric message-cleared (application message)
  (:documentation "Called when a message is cleared by user interaction.
MESSAGE is the message plist that was cleared. Applications can use this
to perform cleanup, e.g. marking notifications as read.")
  (:method (application message)
    (declare (ignore application message))
    nil))

(defun message-expired-p (msg)
  "Return T if MSG (a message-line plist) has exceeded its timeout."
  (let ((timeout (getf msg :timeout))
        (timestamp (getf msg :timestamp)))
    (and timeout timestamp
         (> (- (get-universal-time) timestamp) timeout))))

;;; Field attribute overrides

(defun set-field-attribute (field-name &rest attrs)
  "Override display attributes for FIELD-NAME on the next render.
Call from within a define-screen-update body.
Supported attributes: :write, :intense, :hidden, :color, :highlighting.
Example: (set-field-attribute \"author\" :write nil :intense t)"
  (let ((name (string-downcase (string field-name))))
    (push (cons name attrs) *field-attribute-overrides*)))

;;; Dynamic area updaters

(defgeneric update-dynamic-area (screen-name area-name)
  (:documentation "Return content for AREA-NAME on SCREEN-NAME as a list of entries (one per row).
Each entry may be a string or a plist (:content STRING :color C :highlighting H :intense T).
Called periodically by the background update thread. Return NIL to skip the update.")
  (:method (screen-name area-name)
    (declare (ignore screen-name area-name))
    nil))

(defmacro define-dynamic-area-updater (screen-name area-name (&rest bindings) &body body)
  "Define an updater for a dynamic area, called periodically by the update thread.
AREA-NAME matches the :name in the screen's :dynamic-areas definition.
Return a list of entries (one per row), or NIL to skip the update.
Each entry may be a string or a plist with :content and field attributes:
  (:content \"text\" :color cl3270:+red+ :highlighting cl3270:+underscore+)"
  (let ((screen-sym (intern (string-upcase (string screen-name)) *package*))
        (area-sym (intern (string-upcase (string area-name)) *package*)))
    `(defmethod update-dynamic-area ((screen-name (eql ',screen-sym))
                                     (area-name (eql ',area-sym)))
       (with-field-bindings ((session-context *session*) ,@bindings)
         ,@body))))
