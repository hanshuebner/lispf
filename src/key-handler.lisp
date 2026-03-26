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

;;; Screen preparation generic function

(defgeneric prepare-screen (screen-name)
  (:documentation "Called before each display of SCREEN-NAME.
Use define-screen-update to define methods. Within the body, use
show-key/hide-key to control key label visibility, and field bindings
to set display values.")
  (:method (screen-name)
    (declare (ignore screen-name))
    nil))

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
