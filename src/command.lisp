;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; command.lisp
;;;;
;;;; Command registry for lispf applications.
;;;; Commands are registered with define-command and dispatched by process-command.

(in-package #:lispf)

;;; Command info

(defclass command-info ()
  ((name :initarg :name :reader command-info-name
         :documentation "Primary command name (lowercase string).")
   (aliases :initarg :aliases :initform nil :reader command-info-aliases
            :documentation "List of alias strings (lowercase).")
   (doc :initarg :doc :initform nil :reader command-info-doc
        :documentation "One-liner help string.")
   (handler :initarg :handler :reader command-info-handler
            :documentation "Function of one argument (full command string), returns navigation result.")))

(defun register-command (app name aliases doc handler)
  "Register a command with NAME and ALIASES on application APP.
All names are stored lowercase in the commands hash table."
  (let* ((commands (application-commands app))
         (info (make-instance 'command-info
                              :name name
                              :aliases aliases
                              :doc doc
                              :handler handler)))
    (setf (gethash name commands) info)
    (dolist (alias aliases)
      (setf (gethash alias commands) info))
    info))

(defmacro define-command (app-var name-spec (&key aliases doc) &body body)
  "Define a command on APP-VAR. NAME-SPEC is either a bare symbol for the
command name, or (name var) to bind the full command string to VAR in BODY.
ALIASES are bare symbols. DOC is a one-liner help string.
BODY returns a navigation result."
  (let* ((name-sym (if (listp name-spec) (first name-spec) name-spec))
         (cmd-var (when (listp name-spec) (second name-spec)))
         (name (string-downcase (symbol-name name-sym)))
         (alias-strings (mapcar (lambda (a) (string-downcase (symbol-name a))) aliases)))
    (if cmd-var
        `(register-command ,app-var ,name ',alias-strings ,doc
                           (lambda (,cmd-var) ,@body))
        (let ((ignored (gensym "COMMAND")))
          `(register-command ,app-var ,name ',alias-strings ,doc
                             (lambda (,ignored)
                               (declare (ignore ,ignored))
                               ,@body))))))

;;; Command lookup

(defun find-command (app command)
  "Look up COMMAND in the application's command registry.
Tries exact match first, then first-word match for commands with arguments.
Returns the handler's result, or NIL if no match."
  (let* ((commands (application-commands app))
         (key (string-downcase command))
         (info (gethash key commands)))
    (when info
      (return-from find-command (funcall (command-info-handler info) command)))
    ;; Try first word
    (let ((space (position #\Space key)))
      (when space
        (let ((first-word (subseq key 0 space)))
          (when-let (info (gethash first-word commands))
            (return-from find-command
              (funcall (command-info-handler info) command))))))))

;;; Documentation lookup

(defun find-command-doc (app command)
  "Look up documentation for COMMAND. Checks the command registry first,
then derives a description from menu data for navigable screen aliases.
Returns a description string or NIL."
  (let* ((commands (application-commands app))
         (key (string-downcase command))
         (info (gethash key commands)))
    (when info
      (return-from find-command-doc (command-info-doc info))))
  ;; Try screen alias with menu-derived doc
  (when-let (screen-sym (find-screen-alias app command))
    (build-menu-doc app screen-sym)))

(defun build-menu-doc (app screen-sym)
  "Derive a one-liner description for SCREEN-SYM from loaded menu data.
Returns a string like \"Classic Computing / Exponate - Ausgestellte Exponate\"
or NIL if the screen is not found in any menu."
  (let ((screen-name (string-downcase (symbol-name screen-sym))))
    (maphash (lambda (menu-name menu-data)
               (dolist (item (getf menu-data :items))
                 (when (string-equal screen-name
                                     (symbol-name (getf item :screen)))
                   (let ((menu-title (getf menu-data :title))
                         (label (getf item :label))
                         (desc (getf item :description)))
                     (return-from build-menu-doc
                       (if (string-equal menu-name "main")
                           (format nil "~A - ~A" label desc)
                           (format nil "~A / ~A - ~A"
                                   menu-title label desc)))))))
             (application-menus app))
    nil))

;;; Command collection for browsable list

(defun collect-all-commands (app)
  "Gather all known commands with documentation, sorted alphabetically.
Returns a list of (name doc &optional aliases) entries."
  (let ((entries nil)
        (seen (make-hash-table :test 'equal)))
    ;; Registered commands (skip aliases, group under primary name)
    (maphash (lambda (key info)
               (declare (ignore key))
               (unless (gethash (command-info-name info) seen)
                 (setf (gethash (command-info-name info) seen) t)
                 (push (list (command-info-name info)
                             (command-info-doc info)
                             (command-info-aliases info))
                       entries)))
             (application-commands app))
    ;; Navigable screens not already covered by registered commands
    (maphash (lambda (name-string info)
               (when (and (screen-info-navigable info)
                          (not (gethash name-string seen)))
                 (setf (gethash name-string seen) t)
                 (let ((doc (build-menu-doc app
                              (intern (string-upcase name-string)
                                      (application-package app))))
                       (aliases (screen-info-aliases info)))
                   (when doc
                     (push (list name-string doc
                                 (remove-if (lambda (a) (gethash a seen)) aliases))
                           entries))
                   ;; Mark aliases as seen
                   (dolist (alias aliases)
                     (setf (gethash alias seen) t)))))
             (application-screens app))
    (sort entries #'string< :key #'first)))
