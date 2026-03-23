;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; help.lisp
;;;;
;;;; Help subsystem for lispf applications.
;;;; Supports two formats:
;;;;   1. .help files (hypertext help viewer with links and scrolling)
;;;;   2. .screen files following the naming convention help-<name> (legacy)
;;;; PF1 navigates to the help for the current screen, preferring .help files.
;;;; The command field in legacy help screens navigates to named help topics.

(in-package #:lispf)

;;; Help screen resolution

(defun help-screen-name (screen-name)
  "Return the help screen name for SCREEN-NAME.
Convention: help screen for 'edit' is 'help-edit'."
  (format nil "help-~A" (screen-name-string screen-name)))

(defun find-help-screen (screen-name)
  "Check if a help screen exists for SCREEN-NAME.
Returns the help screen name string if found, NIL otherwise."
  (let ((help-name (help-screen-name screen-name)))
    (when (find-screen-file help-name)
      help-name)))

(defun help-screen-p (screen-name)
  "Return T if SCREEN-NAME is a help screen (name starts with 'help-')."
  (let ((name (screen-name-string screen-name)))
    (and (>= (length name) 5)
         (string= "help-" name :end2 5))))

;;; Help navigation

(defun navigate-to-help (screen-sym app-package)
  "Navigate to the help for SCREEN-SYM.
Prefers .help files (invokes help viewer) over legacy .screen help.
Returns the help screen symbol for legacy screens, :stay for help viewer,
or NIL if no help is available."
  (let ((name (screen-name-string screen-sym)))
    ;; Try .help file first
    (when (find-help-file name)
      (return-from navigate-to-help (show-help name)))
    ;; Fall back to legacy .screen help
    (let ((help-name (find-help-screen screen-sym)))
      (when help-name
        (intern-screen-name help-name app-package)))))

;;; Default PF1 handler: show help

(defmethod handle-key (screen-name (aid-key (eql :pf1)))
  "Default PF1 handler: navigate to help for the current screen.
Prefers .help files over legacy .screen help."
  (let ((result (navigate-to-help screen-name
                                   (application-package *application*))))
    (if result
        result
        (progn
          (setf (gethash "errormsg" (session-context *session*))
                (msg "No help available"))
          :stay))))

;;; Help topic navigation via command field (legacy .screen help)

(defmethod process-screen-command :around (screen-name (command string))
  "On help screens, treat the command field as a help topic navigator."
  (if (help-screen-p screen-name)
      (let* ((topic (string-trim '(#\Space) command))
             (topic-name (format nil "help-~A" (string-downcase topic))))
        (if (find-screen-file topic-name)
            (intern-screen-name topic-name
                                (application-package *application*))
            (progn
              (setf (gethash "errormsg" (session-context *session*))
                    (msg "~A: help topic not found" topic))
              :stay)))
      (call-next-method)))
