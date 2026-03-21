;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; help.lisp
;;;;
;;;; Help subsystem for lispf applications.
;;;; Help screens are .screen files with :help t property.
;;;; PF1 navigates to the help screen for the current screen.
;;;; The command field in help screens navigates to named help topics.

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

;;; Help navigation

(defun navigate-to-help (screen-sym app-package)
  "Navigate to the help screen for SCREEN-SYM.
Returns the help screen symbol if a help screen exists, NIL otherwise."
  (let ((help-name (find-help-screen screen-sym)))
    (when help-name
      (intern-screen-name help-name app-package))))

;;; Default PF1 handler: show help

(defmethod handle-key (screen-name (aid-key (eql :pf1)))
  "Default PF1 handler: navigate to help screen for the current screen."
  (let* ((help-sym (navigate-to-help screen-name
                                      (application-package *application*))))
    (if help-sym
        help-sym
        (progn
          (setf (gethash "errormsg" *current-field-values*)
                (msg "No help available"))
          :stay))))

;;; Help topics via command field on help screens

(defun help-screen-p (screen-name)
  "Return T if SCREEN-NAME is a help screen (name starts with 'help-')."
  (let ((name (screen-name-string screen-name)))
    (and (>= (length name) 5)
         (string= "help-" name :end2 5))))

;; Process commands on help screens: typing a topic name navigates there
(defmethod handle-key :around (screen-name (aid-key (eql :enter)))
  "On help screens, treat the command field as a help topic navigator."
  (if (help-screen-p screen-name)
      (let* ((command (string-trim '(#\Space)
                                    (or (gethash "command" *current-field-values*) ""))))
        (if (plusp (length command))
            ;; Try to navigate to help-<topic>
            (let ((topic-name (format nil "help-~A" (string-downcase command))))
              (if (find-screen-file topic-name)
                  (intern-screen-name topic-name
                                      (application-package *application*))
                  (progn
                    (setf (gethash "errormsg" *current-field-values*)
                          (msg "~A: help topic not found" command))
                    :stay)))
            (call-next-method)))
      (call-next-method)))
