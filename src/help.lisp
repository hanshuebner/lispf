;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; help.lisp
;;;;
;;;; Help subsystem for lispf applications.
;;;; PF1 navigates to the help for the current screen using .help files
;;;; displayed in the help viewer.

(in-package #:lispf)

;;; Help navigation

(defun navigate-to-help (screen-sym)
  "Navigate to the help for SCREEN-SYM.
Returns :stay if a .help file was found and the help viewer invoked, or NIL."
  (let ((name (screen-name-string screen-sym)))
    (when (find-help-file name)
      (show-help name))))

;;; Default PF1 handler: show help

(defmethod handle-key (screen-name (aid-key (eql :pf1)))
  "Default PF1 handler: navigate to help for the current screen."
  (let ((result (navigate-to-help screen-name)))
    (if result
        result
        (progn
          (setf (gethash "errormsg" (session-context *session*))
                (msg "No help available"))
          :stay))))
