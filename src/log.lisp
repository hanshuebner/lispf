;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; log.lisp
;;;;
;;;; Logging facility for LISPF.

(in-package #:lispf)

;;; Log levels

(deftype log-level ()
  '(member :debug :info :warn :error))

(defparameter *log-level-priority*
  '(:debug 0 :info 1 :warn 2 :error 3))

(defvar *log-level* :info
  "Minimum log level. Messages below this level are suppressed.")

;;; Log consumers

(defvar *log-consumers* (list *error-output*)
  "List of log consumers. Each consumer is either a stream or a function
accepting (level connection-id format-string format-args).")

(defun add-log-consumer (consumer)
  "Add a log consumer (stream or function)."
  (pushnew consumer *log-consumers*))

(defun remove-log-consumer (consumer)
  "Remove a log consumer."
  (setf *log-consumers* (remove consumer *log-consumers*)))

;;; Connection ID

(defvar *connection-id-counter* 0
  "Counter for generating connection IDs.")

(defvar *connection-id-lock* (bt:make-lock "connection-id")
  "Lock for thread-safe connection ID generation.")

(defvar *connection-id* nil
  "Connection ID for the current connection thread.
Bound in handle-connection as a fallback for log messages emitted
before or after the session exists (connect/disconnect).")

(defun next-connection-id ()
  "Generate the next unique connection ID."
  (bt:with-lock-held (*connection-id-lock*)
    (incf *connection-id-counter*)))

;;; Core logging

(defun log-message (level fmt &rest args)
  "Log a message at LEVEL with format string FMT and ARGS.
The message is prefixed with a timestamp, level, application name,
and connection ID."
  (declare (type log-level level))
  (when (>= (getf *log-level-priority* level 0)
            (getf *log-level-priority* *log-level* 0))
    (let* ((conn-id (or (when (and (boundp '*session*) *session*)
                          (let ((conn (session-connection *session*)))
                            (when (and conn (slot-boundp conn 'id))
                              (connection-id conn))))
                        (when (boundp '*connection-id*) *connection-id*)))
           (app-name (if (and (boundp '*application*) *application*)
                         (application-name *application*)
                         "lispf"))
           (prefix (if conn-id
                       (format nil "~A[~D]" app-name conn-id)
                       app-name)))
      (multiple-value-bind (sec min hour day month year)
          (decode-universal-time (get-universal-time))
        (dolist (consumer *log-consumers*)
          (let ((line (format nil "~4,'0D-~2,'0D-~2,'0D ~2,'0D:~2,'0D:~2,'0D [~A] ~A: ~?"
                             year month day hour min sec
                             (string-upcase (symbol-name level))
                             prefix fmt args)))
            (if (functionp consumer)
                (ignore-errors (funcall consumer level conn-id fmt args))
                (ignore-errors
                  (format consumer "~&;;; ~A~%" line)
                  (force-output consumer)))))))))

;;;; end of file -- log.lisp
