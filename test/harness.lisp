;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; harness.lisp
;;;;
;;;; App lifecycle management for running LISPF applications in tests.
;;;; Starts the app in a background thread and provides clean shutdown.

(in-package #:lispf-test)

(defvar *test-app-port* nil
  "Port the test application is listening on. Set by with-test-app.")

(defmacro with-test-app ((session-var app &key (port 0) (host "127.0.0.1"))
                          &body body)
  "Run BODY with a test application and s3270 session.
Starts APP in a background thread on PORT (default 0 for auto-assign),
launches s3270, connects, and binds SESSION-VAR to the s3270-session.
Cleans up on exit."
  (let ((listener-var (gensym "LISTENER"))
        (ready-lock (gensym "LOCK"))
        (ready-cv (gensym "CV"))
        (ready-flag (gensym "READY"))
        (app-thread (gensym "THREAD"))
        (s3270-var (gensym "S3270"))
        (port-var (gensym "PORT"))
        (actual-port-var (gensym "ACTUAL-PORT"))
        (host-var (gensym "HOST")))
    `(let* ((,listener-var nil)
            (,ready-lock (bt:make-lock "test-app-ready"))
            (,ready-cv (bt:make-condition-variable :name "test-app-ready"))
            (,ready-flag nil)
            (,port-var ,port)
            (,actual-port-var nil)
            (,host-var ,host)
            (,app-thread
              (bt:make-thread
               (lambda ()
                 (let ((*standard-output* (make-broadcast-stream))
                       (*error-output* (make-broadcast-stream))
                       (*trace-output* (make-broadcast-stream)))
                   (handler-case
                       (lispf:start-application
                        ,app
                        :port ,port-var
                        :host ,host-var
                        :listener-callback
                        (lambda (listener)
                          (setf ,listener-var listener
                                ,actual-port-var (usocket:get-local-port listener))
                          (bt:with-lock-held (,ready-lock)
                            (setf ,ready-flag t)
                            (bt:condition-notify ,ready-cv))))
                     (error () nil))))
               :name "test-app"))
            (,s3270-var nil))
       ;; Wait for the listener to be ready
       (bt:with-lock-held (,ready-lock)
         (loop until ,ready-flag
               do (bt:condition-wait ,ready-cv ,ready-lock)))
       (unwind-protect
            (let ((*test-app-port* ,actual-port-var))
              (setf ,s3270-var (launch-s3270))
              (s3270-connect ,s3270-var ,host-var ,actual-port-var)
              (let ((,session-var ,s3270-var))
                ,@body))
         ;; Cleanup: disconnect clients, close listener, wait for sessions to drain
         (when ,s3270-var
           (ignore-errors (s3270-disconnect ,s3270-var))
           (ignore-errors (close-s3270 ,s3270-var)))
         (when ,listener-var
           (ignore-errors (usocket:socket-close ,listener-var)))
         ;; Wait for connection threads to exit (they remove themselves from the list)
         (loop with deadline = (+ (get-internal-real-time)
                                  (* 5 internal-time-units-per-second))
               while (and (bt:with-lock-held ((lispf::application-connections-lock ,app))
                            (lispf::application-connections ,app))
                          (< (get-internal-real-time) deadline))
               do (bt:thread-yield))
         (let ((remaining (bt:with-lock-held ((lispf::application-connections-lock ,app))
                            (lispf::application-connections ,app))))
           (when remaining
             (error "~D connection thread~:P still alive after test cleanup"
                    (length remaining))))
         (when (bt:thread-alive-p ,app-thread)
           (ignore-errors (bt:destroy-thread ,app-thread)))))))
