;;; -*- Mode: Lisp -*-

;;; Example guestbook application using the lispf application framework.
;;; Screens are loaded from screens/*.screen files.
;;;
;;; Usage:
;;;   (asdf:load-system "lispf")
;;;   (load ".../examples/guestbook/guestbook.lisp")
;;;   (lispf-guestbook:start)
;;;
;;; Then connect with a 3270 terminal emulator (e.g. wx3270) on port 3270.

(defpackage #:lispf-guestbook
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:lspf #:lispf))
  (:export #:start))

(in-package #:lispf-guestbook)

;;; Guestbook data

(defvar *guestbook-entries* '()
  "List of guestbook entries, newest first. Each entry is a plist
with :name, :message, and :date.")

(defvar *guestbook-lock* (bt:make-lock "guestbook"))

;;; Session class

(defclass guestbook-session (lspf:session)
  ((browse-index :initform 0 :accessor browse-index)
   (login-time :initform (get-universal-time) :reader session-login-time)
   (entries-written :initform 0 :accessor session-entries-written)))

;;; Utility

(defun format-duration (seconds)
  "Format a duration in seconds as a human-readable string."
  (let ((hours (floor seconds 3600))
        (minutes (floor (mod seconds 3600) 60))
        (secs (mod seconds 60)))
    (cond
      ((>= hours 1) (format nil "~D hour~:P, ~D minute~:P" hours minutes))
      ((>= minutes 1) (format nil "~D minute~:P, ~D second~:P" minutes secs))
      (t (format nil "~D second~:P" secs)))))

;;; Application definition

(lspf:define-application *guestbook-app*
  :entry-screen welcome
  :screen-directory (merge-pathnames
                     #P"examples/guestbook/screens/"
                     (asdf:system-source-directory :lispf))
  :session-class 'guestbook-session)

;;; Indicator: show entry count in title bar

(defun entry-count-text ()
  (format nil "~D entr~:@P" (length *guestbook-entries*)))

(lspf:define-screen-update welcome ()
  (lspf:set-indicator "entries" (entry-count-text)))

;;; Entry list screen

(lspf:define-list-data-getter entry-list (start end)
  (let* ((entries *guestbook-entries*)
         (total (length entries))
         (page (subseq entries start (min end total))))
    (values (loop for e in page
                  collect (list :author (getf e :name)
                                :date (getf e :date)
                                :preview (substitute #\Space #\Newline
                                                     (getf e :message))))
            total)))

(lspf:define-screen-update entry-list ()
  (lspf:set-indicator "entries" (entry-count-text))
  (lspf:clear-indicator "new-data"))

(lspf:define-key-handler entry-list :enter ()
  (let ((index (lspf:selected-list-index)))
    (when index
      (setf (browse-index lspf:*session*) index)
      'browse)))

;;; Browse screen

(lspf:define-screen-update browse (author date message entry-counter)
  (let ((index (browse-index lspf:*session*))
        (entry-count (length *guestbook-entries*)))
    (let ((entry (nth index *guestbook-entries*)))
      (setf author (getf entry :name)
            date (getf entry :date)
            message (getf entry :message)
            entry-counter (format nil "Entry ~D of ~D" (1+ index) entry-count)))
    (when (> index 0)
      (lspf:show-key :pf7 "Prev"))
    (when (< index (1- entry-count))
      (lspf:show-key :pf8 "Next"))))

(lspf:define-key-handler browse :pf7 ()
  (when (> (browse-index lspf:*session*) 0)
    (decf (browse-index lspf:*session*)))
  :stay)

(lspf:define-key-handler browse :pf8 ()
  (when (< (browse-index lspf:*session*) (1- (max 1 (length *guestbook-entries*))))
    (incf (browse-index lspf:*session*)))
  :stay)

;;; New entry screen

(lspf:define-key-handler new-entry :pf5 (name message)
  (bt:with-lock-held (*guestbook-lock*)
    (push (list :name name
                :message message
                :date (concatenate 'string (cl3270:today-date) " " (cl3270:now-time)))
          *guestbook-entries*))
  (incf (session-entries-written lspf:*session*))
  (lspf:broadcast (lambda ()
                    (lspf:set-indicator "entries" (entry-count-text))
                    (lspf:set-indicator "new-data" "NEW")))
  (setf (browse-index lspf:*session*) 0)
  (setf name "" message "")
  :back)

;;; Bye screen

(lspf:define-screen-update bye (session-time entries-written)
  (let ((elapsed (- (get-universal-time) (session-login-time lspf:*session*))))
    (setf session-time (format-duration elapsed)
          entries-written (format nil "~D" (session-entries-written lspf:*session*)))))

(lspf:define-key-handler bye :enter ()
  :logoff)

;;; Server entry point

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the guestbook application on PORT."
  (lspf:start-application *guestbook-app* :port port :host host))
