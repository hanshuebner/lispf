;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; guestbook-tests.lisp
;;;;
;;;; End-to-end tests for the guestbook example application.

(defpackage #:lispf-guestbook-tests
  (:use #:cl #:lispf-test))

(in-package #:lispf-guestbook-tests)

;;; Screen data (loaded once for field-aware operations)

(defvar *screen-dir*
  (merge-pathnames #P"screens/"
                   (asdf:system-source-directory :lispf-guestbook)))

(defun screen-path (name)
  (merge-pathnames (make-pathname :name name :type "screen") *screen-dir*))

(defvar *new-entry-screen* nil)

(defun load-screen-data-files ()
  (setf *new-entry-screen* (load-test-screen-data (screen-path "new-entry"))))

(define-suite-fixtures
  (:name :screen-data
   :scope :suite
   :setup (load-screen-data-files)))

;;; Test utilities

(defmacro with-guestbook-entries (entries &body body)
  "Run BODY with *guestbook-entries* set to ENTRIES, restoring the original value on exit."
  (let ((saved (gensym "SAVED")))
    `(let ((,saved lispf-guestbook::*guestbook-entries*))
       (unwind-protect
            (progn
              (setf lispf-guestbook::*guestbook-entries* ,entries)
              ,@body)
         (setf lispf-guestbook::*guestbook-entries* ,saved)))))

;;; Tests

(define-test welcome-screen-displays ()
  (with-test-app (s lispf-guestbook::*guestbook-app*)
    (assert-on-screen s "WELCOME")
    (assert-screen-contains s "ENTER Continue")))

(define-test enter-on-welcome-goes-to-empty-list ()
  (with-guestbook-entries '()
    (with-test-app (s lispf-guestbook::*guestbook-app*)
      (assert-on-screen s "WELCOME")
      (press-enter s)
      (assert-on-screen s "ENTRY-LIST"))))

(define-test add-entry-and-verify-list ()
  (with-guestbook-entries
      '((:name "Seed" :message "seed" :date "2026-01-01 00:00:00"))
    (with-test-app (s lispf-guestbook::*guestbook-app*)
      ;; Welcome -> entry-list (has entries)
      (press-enter s)
      (assert-on-screen s "ENTRY-LIST")
      ;; PF5 -> new-entry
      (press-pf s 5)
      (assert-on-screen s "NEW-ENTRY")
      ;; Fill in the form
      (type-in-field s *new-entry-screen* "name" "Test User")
      (type-in-field s *new-entry-screen* "message" "Hello from the test suite!")
      ;; PF5 saves and goes :back to entry-list
      (press-pf s 5)
      (assert-on-screen s "ENTRY-LIST")
      ;; Verify both entries appear
      (assert-screen-contains s "Test User")
      (assert-screen-contains s "Seed"))))

(define-test pf3-exits-to-bye ()
  (with-guestbook-entries '()
    (with-test-app (s lispf-guestbook::*guestbook-app*)
      (assert-on-screen s "WELCOME")
      (press-enter s)
      (assert-on-screen s "ENTRY-LIST")
      ;; PF3 on entry-list goes to bye
      (press-pf s 3)
      (assert-on-screen s "BYE")
      (assert-screen-contains s "Thank you"))))

