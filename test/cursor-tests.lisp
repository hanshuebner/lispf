;;; -*- Mode: Lisp -*-

;;; E2E tests for set-cursor in key handlers.
;;; Tests that set-cursor called from a key handler is honored on the
;;; next :stay redisplay.

(defpackage #:lispf-cursor-tests
  (:use #:cl #:lispf-test)
  (:export #:run-all))

(in-package #:lispf-cursor-tests)

;;; Minimal test application with a screen that has a multi-row input field.
;;; Enter handler advances cursor to the next row via set-cursor.

(lispf:define-application *cursor-test-app*
  :title "CURSOR-TEST"
  :entry-screen cursor-form
  :screen-directory (merge-pathnames
                     #P"test/screens/"
                     (asdf:system-source-directory :lispf))
  :session-class 'lispf:session)

(lispf:define-key-handler cursor-form :enter ()
  (let ((next-row (min (1+ (lispf:cursor-row)) 20)))
    (lispf:set-cursor next-row 0))
  :stay)

(lispf:define-key-handler cursor-form :pf3 ()
  :logoff)

;;; Test

(define-test e2e-set-cursor-from-handler ()
  (with-test-app (s *cursor-test-app* :port 13282)
    (assert-on-screen s "CURSOR-FORM")
    ;; Initial cursor should be on the first writable field.
    ;; Field :from (1 0) in app space = screen row 2 (after title row offset).
    ;; Attribute byte at (1,79), content starts at (2,0).
    (assert-cursor-at s 2 0 :description "Initial cursor on first input field")
    ;; Press Enter — handler sees cursor at row 2, advances to row 3
    (press-enter s)
    (assert-cursor-at s 3 0 :description "Cursor advanced to row 3 after Enter")
    ;; Press Enter again — should advance to row 4
    (press-enter s)
    (assert-cursor-at s 4 0 :description "Cursor advanced to row 4 after second Enter")
    ;; Exit
    (press-pf s 3)))

(defun run-all ()
  (let ((*package* (find-package :lispf-cursor-tests)))
    (run-tests)))
