;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; key-layout-tests.lisp
;;;;
;;;; Unit tests for fixed-position key label layout.

(defpackage #:lispf-key-layout-tests
  (:use #:cl #:lispf-test)
  (:export #:run-all))

(in-package #:lispf-key-layout-tests)

;;; Helpers

(defun layout-for (&rest specs)
  "Compute key layout from specs."
  (lispf::compute-key-layout specs))

(defun format-labels (specs layout)
  "Format key labels with fixed positions, return trimmed result."
  (string-right-trim '(#\Space)
                     (or (lispf::format-key-labels-from-specs specs layout) "")))

;;; Tests

(define-test layout-assigns-sequential-positions ()
  (let ((layout (layout-for '(:PF5 "Add") '(:ENTER "View") '(:PF3 "Exit"))))
    ;; PF5 at col 1, width 7 ("PF5 Add")
    (assert (equal '(:PF5 1 7) (assoc :PF5 layout)))
    ;; ENTER at col 10 (1 + 7 + 2), width 10 ("ENTER View")
    (assert (equal '(:ENTER 10 10) (assoc :ENTER layout)))
    ;; PF3 at col 22 (10 + 10 + 2), width 8 ("PF3 Exit")
    (assert (equal '(:PF3 22 8) (assoc :PF3 layout)))))

(define-test hidden-keys-reserve-slot ()
  (let ((with-hidden (layout-for '(:PF7 "Prev" :hidden t) '(:PF5 "Add")))
        (with-visible (layout-for '(:PF7 "Prev") '(:PF5 "Add"))))
    ;; Same positions whether key is :hidden or not
    (assert (equal with-hidden with-visible))))

(define-test empty-label-without-hidden-gets-no-slot ()
  (let ((layout (layout-for '(:PF7 "") '(:PF5 "Add"))))
    (assert (null (assoc :PF7 layout)))
    (assert (equal '(:PF5 1 7) (assoc :PF5 layout)))))

(define-test all-keys-shown-at-fixed-positions ()
  (let ((layout (layout-for '(:PF7 "Prev") '(:PF8 "Next") '(:PF3 "Exit"))))
    (let ((result (format-labels '((:PF7 "Prev") (:PF8 "Next") (:PF3 "Exit"))
                                 layout)))
      (assert (search "PF7 Prev" result))
      (assert (search "PF8 Next" result))
      (assert (search "PF3 Exit" result)))))

(define-test hidden-keys-leave-blank-space ()
  (let ((layout (layout-for '(:PF7 "Prev" :hidden t) '(:PF8 "Next" :hidden t)
                             '(:PF5 "Add"))))
    (let ((all-shown (format-labels '((:PF7 "Prev") (:PF8 "Next") (:PF5 "Add"))
                                    layout))
          (pf7-pf8-hidden (format-labels '((:PF7 "") (:PF8 "") (:PF5 "Add"))
                                         layout)))
      ;; PF5 starts at same position in both cases
      (let ((pos-all (search "PF5 Add" all-shown))
            (pos-hidden (search "PF5 Add" pf7-pf8-hidden)))
        (assert pos-all)
        (assert pos-hidden)
        (assert (= pos-all pos-hidden))))))

(define-test only-some-keys-hidden ()
  (let ((layout (layout-for '(:PF7 "Prev") '(:PF8 "Next") '(:PF3 "Exit"))))
    (let ((result (format-labels '((:PF7 "Prev") (:PF8 "") (:PF3 "Exit"))
                                 layout)))
      ;; PF7 shown, PF8 blank, PF3 at original position
      (assert (search "PF7 Prev" result))
      (assert (not (search "PF8" result)))
      (let ((pf3-pos (search "PF3 Exit" result))
            (expected-pos (second (assoc :PF3 layout))))
        (assert (= pf3-pos expected-pos))))))

(define-test unknown-key-appended-at-end ()
  (let ((layout (layout-for '(:PF5 "Add") '(:PF3 "Exit"))))
    (let ((result (format-labels '((:PF5 "Add") (:PF3 "Exit") (:PF9 "Extra"))
                                 layout)))
      ;; PF9 appears after PF3
      (let ((pf3-pos (search "PF3 Exit" result))
            (pf9-pos (search "PF9 Extra" result)))
        (assert pf3-pos)
        (assert pf9-pos)
        (assert (> pf9-pos pf3-pos))))))

(define-test no-keys-returns-nil ()
  (assert (null (lispf::format-key-labels-from-specs nil nil))))

(define-test all-hidden-returns-nil ()
  (let ((layout (layout-for '(:PF7 "Prev" :hidden t))))
    (assert (null (lispf::format-key-labels-from-specs '((:PF7 "")) layout)))))

;;; Runner

(defun run-all ()
  "Run all key layout tests."
  (format t "~&;;; Running key layout tests~%")
  (run-tests 'layout-assigns-sequential-positions
             'hidden-keys-reserve-slot
             'empty-label-without-hidden-gets-no-slot
             'all-keys-shown-at-fixed-positions
             'hidden-keys-leave-blank-space
             'only-some-keys-hidden
             'unknown-key-appended-at-end
             'no-keys-returns-nil
             'all-hidden-returns-nil))
