;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; conditions.lisp
;;;;
;;;; Error handling for lispf applications.
;;;; Provides application-error condition for user-facing errors
;;;; and incident ID generation for unexpected errors.

(in-package #:lispf)

;;; Application error condition

(define-condition application-error (error)
  ((message :initarg :message :reader application-error-message))
  (:report (lambda (c s) (write-string (application-error-message c) s))))

(defun application-error (format-string &rest args)
  "Signal an APPLICATION-ERROR with a formatted message.
Use this in key handlers for user-facing errors (e.g. validation failures)."
  (error 'application-error :message (apply #'format nil format-string args)))

;;; Dynamic area error (signaled from update thread to main thread)

(define-condition dynamic-area-error (error)
  ()
  (:report (lambda (c s) (declare (ignore c)) (write-string "Dynamic area update error" s))))

;;; Incident ID generation

(defvar *incident-adjectives*
  #("RED" "BLUE" "GREEN" "GOLD" "GRAY" "PINK" "DARK" "WARM" "COLD" "BOLD"
    "FAST" "SLOW" "TALL" "WIDE" "DEEP" "CALM" "KEEN" "WILD" "FAIR" "PALE"))

(defvar *incident-nouns*
  #("FISH" "BIRD" "TREE" "FROG" "BEAR" "HAWK" "WOLF" "DEER" "HARE" "SEAL"
    "LAKE" "HILL" "ROCK" "LEAF" "MOON" "STAR" "WIND" "RAIN" "WAVE" "PEAK"))

(defvar *incident-counter* 0)

(defun generate-incident-id ()
  "Generate a short, speakable incident ID like RED-FISH-42."
  (let ((adj (aref *incident-adjectives* (random (length *incident-adjectives*))))
        (noun (aref *incident-nouns* (random (length *incident-nouns*))))
        (num (incf *incident-counter*)))
    (format nil "~A-~A-~D" adj noun num)))

;;; Incident logging

(defun log-incident (id condition)
  "Log an incident with its ID and condition details to *error-output*."
  (format *error-output* "~&[INCIDENT ~A] ~A~%" id condition))
