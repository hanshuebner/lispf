;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; read-single-form-tests.lisp
;;;;
;;;; Unit tests for read-single-form file validation.

(defpackage #:lispf-read-single-form-tests
  (:use #:cl #:lispf-test)
  (:export #:run-all))

(in-package #:lispf-read-single-form-tests)

;;; Helpers

(defun temp-file (content)
  "Write CONTENT to a temporary file and return its pathname."
  (let ((path (merge-pathnames
               (format nil "lispf-test-~A.screen" (get-universal-time))
               (uiop:temporary-directory))))
    (with-open-file (s path :direction :output :if-exists :supersede)
      (write-string content s))
    path))

(defmacro with-temp-file ((var content) &body body)
  "Bind VAR to a temp file containing CONTENT, delete after BODY."
  `(let ((,var (temp-file ,content)))
     (unwind-protect (progn ,@body)
       (ignore-errors (delete-file ,var)))))

;;; Tests

(define-test valid-single-form ()
  (with-temp-file (path "(:name \"test\" :fields ())")
    (let ((result (lispf:read-single-form path)))
      (assert (equal "test" (getf result :name))))))

(define-test trailing-form-signals-error ()
  (with-temp-file (path "(:name \"test\" :fields ())
(:keys ((:enter \"Go\")))")
    (handler-case
        (progn
          (lispf:read-single-form path)
          (error "expected an error but none was signalled"))
      (error (c)
        (assert (search "trailing form" (princ-to-string c)))))))

(define-test syntax-error-signals-error ()
  (with-temp-file (path "(:name \"test\" :fields (")
    (handler-case
        (progn
          (lispf:read-single-form path)
          (error "expected an error but none was signalled"))
      (error (c)
        (assert (search "syntax error" (princ-to-string c)))))))

(define-test extra-close-paren-signals-error ()
  (with-temp-file (path "(:name \"test\" :fields ()))")
    (handler-case
        (progn
          (lispf:read-single-form path)
          (error "expected an error but none was signalled"))
      (error (c)
        (assert (search "syntax error" (princ-to-string c)))))))

(define-test trailing-whitespace-and-comments-ok ()
  (with-temp-file (path (format nil "(:name \"test\" :fields ())~%  ~%;; comment~%"))
    (let ((result (lispf:read-single-form path)))
      (assert (equal "test" (getf result :name))))))

;;; Runner

(defun run-all ()
  "Run all read-single-form tests."
  (format t "~&;;; Running read-single-form tests~%")
  (run-tests :lispf-read-single-form-tests))
