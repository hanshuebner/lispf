;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; i18n-tests.lisp - Tests for the internationalization system

(defpackage #:lispf-i18n-tests
  (:use #:cl #:lispf-test)
  (:export #:run-all))

(in-package #:lispf-i18n-tests)

;;; Format directive counting

(define-test count-simple-directives ()
  (assert (= 0 (lispf:count-format-directives "hello")))
  (assert (= 1 (lispf:count-format-directives "~A")))
  (assert (= 2 (lispf:count-format-directives "~A and ~D")))
  (assert (= 1 (lispf:count-format-directives "~S"))))

(define-test count-ignores-non-consuming ()
  (assert (= 0 (lispf:count-format-directives "~%")))
  (assert (= 0 (lispf:count-format-directives "~~")))
  (assert (= 0 (lispf:count-format-directives "~&")))
  (assert (= 1 (lispf:count-format-directives "~A~%"))))

(define-test count-with-modifiers ()
  (assert (= 1 (lispf:count-format-directives "~:D")))
  (assert (= 1 (lispf:count-format-directives "~@A")))
  (assert (= 1 (lispf:count-format-directives "~5,'0D"))))

(define-test count-plural ()
  ;; ~P consumes an argument, ~:P does not (uses previous)
  (assert (= 1 (lispf:count-format-directives "~D line~:P"))))

;;; Validation

(define-test validate-matching ()
  (assert (lispf:validate-translation "~A items" "~A Einträge")))

(define-test validate-mismatch ()
  ;; Should warn but not error
  (let ((warned nil))
    (handler-bind ((warning (lambda (c)
                              (declare (ignore c))
                              (setf warned t)
                              (muffle-warning))))
      (lispf:validate-translation "~A items" "Einträge"))
    (assert warned)))

;;; Message lookup

(define-test msg-returns-english-default ()
  (let ((lispf:*application* nil))
    (assert (string= "hello" (lispf:msg "hello")))))

(define-test msg-with-format-args ()
  (let ((lispf:*application* nil))
    (assert (string= "3 items" (lispf:msg "~D items" 3)))))

(define-test msg-uses-catalog ()
  (let* ((catalog (make-hash-table :test 'equal))
         (app (make-instance 'lispf:application
                             :name "test" :entry-screen 'test
                             :screen-directory #P"/tmp/"
                             :package (find-package :cl-user)))
         (lispf:*application* app))
    (setf (gethash "hello" catalog) "hallo")
    (setf (gethash "~D items" catalog) "~D Einträge")
    (lispf:set-message-catalog app catalog)
    (assert (string= "hallo" (lispf:msg "hello")))
    (assert (string= "3 Einträge" (lispf:msg "~D items" 3)))
    ;; Untranslated falls back to English
    (assert (string= "unknown" (lispf:msg "unknown")))))

;;; Catalog loading

(define-test load-catalog-from-file ()
  (let ((path (merge-pathnames "lispf-i18n-test.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (with-open-file (s path :direction :output :if-exists :supersede)
             (write '("hello" "hallo"
                      "~A: unknown command" "~A: unbekannter Befehl"
                      "Prev" "Zurück")
                    :stream s))
           (let ((catalog (lispf:load-message-catalog path)))
             (assert (string= "hallo" (gethash "hello" catalog)))
             (assert (string= "Zurück" (gethash "Prev" catalog)))
             (assert (= 3 (hash-table-count catalog)))))
      (ignore-errors (delete-file path)))))

;;; Template generation

(define-test write-template ()
  (let ((path (merge-pathnames "lispf-template-test.lisp" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (lispf:write-translation-template '("hello" "~D items") path
                                              :target-language "German")
           (assert (probe-file path))
           (let ((content (uiop:read-file-string path)))
             (assert (search "hello" content))
             (assert (search "German" content))))
      (ignore-errors (delete-file path)))))

;;; Runner

(defun run-all ()
  (format t "~&;;; Running i18n tests~%")
  (run-tests 'count-simple-directives
             'count-ignores-non-consuming
             'count-with-modifiers
             'count-plural
             'validate-matching
             'validate-mismatch
             'msg-returns-english-default
             'msg-with-format-args
             'msg-uses-catalog
             'load-catalog-from-file
             'write-template))
