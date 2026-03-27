;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(defpackage #:lispf-test
  (:use #:cl #:alexandria)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export ;; s3270 driver
           #:s3270-session
           #:s3270-response
           #:s3270-response-data
           #:s3270-response-status
           #:s3270-response-ok-p
           #:launch-s3270
           #:close-s3270
           #:send-action
           #:parse-status-line
           ;; Client
           #:s3270-connect
           #:s3270-disconnect
           #:screen-text
           #:screen-text-at
           #:screen-row
           #:type-text
           #:press-enter
           #:press-pf
           #:press-key
           #:move-cursor
           #:tab-forward
           #:erase-eof
           #:type-at
           #:cursor-position
           #:wait-for-field
           #:press-pf-wait-screen
           #:read-buffer
           #:row-has-input-field-p
           #:row-has-field-attribute-p
           ;; Assertions
           #:test-failure
           #:assert-screen-contains
           #:assert-screen-match
           #:assert-text-at
           #:assert-cursor-at
           #:field-position
           #:read-field
           #:assert-field
           #:type-in-field
           #:assert-on-screen
           #:load-test-screen-data
           ;; Test runner
           #:*test-registry*
           #:*suite-order*
           #:*suite-fixtures*
           #:package-tests
           #:define-test
           #:define-suite-fixtures
           #:run-tests
           #:run-all-suites
           ;; Harness
           #:with-test-app
           #:*test-app-port*))
