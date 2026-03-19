;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(defpackage #:screen-editor
  (:use #:cl #:alexandria)
  (:export #:start-server
           #:stop-server
           #:*screen-directory*))

(in-package #:screen-editor)

(defvar *screen-directory* nil
  "Directory containing .screen files. Set by start-server.")
