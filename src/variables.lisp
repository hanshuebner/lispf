;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(in-package #:lispf)

(defvar *application* nil
  "The current application object.")

(defvar *session* nil
  "The current session object.")

(defvar *connection* nil
  "The current 3270 connection.")

(defvar *device-info* nil
  "The current device info.")

(defvar *attribute-intro-char* #\^
  "The character that introduces inline attribute codes in dynamic area strings.
Default is #\\^. Applications may rebind this to a different character.")

(defvar *field-attribute-overrides* nil
  "Alist of (field-name . plist) for runtime field attribute overrides.
Bound to NIL per render cycle. Populated by set-field-attribute.")
