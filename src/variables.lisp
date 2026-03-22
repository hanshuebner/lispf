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

(defvar *cursor-row* 0
  "The cursor row position from the last 3270 response.
Available in key handler bodies.")

(defvar *cursor-col* 0
  "The cursor column position from the last 3270 response.
Available in key handler bodies.")

(defvar *current-field-values* nil
  "The session context hash table. Used by with-field-bindings in key handlers.
Field values from user input are automatically merged here (except transient fields).
Handlers can read and modify values; changes persist across screen transitions.")


(defvar *current-response* nil
  "The raw 3270 response object from the last key press.
Available in key handler bodies. Use cl3270:field-modified-p to check
if a specific field was modified by the user (MDT-based detection).")
(defvar *attribute-intro-char* #\^
  "The character that introduces inline attribute codes in dynamic area strings.
Default is #\\^. Applications may rebind this to a different character.")

(defvar *field-attribute-overrides* nil
  "Alist of (field-name . plist) for runtime field attribute overrides.
Bound to NIL per render cycle. Populated by set-field-attribute.")
