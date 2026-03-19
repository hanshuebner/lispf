;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; client.lisp
;;;;
;;;; High-level screen interaction API built on top of the s3270 driver.

(in-package #:lispf-test)

;;; Connection

(defun s3270-connect (session host port)
  "Connect to a TN3270 server at HOST:PORT.
Uses Wait(3270Mode) then Wait(Unlock) since the first screen may have no input fields."
  (send-action session (format nil "Connect(~A:~D)" host port))
  (send-action session "Wait(3270Mode)")
  (send-action session "Wait(Unlock)"))

(defun s3270-disconnect (session)
  "Disconnect from the current TN3270 server."
  (send-action session "Disconnect"))

;;; Screen reading

(defun screen-text (session)
  "Read the entire screen as a list of strings (one per row)."
  (let ((resp (send-action session "Ascii()")))
    (s3270-response-data resp)))

(defun screen-text-at (session row col length)
  "Read LENGTH characters starting at ROW,COL. Returns a string."
  (let ((resp (send-action session (format nil "Ascii(~D,~D,~D)" row col length))))
    (if (s3270-response-data resp)
        (first (s3270-response-data resp))
        "")))

(defun screen-row (session row)
  "Read a full row (80 columns) from the screen."
  (screen-text-at session row 0 80))

;;; Input

(defun type-text (session text)
  "Type TEXT at the current cursor position. Quotes are escaped."
  (let ((escaped (with-output-to-string (s)
                   (loop for ch across text
                         do (case ch
                              (#\" (write-string "\\\"" s))
                              (#\\ (write-string "\\\\" s))
                              (t (write-char ch s)))))))
    (send-action session (format nil "String(\"~A\")" escaped))))

(defun press-enter (session)
  "Press Enter and wait for the keyboard to unlock."
  (send-action session "Enter")
  (send-action session "Wait(Unlock)"))

(defun press-pf (session n)
  "Press PF key N and wait for the keyboard to unlock."
  (send-action session (format nil "PF(~D)" n))
  (send-action session "Wait(Unlock)"))

(defun press-key (session key)
  "Press a key by keyword. Supported: :enter, :clear, :tab, :pf1-:pf24."
  (ecase key
    (:enter (press-enter session))
    (:clear (send-action session "Clear")
            (send-action session "Wait(Unlock)"))
    (:tab (send-action session "Tab"))
    ((:pf1 :pf2 :pf3 :pf4 :pf5 :pf6 :pf7 :pf8 :pf9 :pf10
      :pf11 :pf12 :pf13 :pf14 :pf15 :pf16 :pf17 :pf18 :pf19 :pf20
      :pf21 :pf22 :pf23 :pf24)
     (press-pf session (parse-integer (subseq (symbol-name key) 2))))))

(defun move-cursor (session row col)
  "Move the cursor to ROW,COL (0-based)."
  (send-action session (format nil "MoveCursor(~D,~D)" row col)))

(defun tab-forward (session)
  "Advance to the next input field."
  (send-action session "Tab"))

;;; Compound operations

(defun type-at (session row col text)
  "Move cursor to ROW,COL and type TEXT."
  (move-cursor session row col)
  (type-text session text))

(defun cursor-position (session)
  "Return the current cursor position as (values row col).
Queries s3270 for the current status."
  (let* ((resp (send-action session "Query(Cursor)"))
         (data (first (s3270-response-data resp))))
    (if data
        (let ((parts (split-sequence:split-sequence #\Space data
                                                    :remove-empty-subseqs t)))
          (values (parse-integer (first parts))
                  (parse-integer (second parts))))
        (values (getf (s3270-response-status resp) :cursor-row)
                (getf (s3270-response-status resp) :cursor-col)))))

(defun wait-for-field (session)
  "Wait until an input field is available."
  (send-action session "Wait(InputField)"))
