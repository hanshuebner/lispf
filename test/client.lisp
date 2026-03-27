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

(defun read-buffer (session)
  "Read the raw 3270 buffer. Returns a list of strings (one per row)
containing field attribute markers like SF(c0e4) for input fields."
  (let ((resp (send-action session "ReadBuffer(Ascii)")))
    (s3270-response-data resp)))

(defun row-has-input-field-p (session row)
  "Return T if ROW contains an unprotected (input) field attribute.
In ReadBuffer(Ascii) output, field attributes appear as SF(c0=XX,...).
The attribute byte XX encodes protection in bit 5: unprotected values
have hex digits 4,5,c,d in the high nibble (bit 5 = 0)."
  (let ((buffer-rows (read-buffer session)))
    (when (< row (length buffer-rows))
      (let ((line (nth row buffer-rows)))
        (and (cl-ppcre:scan "SF\\(c0=[45cd]" line) t)))))

(defun row-has-field-attribute-p (session row)
  "Return T if ROW contains any field attribute (SF marker).
Detects both protected and unprotected fields."
  (let ((buffer-rows (read-buffer session)))
    (when (< row (length buffer-rows))
      (let ((line (nth row buffer-rows)))
        (and (cl-ppcre:scan "SF\\(" line) t)))))

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

(defun erase-eof (session)
  "Erase from cursor to end of current field."
  (send-action session "EraseEOF"))

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

(defun press-pf-wait-screen (session n expected-text &key (timeout 5) (interval 0.1))
  "Press PF N and wait until the screen contains EXPECTED-TEXT.
Useful when the key press triggers a subapplication exit followed by a
parent screen redisplay, where Wait(Unlock) may return before the new
screen arrives."
  (send-action session (format nil "PF(~D)" n))
  (send-action session "Wait(Unlock)")
  (let ((deadline (+ (get-internal-real-time)
                     (* timeout internal-time-units-per-second))))
    (loop
      (let* ((rows (screen-text session))
             (full (format nil "~{~A~^~%~}" rows)))
        (when (search expected-text full)
          (return)))
      (when (> (get-internal-real-time) deadline)
        (error 'lispf-test:test-failure
               :description (format nil "Timeout waiting for ~S after PF~D" expected-text n)
               :expected expected-text
               :actual (screen-row session 0)))
      (sleep interval))))
