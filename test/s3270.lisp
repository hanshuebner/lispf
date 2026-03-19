;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; s3270.lisp
;;;;
;;;; Subprocess driver for s3270 (headless x3270 scripting client).
;;;; Provides launch, command send/receive, and response parsing.

(in-package #:lispf-test)

;;; Structs

(defstruct s3270-session
  "An s3270 subprocess session."
  process-info
  input-stream
  output-stream)

(defstruct s3270-response
  "A parsed response from s3270."
  (data nil :type list)
  (status nil :type list)
  (ok-p nil :type boolean))

;;; Status line parsing

(defun parse-status-line (line)
  "Parse an s3270 status line into a plist.
The status line has space-separated fields:
  keyboard-state screen-state protect-state connect-state emulator-model
  screen-rows screen-cols cursor-row cursor-col window-id command-time"
  (let ((fields (split-sequence:split-sequence #\Space line
                                               :remove-empty-subseqs t)))
    (list :keyboard-state (first fields)
          :screen-state (second fields)
          :protect-state (third fields)
          :connect-state (fourth fields)
          :emulator-model (fifth fields)
          :rows (when (sixth fields) (parse-integer (sixth fields) :junk-allowed t))
          :cols (when (seventh fields) (parse-integer (seventh fields) :junk-allowed t))
          :cursor-row (when (eighth fields) (parse-integer (eighth fields) :junk-allowed t))
          :cursor-col (when (ninth fields) (parse-integer (ninth fields) :junk-allowed t))
          :window-id (tenth fields)
          :command-time (nth 10 fields))))

;;; Subprocess management

(defun launch-s3270 (&key (program "s3270"))
  "Launch an s3270 subprocess and return an s3270-session."
  (let ((process (uiop:launch-program (list program)
                                      :input :stream
                                      :output :stream
                                      :error-output :stream)))
    (make-s3270-session
     :process-info process
     :input-stream (uiop:process-info-input process)
     :output-stream (uiop:process-info-output process))))

(defun close-s3270 (session)
  "Shut down an s3270 session."
  (ignore-errors
    (send-action session "Quit"))
  (ignore-errors
    (close (s3270-session-input-stream session)))
  (ignore-errors
    (close (s3270-session-output-stream session)))
  (ignore-errors
    (uiop:wait-process (s3270-session-process-info session)))
  nil)

;;; Command protocol

(defun send-action (session action-string)
  "Send an action to s3270 and read the response.
Returns an s3270-response with data lines, status, and ok/error flag."
  (let ((input (s3270-session-input-stream session))
        (output (s3270-session-output-stream session)))
    ;; Send the action
    (write-string action-string input)
    (write-char #\Newline input)
    (finish-output input)
    ;; Read response lines until we see "ok" or "error"
    (let ((data-lines '())
          (status-line nil)
          (ok-p nil))
      (loop for line = (read-line output)
            do (cond
                 ((string= line "ok")
                  (setf ok-p t)
                  (return))
                 ((string= line "error")
                  (setf ok-p nil)
                  (return))
                 ((and (>= (length line) 5)
                       (string= "data: " line :end2 (min 6 (length line))))
                  (push (subseq line 6) data-lines))
                 (t
                  ;; Last non-data, non-terminator line is the status line
                  (setf status-line line))))
      (make-s3270-response
       :data (nreverse data-lines)
       :status (when status-line (parse-status-line status-line))
       :ok-p ok-p))))
