;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; screen.lisp - Screen data building, edit processing, screen update, and key handlers

(in-package #:lispf-editor)

;;; ============================================================

(defun build-screen-data (session)
  "Build prefix and data strings for display.
Returns (values prefix-string data-string) as newline-joined strings
suitable for the framework's repeat field split mechanism."
  (let ((top (editor-top-line session))
        (col-offset (editor-col-offset session))
        (n (line-count session))
        (pending (editor-pending-block session))
        (prefix-lines '())
        (data-lines '()))
    (dotimes (i +page-size+)
      (let ((virtual (+ top i)))
        (cond
          ;; Top of Data marker
          ((= virtual 0)
           (push "******" prefix-lines)
           (push "***************************** Top of Data ******************************"
                 data-lines))
          ;; Bottom of Data marker
          ((= virtual (1+ n))
           (push "******" prefix-lines)
           (push "**************************** Bottom of Data ****************************"
                 data-lines))
          ;; File line
          ((and (> virtual 0) (<= virtual n))
           (let* ((real (1- virtual))
                  (line (or (nth real (editor-lines session)) ""))
                  (pending-start-p (and pending (= real (second pending)))))
             (push (if pending-start-p
                       (format nil "~6A" (string-upcase (symbol-name (first pending))))
                       (format nil "~6,'0D" (1+ real)))
                   prefix-lines)
             (push (visible-portion line col-offset) data-lines)))
          ;; Past end
          (t
           (push "" prefix-lines)
           (push "" data-lines)))))
    (values (format nil "~{~A~^~%~}" (nreverse prefix-lines))
            (format nil "~{~A~^~%~}" (nreverse data-lines)))))

;;; ============================================================
;;; Edit processing (called before any key action)
;;; ============================================================

(defun pending-prefix-for-line-p (session real-index prefix-val)
  "Return T if PREFIX-VAL matches the pending command marker that
build-screen-data would have placed on this line. This means the user
did not type a new command - the prefix is just our own marker echoed back."
  (let ((pending (editor-pending-block session)))
    (when (and pending real-index (= real-index (second pending)))
      (let* ((cmd-name (string-upcase (symbol-name (first pending))))
             (expected (format nil "~6A" cmd-name))
             (trimmed (string-right-trim '(#\Space) prefix-val))
             (expected-trimmed (string-right-trim '(#\Space) expected)))
        (string-equal trimmed expected-trimmed)))))

(defun process-data-edits (session prefix-lines data-lines)
  "Compare data fields with current buffer and apply edits.
Also collects prefix commands. Returns a list of (real-index cmd count row)."
  (let ((top (editor-top-line session))
        (col-offset (editor-col-offset session))
        (commands '()))
    (dotimes (i +page-size+)
      (let* ((virtual (+ top i))
             (real (virtual-to-real session virtual))
             (is-marker (marker-line-p session virtual))
             (prefix-val (if (< i (length prefix-lines))
                             (nth i prefix-lines) ""))
             (data-val (if (< i (length data-lines))
                           (nth i data-lines) "")))
        ;; Apply data edits for real file lines
        (when (and real (not is-marker))
          (let ((original (visible-portion (line-at session real) col-offset)))
            (unless (string= (string-right-trim '(#\Space) data-val)
                             (string-right-trim '(#\Space) original))
              (apply-edit session real data-val))))
        ;; Parse prefix commands (skip if it's our own pending marker echoed back)
        (unless (pending-prefix-for-line-p session real prefix-val)
        (multiple-value-bind (cmd count) (parse-prefix-command prefix-val)
          (when cmd
            (cond
              ;; Top-of-Data marker: allow I (insert at top), A/B targets
              ((and is-marker (= virtual 0))
               (when (member cmd '(:i :a :b))
                 ;; Use real-index -1 to signal "before first line"
                 (push (list -1 cmd count i) commands)))
              ;; Bottom-of-Data marker: allow I (append), A/B targets
              ((and is-marker (= virtual (1+ (line-count session))))
               (when (member cmd '(:i :a :b))
                 (push (list (1- (line-count session)) cmd count i) commands)))
              ;; Regular file line
              (real
               (push (list real cmd count i) commands))
              ;; Empty row past EOF
              (t
               (when (member cmd '(:a :b))
                 (push (list (line-count session) cmd count i) commands)))))))))
    (nreverse commands)))

(defun process-editor-changes (session context)
  "Process data edits and prefix commands from the current response.
CONTEXT is the field-values hash table. Returns error/info message or nil."
  (let* ((prefix-str (or (gethash "prefix" context) ""))
         (data-str (or (gethash "data" context) ""))
         (prefix-lines (split-sequence:split-sequence #\Newline prefix-str))
         (data-lines (split-sequence:split-sequence #\Newline data-str))
         (commands (process-data-edits session prefix-lines data-lines)))
    (when commands
      (execute-prefix-commands session commands))))

;;; ============================================================
;;; Edit screen - screen update (populates repeat fields)
;;; ============================================================

(lspf:define-screen-update edit (info status prefix data)
  (let* ((session lspf:*session*)
         (top (editor-top-line session))
         (total (total-virtual-lines session))
         (col-start (1+ (editor-col-offset session)))
         (col-end (+ (editor-col-offset session) +data-width+))
         (mod-flag (if (editor-modified session) "Modified" ""))
         (pending (editor-pending-block session)))
    ;; Populate data
    (multiple-value-bind (prefix-str data-str) (build-screen-data session)
      (setf prefix prefix-str)
      (setf data data-str))
    ;; Info line
    (setf info (format nil "EDIT  ~A~30TCol ~5,'0D ~5,'0D  Size=~D  Line=~D  ~A"
                       (or (editor-filename session) "(new)")
                       col-start col-end
                       (line-count session)
                       (max 1 (1+ (max 0 (1- top))))
                       mod-flag))
    ;; Status line: show pending block command info with start line
    (setf status
          (if pending
              (let* ((cmd-name (string-upcase (symbol-name (first pending))))
                     (start-line (second pending))
                     (start-visible (<= top (1+ start-line)
                                        (+ top +page-size+ -1))))
                (if start-visible
                    (format nil "~A pending" cmd-name)
                    (format nil "~A pending from line ~D" cmd-name (1+ start-line))))
              ""))
    ;; Make marker data areas non-writable (prefix stays writable for I, A, B commands)
    (dotimes (i +page-size+)
      (let ((virtual (+ top i)))
        (when (marker-line-p session virtual)
          (lspf:set-field-attribute (format nil "prefix.~D" i) :color cl3270:+blue+)
          (lspf:set-field-attribute (format nil "data.~D" i) :write nil :color cl3270:+blue+))))
    ;; Position cursor on command field
    (lspf:set-cursor 21 14)
    ;; Show scroll keys
    (when (> top 0)
      (lspf:show-key :pf7 "Up"))
    (when (< (+ top +page-size+) total)
      (lspf:show-key :pf8 "Down"))
    (when (> (editor-col-offset session) 0)
      (lspf:show-key :pf10 "Left"))
    (lspf:show-key :pf11 "Right")))

;;; ============================================================
;;; Edit screen - key handlers
;;; ============================================================

;;; Enter key (no command) - process prefix commands and edits
(lspf:define-key-handler edit :enter ()
  (let ((msg (process-editor-changes lspf:*session* lspf:*current-field-values*)))
    (when msg
      (setf (gethash "errormsg" lspf:*current-field-values*) msg))
    :stay))

;;; PF3 - Exit (with save prompt if modified)
(lspf:define-key-handler edit :pf3 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (if (editor-modified lspf:*session*)
      (progn
        (setf (gethash "errormsg" lspf:*current-field-values*)
              "File modified - use SAVE, SUBMIT, or CANCEL")
        :stay)
      :back))

;;; PF5 - Repeat Find
(lspf:define-key-handler edit :pf5 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (let ((search-str (editor-last-find lspf:*session*)))
    (if search-str
        (let ((msg (do-find lspf:*session* search-str t)))
          (setf (gethash "errormsg" lspf:*current-field-values*) msg)
          :stay)
        (progn
          (setf (gethash "errormsg" lspf:*current-field-values*) "No previous FIND")
          :stay))))

;;; PF6 - Repeat Change
(lspf:define-key-handler edit :pf6 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (let ((last (editor-last-change lspf:*session*)))
    (if last
        (destructuring-bind (from to all-p) last
          (let ((msg (do-change lspf:*session* from to all-p)))
            (setf (gethash "errormsg" lspf:*current-field-values*) msg)
            :stay))
        (progn
          (setf (gethash "errormsg" lspf:*current-field-values*) "No previous CHANGE")
          :stay))))

;;; PF7 - Scroll Up (DATA mode: one-line overlap for context)
(lspf:define-key-handler edit :pf7 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (setf (editor-top-line lspf:*session*)
        (max 0 (- (editor-top-line lspf:*session*) (1- +page-size+))))
  :stay)

;;; PF8 - Scroll Down (DATA mode: one-line overlap for context)
(lspf:define-key-handler edit :pf8 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (setf (editor-top-line lspf:*session*)
        (+ (editor-top-line lspf:*session*) (1- +page-size+)))
  (clamp-top-line lspf:*session*)
  :stay)

;;; PF10 - Scroll Left
(lspf:define-key-handler edit :pf10 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (setf (editor-col-offset lspf:*session*)
        (max 0 (- (editor-col-offset lspf:*session*) +data-width+)))
  :stay)

;;; PF11 - Scroll Right
(lspf:define-key-handler edit :pf11 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (setf (editor-col-offset lspf:*session*)
        (+ (editor-col-offset lspf:*session*) +data-width+))
  :stay)

