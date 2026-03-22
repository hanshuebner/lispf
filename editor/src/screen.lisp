;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; screen.lisp - Screen data building, edit processing, screen update, and key handlers

(in-package #:lispf-editor)

;;; ============================================================

(defun build-screen-data (session)
  "Build prefix and data strings for display.
Returns (values prefix-string data-string) as newline-joined strings
suitable for the framework's repeat field split mechanism."
  (let* ((layout (editor-layout session))
         (top (editor-top-line session))
         (col-offset (editor-col-offset session))
         (n (line-count session))
         (pending (editor-pending-block session))
         (scale-enabled (layout-scale-row layout))
         (cur-line (editor-current-line session))
         (cur-virtual (1+ cur-line))  ; virtual index of current line
         ;; Scale appears after the current line. Compute which data slot that is.
         (scale-after-slot (when scale-enabled
                             (let ((slot (- cur-virtual top)))
                               (when (and (>= slot 0) (< slot (page-size session)))
                                 (1+ slot)))))
         (scale-string (when scale-enabled
                         ;; Pattern: ....+....1....+....2....+....3...
                         ;; 1-based column positions: +5, 1=10, +15, 2=20, +25, 3=30...
                         (with-output-to-string (s)
                           (loop for c from 1 to +data-width+
                                 do (cond ((zerop (mod c 10))
                                           (write-char (digit-char (mod (floor c 10) 10)) s))
                                          ((zerop (mod c 5))
                                           (write-char #\+ s))
                                          (t (write-char #\. s)))))))
         (prefix-lines '())
         (data-lines '())
         (data-slot 0)    ; tracks which data slot we're filling
         (virtual-idx 0)) ; tracks which virtual line we're showing
    ;; Fill page-size data slots, inserting scale line between data lines
    (loop while (< data-slot (page-size session))
          do (let ((virtual (+ top virtual-idx)))
               ;; Insert scale line after current line if enabled
               (when (and scale-after-slot (= data-slot scale-after-slot))
                 (push "      " prefix-lines)
                 (push scale-string data-lines)
                 (incf data-slot)
                 (when (>= data-slot (page-size session))
                   (return)))
               (cond
                 ((= virtual 0)
                  (push "00000 " prefix-lines)
                  (push "* * * Top of File * * *" data-lines))
                 ((= virtual (1+ n))
                  (push (format nil "~5,'0D " (1+ n)) prefix-lines)
                  (push "* * * End of File * * *" data-lines))
                 ((and (> virtual 0) (<= virtual n))
                  (let* ((real (1- virtual))
                         (line (or (nth real (editor-lines session)) ""))
                         (pending-start-p (and pending (= real (second pending)))))
                    (push (if pending-start-p
                              (format nil "~5A " (string-upcase (symbol-name (first pending))))
                              (format nil "~5,'0D " (1+ real)))
                          prefix-lines)
                    (push (visible-portion line col-offset) data-lines)))
                 (t
                  (push "" prefix-lines)
                  (push "" data-lines)))
               (incf data-slot)
               (incf virtual-idx)))
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
  "Process data edits and prefix commands using MDT-based detection.
Only processes fields that were modified by the user (present in the response).
Skips the scale line slot if the scale is active.
Returns a list of (real-index cmd count row)."
  (let* ((layout (editor-layout session))
         (top (editor-top-line session))
         (col-offset (editor-col-offset session))
         (response lspf:*current-response*)
         ;; Compute which data slot has the scale line (if any)
         (cur-virtual (1+ (editor-current-line session)))
         (scale-slot (when (layout-scale-row layout)
                       (let ((slot (- cur-virtual top)))
                         (when (and (>= slot 0) (< slot (page-size session)))
                           (1+ slot)))))
         (commands '())
         (virtual-offset 0))
    (dotimes (i (page-size session))
      (cond
        ;; Skip scale line slot
        ((and scale-slot (= i scale-slot))
         (decf virtual-offset))
        (t
      (let* ((virtual (+ top i virtual-offset))
             (real (virtual-to-real session virtual))
             (is-marker (marker-line-p session virtual))
             (prefix-key (format nil "prefix.~D" i))
             (data-key (format nil "data.~D" i))
             ;; MDT-based: only process fields the user modified.
             ;; When no response available (unit tests), process all fields.
             (prefix-modified (or (null response)
                                  (cl3270:field-modified-p response prefix-key)))
             (data-modified (or (null response)
                                (cl3270:field-modified-p response data-key)))
             (prefix-val (if (< i (length prefix-lines))
                             (nth i prefix-lines) ""))
             (data-val (if (< i (length data-lines))
                           (nth i data-lines) "")))
        ;; Apply data edits only for fields the user actually modified
        (when (and data-modified real (not is-marker))
          (let ((original (visible-portion (line-at session real) col-offset)))
            (unless (string= (string-right-trim '(#\Space) data-val)
                             (string-right-trim '(#\Space) original))
              (apply-edit session real data-val))))
        ;; Parse prefix commands only from modified prefix fields
        (when prefix-modified
          (unless (pending-prefix-for-line-p session real prefix-val)
            (multiple-value-bind (cmd count) (parse-prefix-command prefix-val)
              (when cmd
                (cond
                  ;; Top-of-Data marker: allow I (insert at top), A/B targets
                  ((and is-marker (= virtual 0))
                   (when (member cmd '(:i :a :b))
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
                     (push (list (line-count session) cmd count i) commands))))))))))))
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

(lspf:define-screen-update edit (ed-status ed-message ed-cmdlabel ed-command
                                             prefix data)
  (let* ((session lspf:*session*)
         (layout (editor-layout session))
         (top (editor-top-line session))
         (total (total-virtual-lines session))
         (col-start (1+ (editor-col-offset session)))
         (col-end (+ (editor-col-offset session) +data-width+))
         (pending (editor-pending-block session)))
    ;; Populate data
    (multiple-value-bind (prefix-str data-str) (build-screen-data session)
      (setf prefix prefix-str)
      (setf data data-str))
    ;; Status line (XEDIT style)
    (setf ed-status
          (format nil " ~A~20TCol ~5,'0D ~5,'0D  Size=~D  Line=~D  Alt=~D"
                  (or (editor-display-name session)
                      (editor-filename session)
                      "(new)")
                  col-start col-end
                  (line-count session)
                  (max 1 (1+ (max 0 (1- top))))
                  (editor-alteration-count session)))
    ;; Message line: show pending info or error
    (setf ed-message
          (or (gethash "errormsg" lspf:*current-field-values*)
              (if pending
                  (let* ((cmd-name (string-upcase (symbol-name (first pending))))
                         (start-line (second pending))
                         (start-visible (<= top (1+ start-line)
                                            (+ top (page-size session) -1))))
                    (if start-visible
                        (format nil "~A pending" cmd-name)
                        (format nil "~A pending from line ~D" cmd-name (1+ start-line))))
                  "")))
    ;; Command prompt
    (setf ed-cmdlabel (layout-command-prompt layout))
    (setf ed-command "")
    ;; Set field attributes: markers, current line, scale, past-EOF
    ;; Note: build-screen-data inserts scale line as data, so we need to track
    ;; which data slots have scale vs file content.
    (let* ((bot-virtual (1+ (line-count session)))
           (cur-line (editor-current-line session))
           (cur-virtual (1+ cur-line))
           (scale-after (when (layout-scale-row layout)
                          (let ((slot (- cur-virtual top)))
                            (when (and (>= slot 0) (< slot (page-size session)))
                              (1+ slot))))))
      (let ((virtual-offset 0))
      (dotimes (i (page-size session))
        (cond
          ;; Scale line slot: non-writable, turquoise
          ((and scale-after (= i scale-after))
           (decf virtual-offset)
           (lspf:set-field-attribute (format nil "prefix.~D" i)
                                     :write nil :color cl3270:+turquoise+)
           (lspf:set-field-attribute (format nil "data.~D" i)
                                     :write nil :color cl3270:+turquoise+))
          (t
           (let* ((virtual (+ top i virtual-offset))
                  (real (virtual-to-real session virtual)))
             (cond
               ((marker-line-p session virtual)
                (lspf:set-field-attribute (format nil "prefix.~D" i) :color cl3270:+pink+)
                (lspf:set-field-attribute (format nil "data.~D" i) :write nil :color cl3270:+pink+))
               ((> virtual bot-virtual)
                (lspf:set-field-attribute (format nil "prefix.~D" i) :write nil)
                (lspf:set-field-attribute (format nil "data.~D" i) :write nil))
               ((and real (= real cur-line))
                (lspf:set-field-attribute (format nil "prefix.~D" i)
                                          :color cl3270:+yellow+)
                (lspf:set-field-attribute (format nil "data.~D" i)
                                          :color cl3270:+yellow+)))))))))
    ;; Position cursor (use override if set, otherwise command field)
    (let ((next (editor-next-cursor session)))
      (if next
          (progn
            (lspf:set-cursor (car next) (cdr next))
            (setf (editor-next-cursor session) nil))
          (lspf:set-cursor (layout-command-row layout)
                           (1+ (length (layout-command-prompt layout))))))
    ;; Show scroll keys
    (when (> top 0)
      (lspf:show-key :pf7 "Up"))
    (when (< (+ top (page-size session)) total)
      (lspf:show-key :pf8 "Down"))
    (when (> (editor-col-offset session) 0)
      (lspf:show-key :pf10 "Left"))
    (lspf:show-key :pf11 "Right")))

;;; ============================================================
;;; Edit screen - key handlers
;;; ============================================================

;;; PF1 - Help (process edits first, then navigate to help)
(lspf:define-key-handler edit :pf1 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (let ((help-sym (lspf:navigate-to-help 'edit
                    (lspf::application-package lspf:*application*))))
    (or help-sym :stay)))

;;; Enter key - process prefix commands, edits, and command field
(lspf:define-key-handler edit :enter ()
  (let* ((session lspf:*session*)
         (layout (editor-layout session))
         (data-start (layout-data-start-row layout))
         (data-col (layout-data-col-start layout))
         ;; In full-control mode, read the editor's own command field
         (command (string-trim '(#\Space)
                               (or (gethash "ed-command" lspf:*current-field-values*) "")))
         (msg (process-editor-changes session lspf:*current-field-values*)))
    ;; Handle command field input (since framework doesn't process it in full-control)
    (when (plusp (length command))
      (let ((cmd-result (or (lspf:process-screen-command
                             (lspf:session-current-screen session) command)
                            (lspf:process-command lspf:*application* command))))
        (cond
          (cmd-result
           (return-from lspf:handle-key cmd-result))
          (t
           (setf (gethash "errormsg" lspf:*current-field-values*)
                 (lspf:unknown-command-message lspf:*application* command))
           (return-from lspf:handle-key :stay)))))
    (when msg
      (setf (gethash "errormsg" lspf:*current-field-values*) msg))
    ;; Auto-insert new line when Enter is pressed on a data line
    (unless msg
      (let* ((cursor-row lspf:*cursor-row*)
             (data-row (- cursor-row data-start))
             (top (editor-top-line session)))
        (when (and (>= data-row 0) (< data-row (page-size session)))
          (let* ((virtual (+ top data-row))
                 (real (virtual-to-real session virtual)))
            (when real
              (save-undo-state session)
              (insert-lines-after session real (list ""))
              (let ((new-display-row (1+ cursor-row)))
                (if (< (1+ data-row) (page-size session))
                    (setf (editor-next-cursor session)
                          (cons new-display-row data-col))
                    (progn
                      (setf (editor-top-line session)
                            (+ top (1- (page-size session))))
                      (clamp-top-line session)
                      (setf (editor-next-cursor session)
                            (cons data-start data-col))))))))))
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

;;; PF7 - Scroll Up (with wrap-around)
(lspf:define-key-handler edit :pf7 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (let* ((session lspf:*session*)
         (top (editor-top-line session))
         (ps (page-size session))
         (scroll (1- ps)))
    (if (<= top 0)
        ;; At top: wrap to show last lines + EOF marker
        (let ((total (total-virtual-lines session)))
          (setf (editor-top-line session)
                (max 0 (- total ps))))
        ;; Normal scroll up
        (setf (editor-top-line session)
              (max 0 (- top scroll)))))
  :stay)

;;; PF8 - Scroll Down (with wrap-around)
(lspf:define-key-handler edit :pf8 ()
  (process-editor-changes lspf:*session* lspf:*current-field-values*)
  (let* ((session lspf:*session*)
         (top (editor-top-line session))
         (ps (page-size session))
         (scroll (1- ps))
         (total (total-virtual-lines session)))
    (if (>= (+ top ps) total)
        ;; At or past EOF: wrap to first file line (skip BOF marker)
        (setf (editor-top-line session) 1)
        ;; Normal scroll down
        (progn
          (setf (editor-top-line session) (+ top scroll))
          (clamp-top-line session))))
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

