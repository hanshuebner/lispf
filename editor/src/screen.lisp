;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; screen.lisp - Screen data building, edit processing, screen update, and key handlers

(in-package #:lispf-editor)

;;; ============================================================

(defun pad-to-field-width (string width)
  "Pad STRING with spaces to at least WIDTH characters.
Ensures the full field is overwritten in 3270 no-clear mode."
  (if (>= (length string) width)
      string
      (concatenate 'string string
                   (make-string (- width (length string))
                                :initial-element #\Space))))

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
         ;; Scale is at a fixed screen row within the data area.
         ;; Compute which data slot it occupies.
         (scale-slot (when scale-enabled
                       (let ((slot (- scale-enabled (layout-data-start-row layout))))
                         (when (and (>= slot 0) (< slot (page-size session)))
                           slot))))
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
               (when (and scale-slot (= data-slot scale-slot))
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
                    (push (visible-portion (if (help-file-p session)
                                               (lispf:strip-help-markup line)
                                               line)
                                           col-offset)
                          data-lines)))
                 (t
                  (push "      " prefix-lines)
                  (push (make-string +data-width+ :initial-element #\Space)
                        data-lines)))
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
         (response (lispf:current-response))
         (cursor-slot (cursor-data-row (lispf:cursor-row) layout))
         ;; Scale is at a fixed screen row within the data area
         (scale-slot (when (layout-scale-row layout)
                       (let ((slot (- (layout-scale-row layout)
                                      (layout-data-start-row layout))))
                         (when (and (>= slot 0) (< slot (page-size session)))
                           slot))))
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
          (let* ((raw-line (line-at session real))
                 (is-help (help-file-p session))
                 (original (visible-portion (if is-help
                                                (lispf:strip-help-markup raw-line)
                                                raw-line)
                                            col-offset)))
            (unless (string= (string-right-trim '(#\Space) data-val)
                             (string-right-trim '(#\Space) original))
              (save-undo-state session)
              (if is-help
                  (setf (nth real (editor-lines session))
                        (lispf:apply-help-line-edit raw-line col-offset
                                                   +data-width+ data-val))
                  (apply-edit session real data-val)))))
        ;; Parse prefix commands only from modified prefix fields
        (when prefix-modified
          (unless (pending-prefix-for-line-p session real prefix-val)
            (let* ((prefix-cursor-col
                     (when (and (= i cursor-slot)
                                (< (lispf:cursor-col) (layout-data-col-start layout)))
                       (lispf:cursor-col))))
              (multiple-value-bind (cmd count) (parse-prefix-command prefix-cursor-col prefix-val)
                (when cmd
                  (cond
                    ;; Top-of-Data marker: allow I (insert at top), A/B targets
                    ((and is-marker (= virtual 0))
                     (if (member cmd '(:i :a :b))
                         (push (list -1 cmd count i) commands)
                         (push (list :error "Command not valid on marker line") commands)))
                    ;; Bottom-of-Data marker: allow I (append), A/B targets
                    ((and is-marker (= virtual (1+ (line-count session))))
                     (if (member cmd '(:i :a :b))
                         (push (list (1- (line-count session)) cmd count i) commands)
                         (push (list :error "Command not valid on marker line") commands)))
                    ;; Regular file line
                    (real
                     (push (list real cmd count i) commands))
                    ;; Empty row past EOF
                    (t
                     (when (member cmd '(:a :b))
                       (push (list (line-count session) cmd count i) commands)))))))))))))
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

(lispf:define-screen-update edit (ed-status ed-message ed-cmdlabel ed-command
                                             prefix data ed-keys)
  (let* ((session lispf:*session*)
         (layout (editor-layout session))
         (top (editor-top-line session))
         (total (total-virtual-lines session))
         (col-start (1+ (editor-col-offset session)))
         (col-end (+ (editor-col-offset session) +data-width+))
         (pending (editor-pending-block session)))
    ;; Ensure top-line is within valid range (may be out after block delete/justify).
    ;; If the file is shorter than one page, show from the top.
    ;; Otherwise, ensure we show as much content as possible (no unnecessary blank rows).
    (let ((effective-page (if (layout-scale-row layout)
                              (1- (page-size session))
                              (page-size session))))
      (when (> top (max 0 (- total effective-page)))
        (setf (editor-top-line session) (max 0 (- total effective-page)))))
    (setf top (editor-top-line session))
    ;; Populate data
    (multiple-value-bind (prefix-str data-str) (build-screen-data session)
      (setf prefix prefix-str)
      (setf data data-str))
    ;; Status line (XEDIT style) - pad to field width for no-clear mode
    (setf ed-status
          (pad-to-field-width
           (format nil " ~A~20TCol ~5,'0D ~5,'0D  Size=~D  Line=~D  Alt=~D"
                   (or (editor-display-name session)
                       (editor-filename session)
                       "(new)")
                   col-start col-end
                   (line-count session)
                   (max 1 (1+ (max 0 (1- top))))
                   (editor-alteration-count session))
           79))
    ;; Message line: show pending info or error - pad to field width
    (setf ed-message
          (pad-to-field-width
           (or (gethash "errormsg" (lispf:session-context lispf:*session*))
               (if pending
                   (let* ((cmd-name (string-upcase (symbol-name (first pending))))
                          (start-line (second pending))
                          (start-visible (<= top (1+ start-line)
                                             (+ top (page-size session) -1))))
                     (if start-visible
                         (format nil "~A pending" cmd-name)
                         (format nil "~A pending from line ~D" cmd-name (1+ start-line))))
                   ""))
           79))
    ;; Command prompt (spaces instead of "" to clear on no-clear redisplay)
    (setf ed-cmdlabel (layout-command-prompt layout)
          ed-command (make-string 73 :initial-element #\Space))
    ;; Set field attributes: markers, current line, scale, past-EOF
    ;; Note: build-screen-data inserts scale line as data, so we need to track
    ;; which data slots have scale vs file content.
    (let* ((bot-virtual (1+ (line-count session)))
           (cur-line (editor-current-line session))
           (scale-slot-attr (when (layout-scale-row layout)
                             (let ((slot (- (layout-scale-row layout)
                                            (layout-data-start-row layout))))
                               (when (and (>= slot 0) (< slot (page-size session)))
                                 slot)))))
      (let ((virtual-offset 0))
      (dotimes (i (page-size session))
        (cond
          ;; Scale line slot: non-writable, turquoise
          ((and scale-slot-attr (= i scale-slot-attr))
           (decf virtual-offset)
           (lispf:set-field-attribute (format nil "prefix.~D" i)
                                     :write nil :color cl3270:+turquoise+)
           (lispf:set-field-attribute (format nil "data.~D" i)
                                     :write nil :color cl3270:+turquoise+))
          (t
           (let* ((virtual (+ top i virtual-offset))
                  (real (virtual-to-real session virtual)))
             (cond
               ((marker-line-p session virtual)
                (lispf:set-field-attribute (format nil "prefix.~D" i) :color cl3270:+pink+)
                (lispf:set-field-attribute (format nil "data.~D" i) :write nil :color cl3270:+pink+))
               ((> virtual bot-virtual)
                (lispf:set-field-attribute (format nil "prefix.~D" i) :write nil)
                (lispf:set-field-attribute (format nil "data.~D" i) :write nil))
               ((and real (= real cur-line))
                (lispf:set-field-attribute (format nil "prefix.~D" i)
                                          :color cl3270:+yellow+)
                (lispf:set-field-attribute (format nil "data.~D" i)
                                          :color cl3270:+yellow+)))))))))
    ;; Highlight lines containing help links in .help files
    (when (help-file-p session)
      (let ((virtual-offset 0))
        (dotimes (i (page-size session))
          (if (and (layout-scale-row layout)
                   (let ((slot (- (layout-scale-row layout)
                                  (layout-data-start-row layout))))
                     (and (>= slot 0) (< slot (page-size session))
                          (= i slot))))
              (decf virtual-offset)
              (let* ((virtual (+ top i virtual-offset))
                     (real (virtual-to-real session virtual)))
                (when (and real (not (marker-line-p session virtual)))
                  (let ((line (line-at session real)))
                    (when (and line (position #\{ line))
                      (lispf:set-field-attribute (format nil "data.~D" i)
                                                :color cl3270:+turquoise+)))))))))
    ;; Position cursor (use override if set, otherwise command field)
    (let ((next (editor-next-cursor session)))
      (if next
          (progn
            (lispf:set-cursor (car next) (cdr next))
            (setf (editor-next-cursor session) nil))
          (lispf:set-cursor (layout-command-row layout)
                           (1+ (length (layout-command-prompt layout))))))
    (lispf:show-key :pf1 "Help")
    ;; Show repeat keys when a find/change has been done
    (when (editor-last-find session)
      (lispf:show-key :pf5 "RFnd"))
    (when (editor-last-change session)
      (lispf:show-key :pf6 "RChg"))
    ;; Show scroll keys
    (when (> top 0)
      (lispf:show-key :pf7 "Up"))
    (when (< (+ top (page-size session)) total)
      (lispf:show-key :pf8 "Down"))
    (when (> (editor-col-offset session) 0)
      (lispf:show-key :pf10 "Left"))
    (lispf:show-key :pf11 "Rght")
    ;; Render key labels
    (setf ed-keys (or (lispf:format-key-labels) ""))))

;;; ============================================================
;;; Edit screen - key handlers
;;; ============================================================

;;; PF1 - Help (process edits first, then navigate to help)

(defun dispatch-command (session command)
  "Dispatch a command entered in the editor's command field.
Returns a navigation result (:stay, :back, screen symbol), or nil if no command."
  (when (plusp (length command))
    (when-let ((result (or (lispf:process-screen-command
                       (lispf:session-current-screen session) command)
                      (lispf:process-command lispf:*application* command))))
      (return-from dispatch-command result))
    (setf (gethash "errormsg" (lispf:session-context lispf:*session*))
          (lispf:unknown-command-message lispf:*application* command))
    :stay))

(defun cursor-data-row (cursor-row layout)
  "Convert a screen cursor row to a data slot index, accounting for scale line."
  (let ((raw (- cursor-row (layout-data-start-row layout)))
        (scale-row (layout-scale-row layout)))
    (if (and scale-row (>= cursor-row scale-row))
        (1- raw)
        raw)))

(defun auto-insert-line (session layout)
  "On Enter: insert blank line if cursor is on the last file line, else advance cursor."
  (let* ((data-row (cursor-data-row (lispf:cursor-row) layout))
         (top (editor-top-line session)))
    (unless (and (>= data-row 0) (< data-row (page-size session)))
      (return-from auto-insert-line))
    (let ((real (virtual-to-real session (+ top data-row))))
      (unless real (return-from auto-insert-line))
      ;; Insert a new line when auto-insert is enabled
      (when (editor-auto-insert-p session)
        (save-undo-state session)
        (insert-lines-after session real (list "")))
      ;; Advance cursor to the next line
      (let* ((scale-row (layout-scale-row layout))
             (new-row (1+ (lispf:cursor-row)))
             (new-row (if (and scale-row (= new-row scale-row))
                          (1+ new-row)
                          new-row))
             (data-col (layout-data-col-start layout)))
        (if (< (1+ data-row) (page-size session))
            (setf (editor-next-cursor session) (cons new-row data-col))
            (progn
              (incf (editor-top-line session))
              (clamp-top-line session)
              (setf (editor-next-cursor session)
                    (cons (+ (layout-data-start-row layout)
                             (1- (page-size session)))
                          data-col))))))))
(lispf:define-key-handler edit :pf1 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (lispf:show-help "index"))

;;; Enter key - process prefix commands, edits, and command field
(lispf:define-key-handler edit :enter ()
  (let* ((session lispf:*session*)
         (layout (editor-layout session))
         (command (string-trim '(#\Space)
                               (or (gethash "ed-command" (lispf:session-context lispf:*session*)) "")))
         (msg (process-editor-changes session (lispf:session-context lispf:*session*))))
    ;; Handle command field input (since framework doesn't process it in full-control)
    (let ((cmd-result (dispatch-command session command)))
      (when cmd-result
        (return-from lispf:handle-key cmd-result)))
    (when msg
      (setf (gethash "errormsg" (lispf:session-context lispf:*session*)) msg))
    ;; Auto-insert new line when Enter is pressed on a data line
    (unless msg
      (auto-insert-line session layout))
    :stay))

;;; PF3 - Exit (with save prompt if modified)
(lispf:define-key-handler edit :pf3 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (when (editor-modified lispf:*session*)
    (setf (gethash "errormsg" (lispf:session-context lispf:*session*))
          "File modified - use SAVE, SUBMIT, or CANCEL")
    (return-from lispf:handle-key :stay))
  :back)

;;; PF5 - Repeat Find
(lispf:define-key-handler edit :pf5 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (let ((search-str (editor-last-find lispf:*session*)))
    (unless search-str
      (setf (gethash "errormsg" (lispf:session-context lispf:*session*)) "No previous FIND")
      (return-from lispf:handle-key :stay))
    (setf (gethash "errormsg" (lispf:session-context lispf:*session*))
          (do-find lispf:*session* search-str t))
    :stay))

;;; PF6 - Repeat Change
(lispf:define-key-handler edit :pf6 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (let ((last (editor-last-change lispf:*session*)))
    (unless last
      (setf (gethash "errormsg" (lispf:session-context lispf:*session*)) "No previous CHANGE")
      (return-from lispf:handle-key :stay))
    (destructuring-bind (from to all-p) last
      (setf (gethash "errormsg" (lispf:session-context lispf:*session*))
            (do-change lispf:*session* from to all-p))
      :stay)))

;;; PF7 - Scroll Up (with wrap-around)
(lispf:define-key-handler edit :pf7 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (let* ((session lispf:*session*)
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
(lispf:define-key-handler edit :pf8 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (let* ((session lispf:*session*)
         (top (editor-top-line session))
         (ps (page-size session))
         (total (total-virtual-lines session)))
    (setf (editor-top-line session)
          (if (>= (+ top ps) total)
              1
              (+ top (1- ps))))
    (clamp-top-line session))
  :stay)

;;; PF10 - Scroll Left
(lispf:define-key-handler edit :pf10 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (setf (editor-col-offset lispf:*session*)
        (max 0 (- (editor-col-offset lispf:*session*) +data-width+)))
  :stay)

;;; PF11 - Scroll Right
(lispf:define-key-handler edit :pf11 ()
  (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*))
  (setf (editor-col-offset lispf:*session*)
        (+ (editor-col-offset lispf:*session*) +data-width+))
  :stay)

