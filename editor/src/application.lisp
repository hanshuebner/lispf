;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; application.lisp - Application definition, customization, and server entry point

(in-package #:lispf-editor)

;;; ============================================================
;;; Application definition
;;; ============================================================

(lspf:define-application *editor-app*
  :entry-screen open
  :screen-directory (merge-pathnames
                     #P"editor/screens/"
                     (asdf:system-source-directory :lispf))
  :session-class 'editor-session)

;;; ============================================================
;;; Customization
;;; ============================================================

(defmethod lspf:default-command-label ((app (eql *editor-app*)))
  "Command ===>")

(defmethod lspf:paging-labels ((app (eql *editor-app*)))
  (values "Up" "Down"))

;;; ============================================================
;;; Open screen handlers
;;; ============================================================

(lspf:define-key-handler open :enter (filename)
  (let* ((trimmed (string-trim '(#\Space) filename))
         (path (parse-namestring trimmed)))
    (unless path
      (lspf:application-error "Invalid file path"))
    (let ((lines (read-file-lines path)))
      (setf (editor-filepath lspf:*session*) path)
      (setf (editor-filename lspf:*session*)
            (file-namestring path))
      (if lines
          (setf (editor-lines lspf:*session*) lines)
          ;; New file
          (setf (editor-lines lspf:*session*) (list "")))
      (setf (editor-modified lspf:*session*) nil)
      (setf (editor-top-line lspf:*session*) 0)
      (setf (editor-col-offset lspf:*session*) 0)
      'edit)))

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

;;; ============================================================
;;; Command processing (for primary commands on edit screen)
;;; ============================================================

(defmethod lspf:process-command ((app (eql *editor-app*)) (command string))
  (if (eq (lspf:session-current-screen lspf:*session*) 'edit)
      ;; Edit screen: process editor changes + primary command
      (let ((msg (process-editor-changes lspf:*session* lspf:*current-field-values*))
            (result (handle-primary-command lspf:*session* command)))
        (cond
          ;; Primary command returned a navigation result
          ((and result (symbolp result) (not (eq result :stay)))
           result)
          ;; Primary command was handled (returned :stay)
          ((eq result :stay)
           (when msg
             (setf (gethash "errormsg" lspf:*current-field-values*) msg))
           :stay)
          ;; Primary command returned a message string (info or error)
          ((stringp result)
           (setf (gethash "errormsg" lspf:*current-field-values*) result)
           :stay)
          ;; Unknown command
          (t nil)))
      ;; Other screens: default behavior
      (call-next-method)))

;;; ============================================================
;;; Server entry point
;;; ============================================================

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the editor application on PORT."
  (lspf:start-application *editor-app* :port port :host host))
