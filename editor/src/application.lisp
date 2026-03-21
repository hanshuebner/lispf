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
