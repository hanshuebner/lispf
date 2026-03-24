;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; application.lisp - Application definition, customization, and server entry point

(in-package #:lispf-editor)

;;; ============================================================
;;; Application definition
;;; ============================================================

(lispf:define-application *editor-app*
  :name "editor"
  :title "EDIT"
  :entry-screen open
  :screen-directory (merge-pathnames
                     #P"editor/screens/"
                     (asdf:system-source-directory :lispf))
  :session-class 'editor-session)

;;; ============================================================
;;; Customization
;;; ============================================================

(defmethod lispf:default-command-label ((app (eql *editor-app*)))
  "Command ===>")

(defmethod lispf:paging-labels ((app (eql *editor-app*)))
  (values "Up" "Down"))

;;; ============================================================
;;; Open screen handlers
;;; ============================================================

(lispf:define-key-handler open :enter (filename)
  (let* ((trimmed (string-trim '(#\Space) filename))
         (path (parse-namestring trimmed)))
    (unless path
      (lispf:application-error "Invalid file path"))
    (open-file lispf:*session* path)
    'edit))

;;; ============================================================
;;; Command processing (for primary commands on edit screen)
;;; ============================================================

(defmethod lispf:process-screen-command ((screen-name (eql 'edit)) (command string))
  "Process primary commands on the edit screen."
  (let ((msg (process-editor-changes lispf:*session* (lispf:session-context lispf:*session*)))
        (result (handle-primary-command lispf:*session* command)))
    (cond
      ;; Primary command returned a navigation result
      ((and result (symbolp result) (not (eq result :stay)))
       result)
      ;; Primary command was handled (returned :stay)
      ((eq result :stay)
       (when msg
         (setf (gethash "errormsg" (lispf:session-context lispf:*session*)) msg))
       :stay)
      ;; Primary command returned a message string (info or error)
      ((stringp result)
       (setf (gethash "errormsg" (lispf:session-context lispf:*session*)) result)
       :stay)
      ;; Unknown command - fall through to app-level process-command
      (t nil))))

;;; ============================================================
;;; Subapplication API
;;; ============================================================

(defun edit-file (path &key display-name restricted)
  "Invoke the editor on PATH as a subapplication.
Call from a key handler or screen-update body. Blocks until the editor exits.
The calling application's session must extend editor-session.

DISPLAY-NAME overrides the filename shown in the info line (e.g. 'New Message').
RESTRICTED when T disables CANCEL and REVERT commands (for controlled editing).

Example:
  (define-key-handler my-screen :pf4 (filename)
    (lispf-editor:edit-file (pathname filename)
                            :display-name \"New Message\"
                            :restricted t))"
  (open-file lispf:*session* path
             :display-name display-name
             :restricted restricted)
  (lispf:invoke-subapplication *editor-app* 'edit))

;;; ============================================================
;;; Server entry point
;;; ============================================================

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the editor application on PORT."
  (lispf:start-application *editor-app* :port port :host host))
