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
    (open-file lspf:*session* path)
    'edit))

;;; ============================================================
;;; Command processing (for primary commands on edit screen)
;;; ============================================================

(defmethod lspf:process-screen-command ((screen-name (eql 'edit)) (command string))
  "Process primary commands on the edit screen."
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
      ;; Unknown command - fall through to app-level process-command
      (t nil))))

;;; ============================================================
;;; Subapplication API
;;; ============================================================

(defun edit-file (path &key display-name restricted)
  "Invoke the editor on PATH as a subapplication.
Call from a key handler body. Returns the screen symbol to navigate to.
The calling application's session must extend editor-session.

DISPLAY-NAME overrides the filename shown in the info line (e.g. 'New Message').
RESTRICTED when T disables CANCEL and REVERT commands (for controlled editing).

Example:
  (define-key-handler my-screen :pf4 (filename)
    (lispf-editor:edit-file (pathname filename)
                            :display-name \"New Message\"
                            :restricted t))

The calling application must register the editor screens at startup:
  (lspf:register-screen-directory
    (merge-pathnames #P\"editor/screens/\"
                     (asdf:system-source-directory :lispf)))"
  (open-file lspf:*session* path
             :display-name display-name
             :restricted restricted)
  'edit)

;;; ============================================================
;;; Server entry point
;;; ============================================================

(defun start (&key (port 3270) (host "127.0.0.1"))
  "Start the editor application on PORT."
  (lspf:start-application *editor-app* :port port :host host))
