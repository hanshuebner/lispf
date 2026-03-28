;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; help-viewer.lisp
;;;;
;;;; Help viewer subapplication for lispf.
;;;; Displays .help files with hypertext navigation, scrolling, and history.
;;;; Links are rendered as tabbable (unprotected) fields in a dynamic area.

(in-package #:lispf)

;;; Help viewer state (stored in session-property)

(defclass help-viewer-state ()
  ((topic :initarg :topic :accessor hv-topic :initform nil)
   (page :initarg :page :accessor hv-page :initform nil)
   (history :initform nil :accessor hv-history
            :documentation "Stack of (topic . scroll-offset) pairs for Back navigation.")
   (scroll-offset :initform 0 :accessor hv-scroll-offset)
   (rendered-lines :initform nil :accessor hv-rendered-lines)
   (link-map :initform nil :accessor hv-link-map)
   (help-directories :initform nil :accessor hv-help-directories
                     :documentation "Directories to search for .help files, captured from calling app.")))

(defconstant +hv-content-rows+ 19
  "Number of content rows in the help viewer (rows 2-20 of the screen).")

(defun ensure-help-viewer-state ()
  "Get or create the help viewer state from session properties."
  (or (session-property *session* :help-viewer-state)
      (setf (session-property *session* :help-viewer-state)
            (make-instance 'help-viewer-state))))

;;; Topic loading

(defun find-help-file-in-directories (name-string directories)
  "Search DIRECTORIES for NAME-STRING.help."
  (loop for dir in directories
        for path = (merge-pathnames (make-pathname :name name-string :type "help") dir)
        when (probe-file path) return path))

(defun load-help-topic (state topic &key no-history)
  "Load a help topic into STATE. Pushes current topic to history unless NO-HISTORY."
  (let ((path (or (find-help-file-in-directories topic (hv-help-directories state))
                  (find-help-file topic))))
    (unless path
      (return-from load-help-topic nil))
    (let ((page (parse-help-file path)))
      (unless page
        (return-from load-help-topic nil))
      ;; Save current topic to history
      (when (and (not no-history) (hv-topic state))
        (push (cons (hv-topic state) (hv-scroll-offset state))
              (hv-history state)))
      ;; Load new topic
      (multiple-value-bind (lines link-map) (render-help-page page)
        (setf (hv-topic state) topic
              (hv-page state) page
              (hv-scroll-offset state) 0
              (hv-rendered-lines state) lines
              (hv-link-map state) link-map))
      t)))

;;; Link resolution

(defun find-link-at-cursor (state)
  "Find the link target at the current cursor position, or NIL.
Maps screen row to document line accounting for scroll offset.
Content starts at screen row 2 (row 1 is info line)."
  (let* ((screen-row (- (cursor-row) 2))
         (doc-line (+ screen-row (hv-scroll-offset state)))
         (col (cursor-col)))
    (when (and (>= screen-row 0) (< screen-row +hv-content-rows+))
      (dolist (lp (hv-link-map state))
        (when (and (= (link-position-line lp) doc-line)
                   (>= col (link-position-col-start lp))
                   (< col (link-position-col-end lp)))
          (return (link-position-target lp)))))))

;;; Content rendering with inline attribute codes for tabbable links

(defun render-help-content-line (rendered-line link-positions offset line-index)
  "Render a single help content line with ^t^+ (turquoise unprotected) for links.
LINK-POSITIONS are the link-position structs for this document line.
Returns a string with inline attribute codes."
  (let ((line-links (remove-if-not (lambda (lp)
                                      (= (link-position-line lp) (+ offset line-index)))
                                    link-positions)))
    (let ((intro *attribute-intro-char*))
      (if (null line-links)
          ;; No links: set green color for the whole line
          (format nil "~Cg~A" intro rendered-line)
          ;; Build attributed string with ^t^+ for link text, ^g^= for normal.
          ;; Each attribute transition creates a field boundary that consumes
          ;; one screen position for the attribute byte. Compensate by inserting
          ;; a padding space at each boundary to keep alignment correct.
          (with-output-to-string (out)
            (let ((col 0)
                  (len (length rendered-line)))
              ;; Start with green
              (format out "~Cg" intro)
              (dolist (lp (sort (copy-list line-links) #'<
                                :key #'link-position-col-start))
                (let ((start (link-position-col-start lp))
                      (end (link-position-col-end lp)))
                  ;; Text before link
                  (when (> start col)
                    (write-string rendered-line out :start col :end (min start len)))
                  ;; Link text: turquoise + unprotected
                  (format out "~Ct~C+" intro intro)
                  (write-string rendered-line out
                               :start (min start len) :end (min end len))
                  ;; Padding space to compensate for the attribute byte that
                  ;; build-dynamic-area-overlay will consume at the field boundary
                  (write-char #\Space out)
                  ;; Back to green + protected
                  (format out "~C=~Cg" intro intro)
                  (setf col end)))
              ;; Remaining text after last link
              (when (< col len)
                (write-string rendered-line out :start col))))))))

;;; Application definition

(define-application *help-viewer-app*
  :name "help"
  :title "Help"
  :entry-screen help-viewer
  :screen-directory (merge-pathnames
                     #P"screens/"
                     (asdf:system-source-directory :lispf)))

;;; Screen update

(define-screen-update help-viewer (hv-info)
  (let* ((state (ensure-help-viewer-state))
         (lines (hv-rendered-lines state))
         (total (length lines))
         (offset (hv-scroll-offset state))
         (page-num (1+ (floor offset +hv-content-rows+)))
         (total-pages (max 1 (ceiling total +hv-content-rows+))))
    ;; Info line (row 1 of the screen, app row 0)
    (setf hv-info
          (format nil "~79A"
                  (if (hv-page state)
                      (format nil " ~A  ~40TPage ~D of ~D  (~A)"
                              (help-page-title (hv-page state))
                              page-num total-pages
                              (or (hv-topic state) ""))
                      " No help content loaded")))
    ;; Override command label to show "Topic" instead of default
    (setf (gethash "%cmdlabel" (session-context *session*))
          "Topic ===>")
    ;; Scroll key visibility
    (when (> offset 0)
      (show-key :pf7 "Up"))
    (when (< (+ offset +hv-content-rows+) total)
      (show-key :pf8 "Down"))))

;;; Dynamic area updater for content

(define-dynamic-area-updater help-viewer content ()
  (let* ((state (ensure-help-viewer-state))
         (lines (hv-rendered-lines state))
         (total (length lines))
         (offset (hv-scroll-offset state))
         (link-map (hv-link-map state)))
    (loop for i below +hv-content-rows+
          for doc-line = (+ offset i)
          collect (if (< doc-line total)
                      (render-help-content-line (nth doc-line lines)
                                                 link-map offset i)
                      ""))))

;;; Key handlers

(defmethod process-screen-command ((screen-name (eql 'help-viewer)) (command string))
  "Handle topic navigation from the command field."
  (let* ((state (ensure-help-viewer-state))
         (topic (string-downcase (string-trim '(#\Space) command))))
    (when (plusp (length topic))
      (if (load-help-topic state topic)
          :stay
          (progn
            (setf (gethash "%errormsg" (session-context *session*))
                  (format nil "~A: help topic not found" topic))
            :stay)))))

(define-key-handler help-viewer :enter ()
  (let ((state (ensure-help-viewer-state)))
    ;; Check for link at cursor
    (when-let (target (find-link-at-cursor state))
      (if (load-help-topic state target)
          (return-from handle-key :stay)
          (progn
            (setf (gethash "%errormsg" (session-context *session*))
                  (format nil "~A: help topic not found" target))
            (return-from handle-key :stay))))
    :stay))

(define-key-handler help-viewer :pf1 ()
  (let ((state (ensure-help-viewer-state)))
    (if (load-help-topic state "index")
        :stay
        (progn
          (setf (gethash "%errormsg" (session-context *session*))
                "No help index available")
          :stay))))

(define-key-handler help-viewer :pf3 ()
  (let ((state (ensure-help-viewer-state)))
    (if (hv-history state)
        (let ((prev (first (hv-history state))))
          (if (load-help-topic state (car prev) :no-history t)
              (progn
                (pop (hv-history state))
                (setf (hv-scroll-offset state) (cdr prev))
                :stay)
              :back))
        :back)))

(define-key-handler help-viewer :pf4 ()
  :back)
(define-key-handler help-viewer :pf7 ()
  (let* ((state (ensure-help-viewer-state))
         (offset (hv-scroll-offset state))
         (scroll (1- +hv-content-rows+)))
    (when (> offset 0)
      (setf (hv-scroll-offset state)
            (max 0 (- offset scroll)))))
  :stay)

(define-key-handler help-viewer :pf8 ()
  (let* ((state (ensure-help-viewer-state))
         (offset (hv-scroll-offset state))
         (total (length (hv-rendered-lines state)))
         (scroll (1- +hv-content-rows+)))
    (when (< (+ offset +hv-content-rows+) total)
      (setf (hv-scroll-offset state) (+ offset scroll))))
  :stay)

;;; Public API

(defun show-help (topic)
  "Display a help topic in the help viewer.
Call from a key handler. TOPIC is the help file name (without .help extension).
Returns :stay."
  (let ((state (ensure-help-viewer-state)))
    ;; Capture the calling application's screen directories for help file search
    (setf (hv-help-directories state)
          (copy-list (application-screen-directories *application*)))
    ;; Reset state for fresh invocation
    (setf (hv-history state) nil)
    (unless (load-help-topic state topic :no-history t)
      (setf (gethash "%errormsg" (session-context *session*))
            (format nil "~A: help topic not found" topic))
      (return-from show-help :stay)))
  (invoke-subapplication *help-viewer-app* 'help-viewer))

(defun show-help-page (page)
  "Display a pre-built help-page in the help viewer.
Call from a key handler. PAGE is a help-page instance.
Returns :stay."
  (let ((state (ensure-help-viewer-state)))
    (setf (hv-help-directories state)
          (copy-list (application-screen-directories *application*)))
    (setf (hv-history state) nil)
    (multiple-value-bind (lines link-map) (render-help-page page)
      (setf (hv-topic state) "commands"
            (hv-page state) page
            (hv-scroll-offset state) 0
            (hv-rendered-lines state) lines
            (hv-link-map state) link-map)))
  (invoke-subapplication *help-viewer-app* 'help-viewer))
