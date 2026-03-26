;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(defpackage #:lispf
  (:use #:cl #:alexandria)
  (:local-nicknames (#:bt #:bordeaux-threads))
  (:export ;; Registry
           #:register-screen-directory
           #:get-screen
           #:get-screen-rules
           #:get-screen-keys
           #:get-screen-transient-fields
           #:get-screen-repeat-fields
           #:reload-screen
           #:reload-all-screens
           #:compile-screen-data
           #:merge-screen-rules
           #:check-field-names
           #:display-screen
           #:read-single-form
           #:load-screen-data
           ;; Field binding macros
           #:with-field-values
           #:with-field-bindings
           #:make-field-values
           ;; AID dispatch
           #:aid-dispatch
           #:aid-keyword-to-constant
           #:aid-to-keyword
           ;; High-level screen macros
           #:with-screen
           #:with-transaction-screen
           ;; Conditions
           #:application-error
           #:application-error-message
           #:generate-incident-id
           #:log-incident
           ;; List data
           #:get-list-data
           #:define-list-data-getter
           #:list-offset
           #:selected-list-index
           ;; Key handlers
           #:handle-key
           #:define-key-handler
           ;; Screen preparation
           #:prepare-screen
           #:define-screen-update
           #:show-key
           #:hide-key
           #:set-cursor
           ;; Application framework
           #:application
           #:application-name
           #:application-title
           #:application-entry-screen
           #:application-screen-directory
           #:application-session-class
           #:invoke-subapplication
           #:session
           #:session-application
           #:session-active-application
           #:session-last-activity
           #:session-current-screen
           #:session-last-successful-screen
           #:session-screen-stack
           #:session-context
           #:session-properties
           #:session-property
           #:session-tls-p
           #:session-connection-id
           #:*application*
           #:*session*
           #:*connection*
           #:*device-info*
           #:cursor-row
           #:cursor-col
           #:current-response
           #:define-application
           #:run-application
           #:start-application
           #:handle-connection
           ;; Real-time updates
           #:session-write-lock
           #:session-indicators
           #:set-indicator
           #:clear-indicator
           #:broadcast
           #:broadcast-indicator
           ;; Dynamic areas
           #:dynamic-area
           #:dynamic-area-name
           #:dynamic-area-from-row
           #:dynamic-area-from-col
           #:dynamic-area-to-row
           #:dynamic-area-to-col
           #:get-screen-dynamic-areas
           #:get-screen-handler-package
           #:update-dynamic-area
           #:define-dynamic-area-updater
           #:*attribute-intro-char*
           ;; Customization
           #:unknown-key-message
           ;; Logging
           #:log-message
           #:*log-level*
           #:*log-consumers*
           #:add-log-consumer
           #:remove-log-consumer
           ;; Idle timeout
           #:session-idle-timeout
           #:session-cleanup
           ;; Access control
           #:session-authenticated-p
           #:anonymous-access-denied-message
           #:session-user-roles
           #:role-access-denied-message
           ;; Command processing
           #:process-screen-command
           #:process-command
           #:unknown-command-message
           #:default-command-label
           #:menu-command-label
           #:paging-labels
           #:menu-key-labels
           ;; Menu system
           #:find-menu-entry
           #:find-screen-alias
           #:load-application-menus
           #:application-menus
           #:application-menu-entries
           ;; Field attribute overrides
           #:set-field-attribute
           ;; Title utilities
           #:now-time-hhmm
           ;; Help system
           #:navigate-to-help
           ;; Help viewer
           #:help-page
           #:help-page-title
           #:help-page-lines
           #:help-link
           #:help-link-target
           #:help-link-text
           #:parse-help-file
           #:parse-help-line
           #:find-help-file
           #:render-help-page
           #:strip-help-markup
           #:parse-help-segments
           #:help-edit-segment
           #:help-edit-segment-kind
           #:help-edit-segment-text
           #:help-edit-segment-target
           #:help-edit-segment-display-start
           #:help-edit-segment-display-end
           #:reconstruct-help-line
           #:apply-help-line-edit
           #:find-link-segment-at-col
           #:wrap-word-as-link
           #:remove-link-at-col
           #:show-help
           ;; Internationalization
           #:msg
           #:load-message-catalog
           #:set-message-catalog
           #:get-message-catalog
           #:count-format-directives
           #:validate-translation
           #:extract-messages-from-catalog
           #:write-translation-template))

(in-package #:lispf)

(defconstant +screen-rows+ 24)
(defconstant +screen-columns+ 80)

;;; Screen layout parsing (internal)

(defun make-cell-map ()
  (loop with cell-map = (make-array +screen-rows+)
        for row below +screen-rows+
        do (setf (aref cell-map row) (make-array +screen-columns+ :initial-element nil))
        finally (return cell-map)))

(defun ensure-valid-screen-layout (screen-image)
  (let ((definitions (coerce (rest (split-sequence:split-sequence #\Newline screen-image)) 'vector)))
    (unless (= (length definitions) +screen-rows+)
      (error "Layout definition must start with an empty row and have ~A rows following" +screen-rows+))
    (when-let (overlong-row (position-if (lambda (row) (> (length row) +screen-columns+)) definitions))
      (error "Row ~A of the layout definition is longer than the maximum of ~A characters"
             overlong-row +screen-columns+))
    definitions))

(defun screen-image-to-cell-map (screen-image)
  (loop with static-row-definitions = (ensure-valid-screen-layout screen-image)
        with cell-map = (make-cell-map)
        for row-map across cell-map
        for static-row-definition across static-row-definitions
        do (loop for char across (format nil "~80A" static-row-definition)
                 for col from 0
                 do (setf (aref row-map col) (cons char nil)))
        finally (return cell-map)))

(defun attribute-change-position (row-map start-col attributes)
  (or (position-if-not (lambda (cell)
                         (equal (cdr cell) attributes))
                       row-map
                       :start start-col)
      +screen-columns+))

(defun static-text-end-position (row-map start-col)
  (loop with attribute = (cdr (aref row-map start-col))
        for col from (1+ start-col) below +screen-columns+
        if (or (and (eql (car (aref row-map col)) #\Space)
                    (or (= col (1- +screen-columns+))
                        (eql (car (aref row-map (1+ col))) #\Space)))
               (not (equal (cdr (aref row-map col)) attribute)))
          return col))

(defun find-field (cell-map row col)
  (let* ((row-map (aref cell-map row))
         (attributes (cdr (aref row-map col)))
         (end (if attributes
                  (attribute-change-position row-map (1+ col) attributes)
                  (static-text-end-position row-map col)))
         (len (- end col))
         (content (map 'string #'car (subseq row-map col end))))
    (values `(cl3270:make-field :row ,(if (> col 0) row (mod (1- row) +screen-rows+))
                                :col ,(1- (if (> col 0) col +screen-columns+))
                                :len ,len
                                ,@(unless (every (lambda (c) (eql c #\Space)) content) `(:content ,content))
                                ,@attributes)
            row
            end)))

(defun make-fields (cell-map &aux fields)
  (dotimes (row +screen-rows+
                (nreverse fields))
    (dotimes (col +screen-columns+)
      (unless (equal (cons #\Space nil) (aref (aref cell-map row) col))
        (let (field)
          (setf (values field row col) (find-field cell-map row col))
          (push field fields)
          (when (and (< col +screen-columns+)
                     (remove-from-plist (rest field) :row :col :content))
            (push `(cl3270:make-field :row ,row :col ,col) fields)))))))

(defun set-attributes-in-cell-map (cell-map field-definitions)
  (dolist (field-definition field-definitions
                            cell-map)
    (destructuring-bind (&rest keys &key from len &allow-other-keys) field-definition
      (destructuring-bind (row col) from
        (let ((attributes (remove-from-plist keys :from :len)))
          (dotimes (i len)
            (multiple-value-bind (r c) (floor (+ col i) +screen-columns+)
              (let ((actual-row (mod (+ row r) +screen-rows+)))
                (setf (cdr (aref (aref cell-map actual-row) c)) attributes)))))))))

(defun parse-screen (screen-image field-definitions)
  (make-fields (set-attributes-in-cell-map (screen-image-to-cell-map screen-image)
                                           field-definitions)))

;;; Symbolic attribute mapping

(defparameter *symbolic-attribute-map*
  '(("RED" . cl3270:+red+)
    ("BLUE" . cl3270:+blue+)
    ("PINK" . cl3270:+pink+)
    ("GREEN" . cl3270:+green+)
    ("TURQUOISE" . cl3270:+turquoise+)
    ("YELLOW" . cl3270:+yellow+)
    ("WHITE" . cl3270:+white+)
    ("BLINK" . cl3270:+blink+)
    ("REVERSE-VIDEO" . cl3270:+reverse-video+)
    ("UNDERSCORE" . cl3270:+underscore+)))

(defun map-symbolic-value (value)
  "Map a symbolic attribute value (e.g. RED) to its cl3270 constant."
  (if (symbolp value)
      (let ((entry (assoc (symbol-name value) *symbolic-attribute-map* :test #'string-equal)))
        (if entry (cdr entry) value))
      value))

(defun map-field-attributes (field-plist)
  "Convert symbolic attribute values in FIELD-PLIST to cl3270 constant references."
  (loop for (key value) on field-plist by #'cddr
        nconc (list key
                    (case key
                      ((:color :highlighting) (map-symbolic-value value))
                      (:name (string-downcase (string value)))
                      (otherwise value)))))

;;; Screen data file loading

(defun read-single-form (path)
  "Read exactly one s-expression from PATH.
Signals an error if the file contains trailing forms or has a syntax error."
  (with-open-file (s path)
    (let ((*package* (find-package :lispf)))
      (handler-case
          (let ((form (read s)))
            (let ((extra (read s nil s)))
              (unless (eq extra s)
                (error "~A: unexpected trailing form after main definition" path)))
            form)
        (reader-error (c)
          (error "~A: syntax error: ~A" path c))))))

(defun load-screen-data (path)
  "Read a screen definition plist from PATH."
  (read-single-form path))
