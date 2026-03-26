;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; packages.lisp - Package definition for the LISPF editor

(defpackage #:lispf-editor
  (:use #:cl)
  (:import-from #:alexandria #:when-let)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:cl3270 #:cl3270))
  (:export ;; Entry point
           #:start
           #:edit-file

           ;; Constants
           #:+data-width+
           #:+max-undo+

           ;; Layout
           #:editor-layout
           #:make-default-layout
           #:validate-layout
           #:page-size
           #:layout-status-row
           #:layout-message-row
           #:layout-data-start-row
           #:layout-data-end-row
           #:layout-command-row
           #:layout-command-prompt
           #:layout-scale-row
           #:layout-key-labels-row
           #:layout-data-col-start
           #:layout-prefix-width

           ;; Session
           #:editor-session
           #:editor-layout
           #:editor-lines
           #:editor-filename
           #:editor-filepath
           #:editor-modified
           #:editor-top-line
           #:editor-col-offset
           #:editor-last-find
           #:editor-last-change
           #:editor-last-find-line
           #:editor-pending-block
           #:editor-undo-stack
           #:editor-display-name
           #:editor-restricted-p
           #:editor-current-line
           #:editor-auto-insert-p
           #:editor-verbose-p
           #:editor-alteration-count
           #:help-file-p
           #:make-test-session

           ;; Buffer operations
           #:line-count
           #:total-virtual-lines
           #:virtual-to-real
           #:line-at
           #:insert-lines-after
           #:insert-lines-before
           #:delete-line-range
           #:extract-line-range
           #:visible-portion
           #:apply-edit

           ;; File I/O
           #:open-file
           #:read-file-lines
           #:write-file-lines

           ;; Undo
           #:save-undo-state
           #:undo

           ;; Prefix commands
           #:parse-prefix-command
           #:execute-prefix-commands

           ;; Justify
           #:justify-lines
           #:editor-justify-range

           ;; Primary commands
           #:parse-delimited-string
           #:handle-primary-command
           #:do-find
           #:do-change
           #:save-editor-file
           #:revert))
