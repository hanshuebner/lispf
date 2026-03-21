;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; packages.lisp - Package definition for the LISPF editor

(defpackage #:lispf-editor
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:lspf #:lispf)
                    (#:cl3270 #:cl3270))
  (:export ;; Entry point
           #:start

           ;; Constants
           #:+page-size+
           #:+data-width+
           #:+max-undo+

           ;; Session
           #:editor-session
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
           #:read-file-lines
           #:write-file-lines

           ;; Undo
           #:save-undo-state
           #:undo

           ;; Prefix commands
           #:parse-prefix-command
           #:execute-prefix-commands

           ;; Primary commands
           #:parse-delimited-string
           #:handle-primary-command
           #:do-find
           #:do-change
           #:save-editor-file
           #:revert))
