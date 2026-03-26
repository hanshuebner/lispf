;;; -*- Mode: Lisp -*-

;;; Unit and E2E tests for the LISPF help system.
;;;
;;; Usage:
;;;   (asdf:load-system "lispf")
;;;   (asdf:load-system "lispf-test")
;;;   (asdf:load-system "lispf-edit")
;;;   (load ".../test/help-tests.lisp")
;;;   (lispf-help-tests:run-all)

(defpackage #:lispf-help-tests
  (:use #:cl #:lispf-test)
  (:local-nicknames (#:ed #:lispf-editor))
  (:export #:run-all))

(in-package #:lispf-help-tests)

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun assert-equal (expected actual &optional description)
  (unless (equal expected actual)
    (error 'test-failure
           :description (or description "Value mismatch")
           :expected expected
           :actual actual)))

(defun assert-true (value &optional description)
  (unless value
    (error 'test-failure
           :description (or description "Expected true")
           :expected t
           :actual nil)))

(defun assert-nil (value &optional description)
  (when value
    (error 'test-failure
           :description (or description "Expected nil")
           :expected nil
           :actual value)))

(defun assert-string= (expected actual &optional description)
  (unless (string= expected actual)
    (error 'test-failure
           :description (or description "String mismatch")
           :expected expected
           :actual actual)))

(defun write-help-file (path title &rest body-lines)
  "Write a .help file with TITLE and BODY-LINES."
  (with-open-file (s path :direction :output :if-exists :supersede)
    (write-line title s)
    (dolist (line body-lines)
      (write-line line s))))

(defun make-help-session (&rest lines)
  "Create an editor test session with .help file extension."
  (let ((s (ed:make-test-session (copy-list lines))))
    (setf (ed:editor-filepath s) #P"/tmp/test.help")
    s))

;;; ============================================================
;;; Help parser tests
;;; ============================================================

(define-test parse-plain-line ()
  (let ((segments (lispf:parse-help-line "Hello world")))
    (assert-equal 1 (length segments))
    (assert-true (stringp (first segments)))
    (assert-string= "Hello world" (first segments))))

(define-test parse-link-with-text ()
  (let ((segments (lispf:parse-help-line "See {edit:Editor Help} for info.")))
    (assert-equal 3 (length segments))
    (assert-string= "See " (first segments))
    (assert-true (typep (second segments) 'lispf:help-link))
    (assert-string= "edit" (lispf:help-link-target (second segments)))
    (assert-string= "Editor Help" (lispf:help-link-text (second segments)))
    (assert-string= " for info." (third segments))))

(define-test parse-shorthand-link ()
  (let ((segments (lispf:parse-help-line "See {edit} for info.")))
    (assert-equal 3 (length segments))
    (let ((link (second segments)))
      (assert-string= "edit" (lispf:help-link-target link))
      (assert-string= "edit" (lispf:help-link-text link)))))

(define-test parse-multiple-links ()
  (let ((segments (lispf:parse-help-line "{a:alpha} and {b:beta}")))
    (assert-equal 3 (length segments))
    (assert-string= "alpha" (lispf:help-link-text (first segments)))
    (assert-string= " and " (second segments))
    (assert-string= "beta" (lispf:help-link-text (third segments)))))

(define-test parse-unclosed-brace ()
  (let ((segments (lispf:parse-help-line "Text with {unclosed")))
    (assert-equal 1 (length segments))
    (assert-string= "Text with {unclosed" (first segments))))

(define-test parse-empty-line ()
  (let ((segments (lispf:parse-help-line "")))
    (assert-equal 0 (length segments))))

(define-test parse-help-file-basic ()
  (let ((path #P"/tmp/lispf-test-parse.help"))
    (unwind-protect
         (progn
           (write-help-file path "Test Title"
                            "First line."
                            "# comment line"
                            "Third {topic:line}.")
           (let ((page (lispf:parse-help-file path)))
             (assert-true page)
             (assert-string= "Test Title" (lispf:help-page-title page))
             ;; Comment should be stripped, leaving 2 body lines
             (assert-equal 2 (length (lispf:help-page-lines page)))))
      (ignore-errors (delete-file path)))))

;;; ============================================================
;;; Markup stripping tests
;;; ============================================================

(define-test strip-plain-text ()
  (assert-string= "Hello world"
                   (lispf:strip-help-markup "Hello world")))

(define-test strip-link-with-text ()
  (assert-string= "See Editor Help for info."
                   (lispf:strip-help-markup "See {edit:Editor Help} for info.")))

(define-test strip-shorthand-link ()
  (assert-string= "See edit for info."
                   (lispf:strip-help-markup "See {edit} for info.")))

(define-test strip-multiple-links ()
  (assert-string= "alpha and beta"
                   (lispf:strip-help-markup "{a:alpha} and {b:beta}")))

(define-test strip-unclosed-brace ()
  (assert-string= "Text with {unclosed"
                   (lispf:strip-help-markup "Text with {unclosed")))

(define-test strip-empty ()
  (assert-string= "" (lispf:strip-help-markup "")))

;;; ============================================================
;;; Help page rendering tests
;;; ============================================================

(define-test render-page-lines ()
  (let ((path #P"/tmp/lispf-test-render.help"))
    (unwind-protect
         (progn
           (write-help-file path "Title"
                            "Plain line."
                            "{edit:A Link} here.")
           (let ((page (lispf:parse-help-file path)))
             (multiple-value-bind (lines link-map) (lispf:render-help-page page)
               (assert-equal 2 (length lines))
               (assert-string= "Plain line." (first lines))
               (assert-string= "A Link here." (second lines))
               ;; One link on line 1
               (assert-equal 1 (length link-map))
               (let ((lp (first link-map)))
                 (assert-equal 1 (lispf::link-position-line lp))
                 (assert-equal 0 (lispf::link-position-col-start lp))
                 (assert-equal 6 (lispf::link-position-col-end lp))
                 (assert-string= "edit" (lispf::link-position-target lp))))))
      (ignore-errors (delete-file path)))))

(define-test render-multiple-links-same-line ()
  (let ((path #P"/tmp/lispf-test-render2.help"))
    (unwind-protect
         (progn
           (write-help-file path "Title"
                            "{a:alpha} and {b:beta}")
           (let ((page (lispf:parse-help-file path)))
             (multiple-value-bind (lines link-map) (lispf:render-help-page page)
               (assert-string= "alpha and beta" (first lines))
               (assert-equal 2 (length link-map))
               ;; First link: cols 0-5
               (assert-equal 0 (lispf::link-position-col-start (first link-map)))
               (assert-equal 5 (lispf::link-position-col-end (first link-map)))
               ;; Second link: cols 10-14
               (assert-equal 10 (lispf::link-position-col-start (second link-map)))
               (assert-equal 14 (lispf::link-position-col-end (second link-map))))))
      (ignore-errors (delete-file path)))))

;;; ============================================================
;;; Segment parsing tests (for edit mapping)
;;; ============================================================

(define-test segments-plain-text ()
  (let ((segs (lispf:parse-help-segments "Hello world")))
    (assert-equal 1 (length segs))
    (assert-equal :text (lispf:help-edit-segment-kind (first segs)))
    (assert-string= "Hello world" (lispf:help-edit-segment-text (first segs)))
    (assert-equal 0 (lispf:help-edit-segment-display-start (first segs)))
    (assert-equal 11 (lispf:help-edit-segment-display-end (first segs)))))

(define-test segments-with-link ()
  (let ((segs (lispf:parse-help-segments "See {edit:Editor} more.")))
    (assert-equal 3 (length segs))
    ;; "See "
    (assert-equal :text (lispf:help-edit-segment-kind (first segs)))
    (assert-equal 0 (lispf:help-edit-segment-display-start (first segs)))
    (assert-equal 4 (lispf:help-edit-segment-display-end (first segs)))
    ;; Link: "Editor"
    (assert-equal :link (lispf:help-edit-segment-kind (second segs)))
    (assert-string= "Editor" (lispf:help-edit-segment-text (second segs)))
    (assert-string= "edit" (lispf:help-edit-segment-target (second segs)))
    (assert-equal 4 (lispf:help-edit-segment-display-start (second segs)))
    (assert-equal 10 (lispf:help-edit-segment-display-end (second segs)))
    ;; " more."
    (assert-equal :text (lispf:help-edit-segment-kind (third segs)))
    (assert-equal 10 (lispf:help-edit-segment-display-start (third segs)))
    (assert-equal 16 (lispf:help-edit-segment-display-end (third segs)))))

(define-test segments-shorthand-link ()
  (let ((segs (lispf:parse-help-segments "Visit {help} now.")))
    (assert-equal 3 (length segs))
    (let ((link (second segs)))
      (assert-equal :link (lispf:help-edit-segment-kind link))
      (assert-string= "help" (lispf:help-edit-segment-text link))
      (assert-string= "help" (lispf:help-edit-segment-target link)))))

;;; ============================================================
;;; Help line edit reconstruction tests
;;; ============================================================

(define-test reconstruct-unchanged ()
  (let* ((raw "See {edit:Editor Help} for info.")
         (segs (lispf:parse-help-segments raw))
         (display (lispf:strip-help-markup raw))
         (result (lispf:reconstruct-help-line segs display)))
    (assert-string= raw result)))

(define-test reconstruct-changed-link-text ()
  (let* ((raw "See {edit:Editor Help} for info.")
         (segs (lispf:parse-help-segments raw))
         (new-display "See EDITOR HELP for info."))
    (assert-string= "See {edit:EDITOR HELP} for info."
                     (lispf:reconstruct-help-line segs new-display))))

(define-test reconstruct-changed-plain-text ()
  (let* ((raw "See {edit:Editor Help} for info.")
         (segs (lispf:parse-help-segments raw))
         (new-display "XXX Editor Help for info."))
    (assert-string= "XXX {edit:Editor Help} for info."
                     (lispf:reconstruct-help-line segs new-display))))

(define-test reconstruct-preserves-unchanged-shorthand ()
  (let* ((raw "Visit {help} now.")
         (segs (lispf:parse-help-segments raw))
         (display (lispf:strip-help-markup raw))
         (result (lispf:reconstruct-help-line segs display)))
    (assert-string= raw result)))

(define-test reconstruct-changed-shorthand-becomes-full ()
  (let* ((raw "Visit {help} now.")
         (segs (lispf:parse-help-segments raw))
         (new-display "Visit HELP now."))
    ;; Shorthand {help} becomes {help:HELP} when text changes
    (assert-string= "Visit {help:HELP} now."
                     (lispf:reconstruct-help-line segs new-display))))

;;; ============================================================
;;; apply-help-line-edit tests
;;; ============================================================

(define-test help-edit-preserves-link ()
  (let* ((raw "Hello {edit:test link} world.")
         (new-data (format nil "~72A" "Hello TEST LINK world."))
         (result (lispf:apply-help-line-edit raw 0 72 new-data)))
    (assert-string= "Hello {edit:TEST LINK} world." result)))

(define-test help-edit-no-change ()
  (let* ((raw "Hello {edit:test link} world.")
         (stripped (lispf:strip-help-markup raw))
         (new-data (format nil "~72A" stripped))
         (result (lispf:apply-help-line-edit raw 0 72 new-data)))
    (assert-string= raw result)))

(define-test help-edit-plain-text-only ()
  (let* ((raw "Just plain text.")
         (new-data (format nil "~72A" "JUST PLAIN TEXT."))
         (result (lispf:apply-help-line-edit raw 0 72 new-data)))
    (assert-string= "JUST PLAIN TEXT." result)))

;;; ============================================================
;;; Link editing command tests
;;; ============================================================

(define-test wrap-word-as-link-basic ()
  (let ((result (lispf:wrap-word-as-link "hello world" 6 "topic")))
    (assert-true result)
    (assert-true (search "{topic:world}" result))))

(define-test wrap-word-as-link-first-word ()
  (let ((result (lispf:wrap-word-as-link "hello world" 0 "greet")))
    (assert-true result)
    (assert-true (search "{greet:hello}" result))))

(define-test wrap-word-on-space-returns-nil ()
  (assert-nil (lispf:wrap-word-as-link "hello world" 5 "topic")))

(define-test wrap-word-past-end-returns-nil ()
  (assert-nil (lispf:wrap-word-as-link "hello" 10 "topic")))

(define-test wrap-word-already-linked-returns-nil ()
  (assert-nil (lispf:wrap-word-as-link "See {edit:Editor} here." 5 "other")))

(define-test remove-link-basic ()
  (let ((result (lispf:remove-link-at-col "See {edit:Editor} here." 5)))
    (assert-true result)
    (assert-string= "See Editor here." result)))

(define-test remove-link-preserves-other-links ()
  (let ((result (lispf:remove-link-at-col "{a:alpha} and {b:beta}" 11)))
    (assert-true result)
    (assert-true (search "{a:alpha}" result))
    (assert-true (not (search "{b:" result)))))

(define-test remove-link-no-link-at-col ()
  (assert-nil (lispf:remove-link-at-col "plain text here" 5)))

(define-test find-link-segment-at-col ()
  (let* ((segs (lispf:parse-help-segments "See {edit:Editor Help} more."))
         (found (lispf:find-link-segment-at-col segs 5)))
    (assert-true found)
    (assert-string= "edit" (lispf:help-edit-segment-target found))))

(define-test find-link-segment-on-plain-text ()
  (let* ((segs (lispf:parse-help-segments "See {edit:Editor Help} more."))
         (found (lispf:find-link-segment-at-col segs 1)))
    (assert-nil found)))

;;; ============================================================
;;; Editor WYSIWYG display tests
;;; ============================================================

(define-test help-file-detection ()
  (let ((s1 (make-help-session "line")))
    (assert-true (ed:help-file-p s1)))
  (let ((s2 (ed:make-test-session '("line"))))
    (setf (ed:editor-filepath s2) #P"/tmp/test.txt")
    (assert-nil (ed:help-file-p s2))))

(define-test wysiwyg-display-strips-markup ()
  (let ((s (make-help-session "Plain." "{edit:A Link} here." "More.")))
    (multiple-value-bind (prefix data) (lispf-editor::build-screen-data s)
      (declare (ignore prefix))
      (let ((data-lines (split-sequence:split-sequence #\Newline data)))
        ;; data-lines: [0]=TOF, [1]=Plain., [2]=A Link here., [3]=More., [4]=EOF, ...
        (assert-true (search "A Link here." (nth 2 data-lines))
                     "Link text should be visible")
        (assert-nil (search "{edit:" (nth 2 data-lines))
                    "Markup should be hidden")))))

(define-test wysiwyg-display-plain-file-unchanged ()
  (let ((s (ed:make-test-session '("Hello {world}"))))
    (setf (ed:editor-filepath s) #P"/tmp/test.txt")
    (multiple-value-bind (prefix data) (lispf-editor::build-screen-data s)
      (declare (ignore prefix))
      (let ((data-lines (split-sequence:split-sequence #\Newline data)))
        ;; In a .txt file, braces should NOT be stripped
        (assert-true (search "{world}" (nth 1 data-lines))
                     "Non-help files should show braces")))))

;;; ============================================================
;;; Help viewer state tests
;;; ============================================================

(define-test help-viewer-state-creation ()
  (let ((state (make-instance 'lispf::help-viewer-state)))
    (assert-nil (lispf::hv-topic state))
    (assert-nil (lispf::hv-history state))
    (assert-equal 0 (lispf::hv-scroll-offset state))))

(define-test help-viewer-load-topic ()
  (let ((path #P"/tmp/lispf-test-viewer.help")
        (state (make-instance 'lispf::help-viewer-state)))
    (unwind-protect
         (progn
           (write-help-file path "Viewer Test"
                            "Line one."
                            "Line {topic:two}.")
           (setf (lispf::hv-help-directories state)
                 (list #P"/tmp/"))
           (assert-true (lispf::load-help-topic state "lispf-test-viewer" :no-history t))
           (assert-string= "lispf-test-viewer" (lispf::hv-topic state))
           (assert-equal 2 (length (lispf::hv-rendered-lines state)))
           (assert-string= "Line one." (first (lispf::hv-rendered-lines state)))
           (assert-string= "Line two." (second (lispf::hv-rendered-lines state)))
           ;; Link map should have one entry
           (assert-equal 1 (length (lispf::hv-link-map state))))
      (ignore-errors (delete-file path)))))

(define-test help-viewer-history-stack ()
  (let ((dir #P"/tmp/")
        (state (make-instance 'lispf::help-viewer-state)))
    (setf (lispf::hv-help-directories state) (list dir))
    (unwind-protect
         (progn
           (write-help-file (merge-pathnames "hv-a.help" dir) "Page A" "Content A.")
           (write-help-file (merge-pathnames "hv-b.help" dir) "Page B" "Content B.")
           ;; Load first topic
           (lispf::load-help-topic state "hv-a" :no-history t)
           (assert-string= "hv-a" (lispf::hv-topic state))
           (assert-equal 0 (length (lispf::hv-history state)))
           ;; Load second topic (should push first to history)
           (lispf::load-help-topic state "hv-b")
           (assert-string= "hv-b" (lispf::hv-topic state))
           (assert-equal 1 (length (lispf::hv-history state)))
           (assert-string= "hv-a" (car (first (lispf::hv-history state)))))
      (ignore-errors (delete-file (merge-pathnames "hv-a.help" dir)))
      (ignore-errors (delete-file (merge-pathnames "hv-b.help" dir))))))

(define-test help-viewer-nonexistent-topic ()
  (let ((state (make-instance 'lispf::help-viewer-state)))
    (setf (lispf::hv-help-directories state) (list #P"/nonexistent/"))
    ;; find-help-file-in-directories should return nil for missing topic
    (assert-nil (lispf::find-help-file-in-directories "no-such-topic"
                                                      (lispf::hv-help-directories state)))))

(define-test help-viewer-scroll-offset ()
  (let ((dir #P"/tmp/")
        (state (make-instance 'lispf::help-viewer-state)))
    (setf (lispf::hv-help-directories state) (list dir))
    (unwind-protect
         (progn
           ;; Create a file with 30 lines (more than one page of 20)
           (apply #'write-help-file
                  (merge-pathnames "hv-scroll.help" dir) "Scroll Test"
                  (loop for i from 1 to 30 collect (format nil "Line ~D" i)))
           (lispf::load-help-topic state "hv-scroll" :no-history t)
           (assert-equal 30 (length (lispf::hv-rendered-lines state)))
           (assert-equal 0 (lispf::hv-scroll-offset state))
           ;; Simulate scroll down
           (setf (lispf::hv-scroll-offset state) 19)
           (assert-equal 19 (lispf::hv-scroll-offset state)))
      (ignore-errors (delete-file (merge-pathnames "hv-scroll.help" dir))))))

;;; ============================================================
;;; E2E help viewer tests (requires s3270)
;;; ============================================================

(define-test e2e-help-viewer-basic ()
  (let ((path #P"/tmp/lispf-e2e-helpview.txt"))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("test"))
           (with-test-app (s ed::*editor-app* :port 0)
             ;; Open a file
             (type-text s (namestring path))
             (press-enter s)
             (assert-screen-contains s "Size=")
             ;; PF1 should open help viewer
             (press-pf s 1)
             (assert-screen-contains s "LISPF Editor Help")
             ;; Dynamic area overlay arrives shortly after main screen
             ;; Should see link text without markup
             (assert-screen-contains s "Prefix Commands")
             ;; PF3 exits the help viewer subapp. The parent screen loop then
             ;; sends the editor screen. Wait(Unlock) may return before the
             ;; editor screen arrives, so poll until we see it.
             (press-pf-wait-screen s 3 "Size=")
             ;; Cancel and exit
             (move-cursor s 22 6)
             (type-text s "CANCEL")
             (press-enter s)))
      (ignore-errors (delete-file path)))))

(define-test e2e-help-viewer-link-navigation ()
  (let ((dir #P"/tmp/"))
    (unwind-protect
         (progn
           ;; Create two linked help files
           (write-help-file (merge-pathnames "lispf-e2e-helpnav.help" dir)
                            "Main Help"
                            "Go to {lispf-e2e-subtopic:Sub Topic} now.")
           (write-help-file (merge-pathnames "lispf-e2e-subtopic.help" dir)
                            "Sub Topic"
                            "This is the sub topic."
                            "Back to {lispf-e2e-helpnav:main}.")
           (ed:write-file-lines #P"/tmp/lispf-e2e-helpnav.txt" '("test"))
           (with-test-app (s ed::*editor-app* :port 0)
             ;; Open dummy file
             (type-text s "/tmp/lispf-e2e-helpnav.txt")
             (press-enter s)
             (assert-screen-contains s "Size=")
             ;; PF1 opens help viewer at the index
             (press-pf s 1)
             (assert-screen-contains s "Help Index")
             ;; Navigate to keys topic via command field
             (move-cursor s 21 14)
             (type-text s "keys")
             (press-enter s)
             (assert-screen-contains s "Page 1 of 1  (keys)")
             ;; PF3 back to index
             (press-pf s 3)
             (assert-screen-contains s "Page 1 of 1  (index)")
             ;; PF3 back to editor (no more history)
             (press-pf s 3)
             (assert-screen-contains s "Size=")
             ;; Clean up
             (move-cursor s 22 6)
             (type-text s "CANCEL")
             (press-enter s)))
      (ignore-errors (delete-file (merge-pathnames "lispf-e2e-helpnav.help" dir)))
      (ignore-errors (delete-file (merge-pathnames "lispf-e2e-subtopic.help" dir)))
      (ignore-errors (delete-file #P"/tmp/lispf-e2e-helpnav.txt")))))

(define-test e2e-help-viewer-index ()
  (let ((path #P"/tmp/lispf-e2e-helpidx.txt"))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("test"))
           (with-test-app (s ed::*editor-app* :port 0)
             (type-text s (namestring path))
             (press-enter s)
             (assert-screen-contains s "Size=")
             ;; PF1 opens help directly at the index
             (press-pf s 1)
             (assert-screen-contains s "Help Index")
             (assert-screen-contains s "Choose a topic")
             ;; Navigate to keys topic via command field
             (move-cursor s 21 14)
             (type-text s "keys")
             (press-enter s)
             (assert-screen-contains s "Function Keys")
             ;; PF1 from within help viewer should go back to index
             (press-pf-wait-screen s 1 "index")
             (assert-screen-contains s "Choose a topic")
             ;; PF3 back to keys topic (history)
             (press-pf s 3)
             (assert-screen-contains s "Function Keys")
             ;; PF4 returns directly to editor (skipping history)
             (press-pf-wait-screen s 4 "Size=")
             (move-cursor s 22 6)
             (type-text s "CANCEL")
             (press-enter s)))
      (ignore-errors (delete-file path)))))

;;; ============================================================
;;; E2E editor help file editing tests (requires s3270)
;;; ============================================================

(define-test e2e-help-edit-wysiwyg ()
  (let ((path #P"/tmp/lispf-e2e-helpedit.help"))
    (unwind-protect
         (progn
           (write-help-file path "Test Page"
                            "See {topic:a link} here."
                            "Plain text.")
           (with-test-app (s ed::*editor-app* :port 0)
             (type-text s (namestring path))
             (press-enter s)
             ;; Should see stripped content
             (assert-screen-contains s "a link")
             ;; Should NOT see raw markup
             (let ((full (format nil "~{~A~^~%~}" (screen-text s))))
               (assert-nil (search "{topic:" full)
                           "Markup should be hidden in display"))
             ;; Cancel
             (move-cursor s 22 6)
             (type-text s "CANCEL")
             (press-enter s)))
      (ignore-errors (delete-file path)))))

(define-test e2e-help-edit-link-command ()
  (let ((path #P"/tmp/lispf-e2e-helplink.help"))
    (unwind-protect
         (progn
           (write-help-file path "Test Page"
                            "Hello world here."
                            "Another line.")
           (with-test-app (s ed::*editor-app* :port 0)
             (type-text s (namestring path))
             (press-enter s)
             (assert-screen-contains s "Hello world here.")
             ;; Type LINK command targeting "world"
             (move-cursor s 22 6)
             (type-text s "LINK greet")
             ;; Position cursor on "world" (row 3 = first file line, col 13 = "world")
             (move-cursor s 3 13)
             (press-enter s)
             (assert-screen-contains s "Linked to greet")
             ;; Verify link exists via LINK query
             (move-cursor s 22 6)
             (erase-eof s)
             (type-text s "LINK")
             (move-cursor s 3 13)
             (press-enter s)
             (assert-screen-contains s "Link target: greet")
             ;; UNLINK it
             (move-cursor s 22 6)
             (erase-eof s)
             (type-text s "UNLINK")
             (move-cursor s 3 13)
             (press-enter s)
             (assert-screen-contains s "Link removed")
             ;; Verify link is gone
             (move-cursor s 22 6)
             (erase-eof s)
             (type-text s "LINK")
             (move-cursor s 3 13)
             (press-enter s)
             (assert-screen-contains s "No link at cursor")
             ;; Cancel
             (move-cursor s 22 6)
             (erase-eof s)
             (type-text s "CANCEL")
             (press-enter s)))
      (ignore-errors (delete-file path)))))

(define-test e2e-help-edit-inline-preserves-link ()
  (let ((path #P"/tmp/lispf-e2e-helpinline.help"))
    (unwind-protect
         (progn
           (write-help-file path "Test Page"
                            "See {topic:old text} end."
                            "Plain.")
           (with-test-app (s ed::*editor-app* :port 0)
             (type-text s (namestring path))
             (press-enter s)
             (assert-screen-contains s "old text")
             ;; File lines: 1=Test Page, 2=See old text end., 3=Plain.
             ;; Screen: row 2=TOF, row 3=line 1, row 4=line 2, row 5=line 3
             ;; "old" in "See old text end." starts at display col 4, screen col 7+4=11
             ;; Overtype "old text" with "NEW TEXT" on row 4
             (move-cursor s 4 11)
             (type-text s "NEW TEXT")
             ;; Move to command field before Enter to avoid auto-insert
             (move-cursor s 22 6)
             (press-enter s)
             ;; Should see updated text
             (assert-screen-contains s "NEW TEXT")
             ;; Verify link target preserved
             (move-cursor s 22 6)
             (type-text s "LINK")
             (move-cursor s 4 11)
             (press-enter s)
             (assert-screen-contains s "Link target: topic")
             ;; Save and verify file content
             (move-cursor s 22 6)
             (erase-eof s)
             (type-text s "SAVE")
             (press-enter s)
             ;; Read back the file and check raw content
             (let ((lines (ed:read-file-lines path)))
               (assert-true (search "{topic:NEW TEXT}" (second lines))
                            "Saved file should have link markup with new text"))
             ;; Exit
             (press-pf s 3)))
      (ignore-errors (delete-file path)))))

;;; ============================================================
;;; Runner
;;; ============================================================

(defun run-all ()
  (format t "~&;;; Running help system tests~%")
  (let ((*package* (find-package :lispf-help-tests)))
    (run-tests)))
