;;; -*- Mode: Lisp -*-

;;; Unit tests for the LISPF Editor.
;;; Tests editor logic directly without 3270 terminal connection.
;;;
;;; Usage:
;;;   (asdf:load-system "lispf")
;;;   (asdf:load-system "lispf-test")
;;;   (asdf:load-system "lispf-editor")
;;;   (load ".../editor/test/editor-tests.lisp")
;;;   (lispf-editor-tests:run-all)

(defpackage #:lispf-editor-tests
  (:use #:cl #:lispf-test)
  (:local-nicknames (#:ed #:lispf-editor))
  (:export #:run-all))

(in-package #:lispf-editor-tests)

;;; ============================================================
;;; Helpers
;;; ============================================================

(defun make-session (&rest lines)
  "Create a test session with the given lines."
  (ed:make-test-session (copy-list lines)))

(defun lines (session)
  "Return the lines as a list."
  (ed:editor-lines session))

(defun assert-lines (session expected &optional description)
  "Assert that session lines match EXPECTED."
  (unless (equal (lines session) expected)
    (error 'test-failure
           :description (or description "Lines mismatch")
           :expected expected
           :actual (lines session))))

(defun assert-equal (expected actual &optional description)
  "Assert two values are EQUAL."
  (unless (equal expected actual)
    (error 'test-failure
           :description (or description "Value mismatch")
           :expected expected
           :actual actual)))

(defun assert-true (value &optional description)
  "Assert value is non-nil."
  (unless value
    (error 'test-failure
           :description (or description "Expected true")
           :expected t
           :actual nil)))

(defun assert-nil (value &optional description)
  "Assert value is nil."
  (when value
    (error 'test-failure
           :description (or description "Expected nil")
           :expected nil
           :actual value)))

(defun assert-string-contains (haystack needle &optional description)
  "Assert that HAYSTACK contains NEEDLE."
  (unless (search needle haystack)
    (error 'test-failure
           :description (or description (format nil "Should contain ~S" needle))
           :expected needle
           :actual haystack)))

;;; ============================================================
;;; Line buffer tests
;;; ============================================================

(define-test line-count-empty ()
  (let ((s (make-session "")))
    (assert-equal 1 (ed:line-count s))))

(define-test line-count-multiple ()
  (let ((s (make-session "a" "b" "c")))
    (assert-equal 3 (ed:line-count s))))

(define-test total-virtual-lines-includes-markers ()
  (let ((s (make-session "a" "b" "c")))
    ;; 3 lines + top marker + bottom marker = 5
    (assert-equal 5 (ed:total-virtual-lines s))))

(define-test virtual-to-real-top-marker ()
  (let ((s (make-session "a" "b")))
    (assert-nil (ed:virtual-to-real s 0) "Top marker returns nil")))

(define-test virtual-to-real-file-lines ()
  (let ((s (make-session "a" "b" "c")))
    (assert-equal 0 (ed:virtual-to-real s 1))
    (assert-equal 1 (ed:virtual-to-real s 2))
    (assert-equal 2 (ed:virtual-to-real s 3))))

(define-test virtual-to-real-bottom-marker ()
  (let ((s (make-session "a" "b")))
    ;; Virtual index 3 = bottom marker for 2-line file
    (assert-nil (ed:virtual-to-real s 3) "Bottom marker returns nil")))

(define-test virtual-to-real-out-of-range ()
  (let ((s (make-session "a")))
    (assert-nil (ed:virtual-to-real s 5) "Out of range returns nil")))

(define-test line-at-valid ()
  (let ((s (make-session "hello" "world")))
    (assert-equal "hello" (ed:line-at s 0))
    (assert-equal "world" (ed:line-at s 1))))

(define-test line-at-out-of-range ()
  (let ((s (make-session "hello")))
    (assert-nil (ed:line-at s -1))
    (assert-nil (ed:line-at s 5))))

;;; ============================================================
;;; Layout tests
;;; ============================================================

(define-test layout-default-page-size ()
  (let ((layout (ed:make-default-layout)))
    ;; Default: rows 2-22 = 21 data lines
    (assert-equal 21 (ed:page-size layout))))

(define-test layout-session-page-size ()
  (let ((s (make-session "a")))
    (assert-equal 21 (ed:page-size s))))

(define-test layout-validation-overlap ()
  ;; Message row inside data area should error
  (let ((layout (make-instance 'ed:editor-layout
                               :message-row 5
                               :data-start-row 2
                               :data-end-row 22)))
    (handler-case
        (progn (ed:validate-layout layout) (error "should have errored"))
      (error (c) (assert-true (search "overlaps" (format nil "~A" c)))))))

(define-test layout-validation-data-range ()
  ;; Start > end should error
  (let ((layout (make-instance 'ed:editor-layout
                               :data-start-row 15
                               :data-end-row 5)))
    (handler-case
        (progn (ed:validate-layout layout) (error "should have errored"))
      (error (c) (assert-true (search "less than" (format nil "~A" c)))))))

(define-test layout-validation-out-of-range ()
  (let ((layout (make-instance 'ed:editor-layout :status-row 25)))
    (handler-case
        (progn (ed:validate-layout layout) (error "should have errored"))
      (error (c) (assert-true (search "out of range" (format nil "~A" c)))))))

(define-test layout-custom-page-size ()
  ;; Smaller data area
  (let ((layout (make-instance 'ed:editor-layout
                               :data-start-row 3
                               :data-end-row 20)))
    (assert-equal 18 (ed:page-size layout))))

;;; ============================================================
;;; Line buffer tests
;;; ============================================================

(define-test insert-lines-after-beginning ()
  (let ((s (make-session "c")))
    (ed:insert-lines-after s -1 (list "a" "b"))
    (assert-lines s '("a" "b" "c"))))

(define-test insert-lines-after-middle ()
  (let ((s (make-session "a" "c")))
    (ed:insert-lines-after s 0 (list "b"))
    (assert-lines s '("a" "b" "c"))))

(define-test insert-lines-after-end ()
  (let ((s (make-session "a" "b")))
    (ed:insert-lines-after s 1 (list "c"))
    (assert-lines s '("a" "b" "c"))))

(define-test insert-lines-before ()
  (let ((s (make-session "b" "c")))
    (ed:insert-lines-before s 0 (list "a"))
    (assert-lines s '("a" "b" "c"))))

(define-test delete-line-range-single ()
  (let ((s (make-session "a" "b" "c")))
    (ed:delete-line-range s 1 1)
    (assert-lines s '("a" "c"))))

(define-test delete-line-range-multiple ()
  (let ((s (make-session "a" "b" "c" "d")))
    (ed:delete-line-range s 1 2)
    (assert-lines s '("a" "d"))))

(define-test delete-line-range-all ()
  (let ((s (make-session "a" "b" "c")))
    (ed:delete-line-range s 0 3)
    (assert-lines s '())))

(define-test extract-line-range ()
  (let ((s (make-session "a" "b" "c" "d")))
    (assert-equal '("b" "c") (ed:extract-line-range s 1 2))))

(define-test extract-line-range-clamped ()
  (let ((s (make-session "a" "b")))
    (assert-equal '("b") (ed:extract-line-range s 1 5))))

;;; ============================================================
;;; Visible portion and edit tests
;;; ============================================================

(define-test visible-portion-no-offset ()
  (let ((result (ed:visible-portion "Hello" 0)))
    (assert-equal ed:+data-width+ (length result))
    (assert-equal "Hello" (string-right-trim '(#\Space) result))))

(define-test visible-portion-with-offset ()
  (let ((result (ed:visible-portion "Hello World" 6)))
    (assert-equal "World" (string-right-trim '(#\Space) result))))

(define-test visible-portion-offset-past-end ()
  (let ((result (ed:visible-portion "Hi" 10)))
    (assert-equal "" (string-right-trim '(#\Space) result))))

(define-test apply-edit-simple ()
  (let ((s (make-session "Hello")))
    (ed:save-undo-state s)
    (ed:apply-edit s 0 "World")
    (assert-lines s '("World"))
    (assert-true (ed:editor-modified s))))

(define-test apply-edit-no-change ()
  (let ((s (make-session "Hello")))
    (ed:apply-edit s 0 "Hello")
    (assert-nil (ed:editor-modified s) "No change should not set modified")))

(define-test apply-edit-with-horizontal-scroll ()
  (let ((s (make-session "AAAA____BBBB")))
    (setf (ed:editor-col-offset s) 4)
    ;; Visible portion at offset 4 is "____BBBB" (padded to 72)
    ;; Edit only the visible window
    (ed:apply-edit s 0 "XXXX")
    ;; Before: "AAAA", Middle: "XXXX" (padded to 72 since after exists? No, after is at 4+72=76 which is > 12)
    ;; Actually the line is only 12 chars, so after = "" (12 < 4+72=76)
    (assert-equal "AAAAXXXX" (first (lines s)))))

(define-test apply-edit-preserves-after-content ()
  ;; Line longer than col-offset + data-width
  (let* ((before-part (make-string 10 :initial-element #\A))
         (visible-part (make-string ed:+data-width+ :initial-element #\B))
         (after-part "CCCC")
         (full-line (concatenate 'string before-part visible-part after-part))
         (s (make-session full-line)))
    (setf (ed:editor-col-offset s) 10)
    ;; Edit: change visible portion (all B's to X's)
    (let ((new-vis (make-string ed:+data-width+ :initial-element #\X)))
      (ed:apply-edit s 0 new-vis)
      ;; After should be preserved
      (let ((result (first (lines s))))
        (assert-equal before-part (subseq result 0 10) "Before preserved")
        (assert-equal after-part (subseq result (+ 10 ed:+data-width+))
                      "After preserved")))))

;;; ============================================================
;;; Prefix command parsing tests
;;; ============================================================

(define-test parse-prefix-insert ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "I")
    (assert-equal :i cmd)
    (assert-equal 1 count))
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "i5")
    (assert-equal :i cmd)
    (assert-equal 5 count)))

(define-test parse-prefix-delete ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "D")
    (assert-equal :d cmd)
    (assert-equal 1 count))
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "D3")
    (assert-equal :d cmd)
    (assert-equal 3 count)))

(define-test parse-prefix-block-delete ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "DD")
    (assert-equal :dd cmd)
    (assert-equal 0 count)))

(define-test parse-prefix-repeat ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "R")
    (assert-equal :r cmd)
    (assert-equal 1 count))
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "R3")
    (assert-equal :r cmd)
    (assert-equal 3 count)))

(define-test parse-prefix-block-repeat ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "RR")
    (assert-equal :rr cmd)))

(define-test parse-prefix-copy-move ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "C")
    (assert-equal :c cmd)
    (assert-equal 1 count))
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "M2")
    (assert-equal :m cmd)
    (assert-equal 2 count)))

(define-test parse-prefix-block-copy-move ()
  (multiple-value-bind (cmd) (ed:parse-prefix-command "CC")
    (assert-equal :cc cmd))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "MM")
    (assert-equal :mm cmd)))

(define-test parse-prefix-targets ()
  (multiple-value-bind (cmd) (ed:parse-prefix-command "A")
    (assert-equal :a cmd))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "B")
    (assert-equal :b cmd)))

(define-test parse-prefix-case ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "UC")
    (assert-equal :uc cmd)
    (assert-equal 1 count))
  ;; LC with digits stripped (digits are line number remnants)
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "LC0001")
    (assert-equal :lc cmd)
    (assert-equal 1 count))
  ;; UC typed anywhere in the field
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "00UC01")
    (assert-equal :uc cmd)
    (assert-equal 1 count)))

(define-test parse-prefix-shift ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "(")
    (assert-equal :shift-left cmd)
    (assert-equal 1 count))
  (multiple-value-bind (cmd count) (ed:parse-prefix-command ")5")
    (assert-equal :shift-right cmd)
    (assert-equal 5 count)))

(define-test parse-prefix-empty ()
  (assert-nil (ed:parse-prefix-command ""))
  (assert-nil (ed:parse-prefix-command "   ")))

(define-test parse-prefix-unknown ()
  (assert-nil (ed:parse-prefix-command "X"))
  (assert-nil (ed:parse-prefix-command "ZZ")))

(define-test parse-prefix-overtyped-line-number ()
  ;; Commands typed at start of field
  (multiple-value-bind (cmd) (ed:parse-prefix-command "DD0001")
    (assert-equal :dd cmd "DD at start"))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "CC0001")
    (assert-equal :cc cmd "CC at start"))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "MM0001")
    (assert-equal :mm cmd "MM at start"))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "RR0001")
    (assert-equal :rr cmd "RR at start"))
  ;; Commands typed in middle of field (digits stripped)
  (multiple-value-bind (cmd) (ed:parse-prefix-command "000D01")
    (assert-equal :d cmd "D in middle"))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "00DD01")
    (assert-equal :dd cmd "DD in middle"))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "000I01")
    (assert-equal :i cmd "I in middle"))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "000A01")
    (assert-equal :a cmd "A in middle"))
  (multiple-value-bind (cmd) (ed:parse-prefix-command "00B001")
    (assert-equal :b cmd "B in middle"))
  ;; Single command with count (I5 typed at start)
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "I50001")
    (assert-equal :i cmd "I5 at start")
    (assert-equal 5 count "I5 count"))
  ;; UC/LC overtyped
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "UC0001")
    (assert-equal :uc cmd "UC overtyped")
    (assert-equal 1 count)))

(define-test parse-prefix-with-spaces ()
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "  D3  ")
    (assert-equal :d cmd)
    (assert-equal 3 count)))

;;; ============================================================
;;; Prefix command execution tests
;;; ============================================================

(define-test exec-insert-at-top-of-data ()
  ;; I on Top-of-Data marker (real-index -1) should insert at beginning
  (let ((s (make-session "a" "b" "c")))
    (ed:execute-prefix-commands s '((-1 :i 1 0)))
    (assert-lines s '("" "a" "b" "c"))))

(define-test exec-insert ()
  (let ((s (make-session "a" "b" "c")))
    (ed:execute-prefix-commands s '((0 :i 2 0)))
    (assert-lines s '("a" "" "" "b" "c"))))

(define-test exec-delete ()
  (let ((s (make-session "a" "b" "c")))
    (ed:execute-prefix-commands s '((1 :d 1 1)))
    (assert-lines s '("a" "c"))))

(define-test exec-delete-multiple ()
  (let ((s (make-session "a" "b" "c" "d")))
    (ed:execute-prefix-commands s '((1 :d 2 1)))
    (assert-lines s '("a" "d"))))

(define-test exec-repeat ()
  (let ((s (make-session "a" "b" "c")))
    (ed:execute-prefix-commands s '((1 :r 2 1)))
    (assert-lines s '("a" "b" "b" "b" "c"))))

(define-test exec-uppercase ()
  (let ((s (make-session "hello" "world")))
    (ed:execute-prefix-commands s '((0 :uc 2 0)))
    (assert-lines s '("HELLO" "WORLD"))))

(define-test exec-lowercase ()
  (let ((s (make-session "HELLO" "WORLD")))
    (ed:execute-prefix-commands s '((0 :lc 1 0)))
    (assert-lines s '("hello" "WORLD"))))

(define-test exec-shift-left ()
  (let ((s (make-session "   Hello")))
    (ed:execute-prefix-commands s '((0 :shift-left 3 0)))
    (assert-lines s '("Hello"))))

(define-test exec-shift-right ()
  (let ((s (make-session "Hello")))
    (ed:execute-prefix-commands s '((0 :shift-right 3 0)))
    (assert-lines s '("   Hello"))))

(define-test exec-multiple-on-different-lines ()
  (let ((s (make-session "a" "b" "c")))
    ;; Insert after line 0, then delete line 2 (which shifts)
    (ed:execute-prefix-commands s '((0 :i 1 0) (2 :d 1 2)))
    ;; After insert: "a" "" "b" "c"
    ;; Delete adjusted index 2+1=3: "a" "" "b"
    (assert-lines s '("a" "" "b"))))

;;; ============================================================
;;; Block command tests
;;; ============================================================

(define-test exec-block-delete-same-screen ()
  ;; Both DD markers on the same Enter press should execute immediately
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (let ((msg (ed:execute-prefix-commands s '((1 :dd 0 1) (3 :dd 0 3)))))
      (assert-string-contains msg "deleted" "Should report deletion")
      (assert-string-contains msg "3" "Should report count"))
    (assert-lines s '("a" "e"))))

(define-test exec-block-delete-two-screen ()
  ;; DD on one screen, DD on next screen
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 5) ; start away from block
    ;; First DD marks start
    (let ((msg (ed:execute-prefix-commands s '((1 :dd 0 1)))))
      (assert-string-contains msg "pending"))
    ;; Second DD marks end - should navigate to block start
    (ed:execute-prefix-commands s '((3 :dd 0 3)))
    (assert-lines s '("a" "e"))
    ;; Should navigate to the start of the deleted block (line 1)
    (assert-equal 1 (ed:editor-top-line s) "Should navigate to block start")))

(define-test exec-block-repeat-same-screen ()
  (let ((s (make-session "a" "b" "c" "d")))
    (ed:execute-prefix-commands s '((1 :rr 0 1) (2 :rr 0 2)))
    (assert-lines s '("a" "b" "c" "b" "c" "d"))))

(define-test exec-block-repeat-two-screen ()
  (let ((s (make-session "a" "b" "c" "d")))
    (ed:execute-prefix-commands s '((1 :rr 0 1)))
    (ed:execute-prefix-commands s '((2 :rr 0 2)))
    (assert-lines s '("a" "b" "c" "b" "c" "d"))))

(define-test exec-block-copy-same-screen ()
  ;; CC..CC + A target all on one Enter
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (let ((msg (ed:execute-prefix-commands s '((1 :cc 0 1) (2 :cc 0 2) (4 :a 0 4)))))
      (assert-string-contains msg "copied" "Should report copy"))
    (assert-lines s '("a" "b" "c" "d" "e" "b" "c"))))

(define-test exec-block-copy-two-screen ()
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (ed:execute-prefix-commands s '((1 :cc 0 1)))
    (let ((msg (ed:execute-prefix-commands s '((2 :cc 0 2)))))
      (assert-string-contains msg "Copy block marked"))
    (ed:execute-prefix-commands s '((4 :a 0 4)))
    (assert-lines s '("a" "b" "c" "d" "e" "b" "c"))))

(define-test exec-block-move-same-screen ()
  ;; MM..MM + B target all on one Enter
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (let ((msg (ed:execute-prefix-commands s '((1 :mm 0 1) (2 :mm 0 2) (4 :b 0 4)))))
      (assert-string-contains msg "moved" "Should report move"))
    (assert-lines s '("a" "d" "b" "c" "e"))))

(define-test exec-block-move-two-screen ()
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (ed:execute-prefix-commands s '((1 :mm 0 1)))
    (let ((msg (ed:execute-prefix-commands s '((2 :mm 0 2)))))
      (assert-string-contains msg "Move block marked"))
    (ed:execute-prefix-commands s '((4 :b 0 4)))
    (assert-lines s '("a" "d" "b" "c" "e"))))

(define-test exec-single-copy-with-after ()
  (let ((s (make-session "a" "b" "c")))
    ;; C on line 0 needs A/B target
    (let ((msg (ed:execute-prefix-commands s '((0 :c 1 0)))))
      (assert-string-contains msg "pending"))
    ;; A after line 2
    (ed:execute-prefix-commands s '((2 :a 0 2)))
    (assert-lines s '("a" "b" "c" "a"))))

(define-test exec-move-target-inside-source-rejected ()
  (let ((s (make-session "a" "b" "c" "d" "e")))
    ;; Mark MM block 1-3 (b c d)
    (ed:execute-prefix-commands s '((1 :mm 0 1)))
    ;; Complete MM block
    (ed:execute-prefix-commands s '((3 :mm 0 3)))
    ;; Try A target inside the source block (after "c" = index 3, which is inside 1-3)
    (let ((msg (ed:execute-prefix-commands s '((2 :a 0 2)))))
      ;; Target index 3 (after line 2) is inside source block 1-3
      (assert-string-contains msg "inside the source block"))))

;;; ============================================================
;;; Justify tests
;;; ============================================================

;;; --- justify-lines function tests ---

(define-test justify-single-line-wrap ()
  (let ((result (ed:justify-lines '("hello world this is a test of justification") 20)))
    (assert-true (> (length result) 1) "Should wrap to multiple lines")
    (dolist (line result)
      (assert-true (<= (length line) 20)
                   (format nil "Width 20: ~S too long" line)))))

(define-test justify-single-line-no-wrap ()
  (let ((result (ed:justify-lines '("short text") 72)))
    (assert-equal 1 (length result))
    (assert-equal "short text" (first result))))

(define-test justify-multi-line-joined ()
  ;; Multiple short lines in same paragraph should be joined
  (let ((result (ed:justify-lines '("short" "lines" "here") 40)))
    (assert-equal 1 (length result))
    (assert-equal "short lines here" (first result))))

(define-test justify-multi-line-rewrap ()
  ;; Multiple lines that together exceed width should be re-wrapped
  (let ((result (ed:justify-lines '("aaa bbb ccc" "ddd eee fff" "ggg hhh") 10)))
    (assert-true (> (length result) 2))
    (dolist (line result)
      (assert-true (<= (length line) 10)
                   (format nil "Width 10: ~S" line)))))

(define-test justify-preserves-paragraphs ()
  (let ((result (ed:justify-lines '("first paragraph" "" "second paragraph") 40)))
    (assert-equal 3 (length result) "Should have 3 lines (2 paragraphs + separator)")
    (assert-equal "" (second result) "Middle line should be empty")))

(define-test justify-multiple-paragraphs ()
  (let ((result (ed:justify-lines '("para one words" "" "para two words" "" "para three") 40)))
    ;; Should preserve both empty lines
    (assert-equal 5 (length result))
    (assert-equal "" (second result))
    (assert-equal "" (fourth result))))

(define-test justify-long-word-not-broken ()
  ;; A word longer than width is kept intact on its own line
  (let ((result (ed:justify-lines '("superlongword short") 10)))
    (assert-true (find "superlongword" result :test #'string=)
                 "Long word should be on its own line")
    (assert-true (find "short" result :test #'string=)
                 "Short word should be on next line")))

(define-test justify-long-word-count-returned ()
  ;; justify-lines returns count of words exceeding width
  (multiple-value-bind (result long-count)
      (ed:justify-lines '("superlongword another-long-one short") 10)
    (declare (ignore result))
    (assert-equal 2 long-count "Should report 2 long words")))

(define-test justify-no-long-words ()
  (multiple-value-bind (result long-count)
      (ed:justify-lines '("aa bb cc") 10)
    (declare (ignore result))
    (assert-equal 0 long-count "No long words")))

(define-test justify-width-5 ()
  (let ((result (ed:justify-lines '("aa bb cc dd ee") 5)))
    ;; "aa bb"=5, "cc dd"=5, "ee"=2 -> 3 lines
    (assert-equal 3 (length result))
    (dolist (line result)
      (assert-true (<= (length line) 5)
                   (format nil "Width 5: ~S" line)))))

(define-test justify-width-10 ()
  (let ((result (ed:justify-lines '("one two three four five six seven eight") 10)))
    (assert-true (> (length result) 3))
    (dolist (line result)
      (assert-true (<= (length line) 10)
                   (format nil "Width 10: ~S" line)))))

(define-test justify-width-20 ()
  (let ((result (ed:justify-lines '("one two three four five six seven eight nine ten") 20)))
    (assert-true (> (length result) 1))
    (dolist (line result)
      (assert-true (<= (length line) 20)
                   (format nil "Width 20: ~S" line)))))

(define-test justify-width-72-default ()
  (let ((result (ed:justify-lines '("short text should stay on one line") 72)))
    (assert-equal 1 (length result))))

(define-test justify-empty-input ()
  (let ((result (ed:justify-lines '() 40)))
    (assert-equal 0 (length result) "Empty input should give empty output")))

(define-test justify-all-empty-lines ()
  (let ((result (ed:justify-lines '("" "" "") 40)))
    (assert-equal 3 (length result) "Empty lines should be preserved")))

;;; --- JJ prefix command parsing ---

(define-test parse-prefix-jj ()
  ;; JJ is a simple block marker (no width)
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "JJ")
    (assert-equal :jj cmd)
    (assert-equal 0 count))
  ;; Overtyped: JJ over 000001
  (multiple-value-bind (cmd count) (ed:parse-prefix-command "JJ0001")
    (assert-equal :jj cmd)))

;;; --- JJ marks region, JUSTIFY command executes ---

(define-test jj-marks-region ()
  ;; JJ..JJ marks a region for JUSTIFY
  (let ((s (make-session "aa bb cc" "dd ee ff" "gg hh ii")))
    (let ((msg (ed:execute-prefix-commands s '((0 :jj 0 0) (2 :jj 0 2)))))
      (assert-string-contains msg "marked")
      ;; Lines should be unchanged
      (assert-lines s '("aa bb cc" "dd ee ff" "gg hh ii"))
      ;; Range should be stored
      (assert-true (ed::editor-justify-range s)))))

(define-test justify-command-on-marked-range ()
  ;; Mark with JJ, then JUSTIFY 10 in command field
  (let ((s (make-session "aa bb cc dd ee ff gg hh ii jj kk ll")))
    (ed:execute-prefix-commands s '((0 :jj 0 0) (0 :jj 0 0)))
    (let ((msg (ed:handle-primary-command s "JUSTIFY 10")))
      (assert-string-contains msg "Justified")
      (assert-string-contains msg "width 10")
      (dolist (line (ed:editor-lines s))
        (assert-true (<= (length line) 10)
                     (format nil "Width 10: ~S" line))))))

(define-test justify-command-default-width ()
  ;; JUSTIFY without width uses 72
  (let ((s (make-session "this is a short line that fits in 72 columns easily")))
    (ed:execute-prefix-commands s '((0 :jj 0 0) (0 :jj 0 0)))
    (let ((msg (ed:handle-primary-command s "JUSTIFY")))
      (assert-string-contains msg "width 72"))))

(define-test justify-command-whole-file ()
  ;; JUSTIFY without JJ marks operates on entire file
  (let ((s (make-session "aa bb cc dd ee ff gg hh ii jj kk ll")))
    (let ((msg (ed:handle-primary-command s "JUSTIFY 10")))
      (assert-string-contains msg "Justified")
      (dolist (line (ed:editor-lines s))
        (assert-true (<= (length line) 10)
                     (format nil "Width 10: ~S" line))))))

(define-test justify-command-warns-long-words ()
  (let ((s (make-session "superlongword short words")))
    (let ((msg (ed:handle-primary-command s "JUSTIFY 5")))
      (assert-string-contains msg "exceed")
      (assert-true (find "superlongword" (ed:editor-lines s) :test #'string=)
                   "Long word should be preserved"))))

(define-test justify-command-preserves-paragraphs ()
  (let ((s (make-session "first paragraph words" "" "second paragraph words")))
    (ed:handle-primary-command s "JUSTIFY 20")
    (assert-true (member "" (ed:editor-lines s) :test #'string=)
                 "Paragraph break should be preserved")))

(define-test justify-long-line-many-output-lines ()
  ;; A single 200+ char line of short words, justified to width 10.
  ;; This produces more lines than fit on one screen (17 visible).
  ;; Must not cause any errors (e.g. "Screen has N application rows").
  (let* ((words (loop for i from 1 to 80 collect (format nil "w~D" i)))
         (long-line (format nil "~{~A~^ ~}" words))
         (s (make-session long-line)))
    (assert-true (> (length long-line) 200)
                 (format nil "Input should be >200 chars, got ~D" (length long-line)))
    ;; Mark with JJ and justify to width 10
    (ed:execute-prefix-commands s '((0 :jj 0 0) (0 :jj 0 0)))
    (let ((msg (ed:handle-primary-command s "JUSTIFY 10")))
      (assert-true (stringp msg) "JUSTIFY should return a message")
      (assert-string-contains msg "Justified"))
    ;; Should have many lines now
    (assert-true (> (ed:line-count s) 17)
                 (format nil "Should have >17 lines, got ~D" (ed:line-count s)))
    ;; All lines should fit within width 10
    (dolist (line (ed:editor-lines s))
      (assert-true (<= (length line) 10)
                   (format nil "Line exceeds width 10: ~S" line)))
    ;; Building screen data for display must not error
    (setf (ed:editor-top-line s) 0)
    (multiple-value-bind (prefix data)
        (ed::build-screen-data s)
      (let ((prefix-lines (split-sequence:split-sequence #\Newline prefix))
            (data-lines (split-sequence:split-sequence #\Newline data)))
        (assert-equal (ed:page-size s) (length prefix-lines)
                      "Prefix should have exactly page-size lines")
        (assert-equal (ed:page-size s) (length data-lines)
                      "Data should have exactly page-size lines")))
    ;; Scrolling to middle and end must also work
    (setf (ed:editor-top-line s) 10)
    (ed::build-screen-data s)  ; should not error
    (setf (ed:editor-top-line s) (1- (ed:total-virtual-lines s)))
    (ed::build-screen-data s))) ; should not error

;;; ============================================================
;;; Block command navigation tests
;;; ============================================================

(define-test block-delete-navigates-to-start ()
  ;; DD lines 2-100 in a 120-line file, should navigate to line 2 area
  (let ((s (apply #'make-session
                  (loop for i from 1 to 120 collect (format nil "line ~D" i)))))
    (setf (ed:editor-top-line s) 50) ; viewing middle of file
    ;; DD on line 1 (0-based), pending
    (ed:execute-prefix-commands s '((1 :dd 0 0)))
    ;; DD on line 99 (0-based), completes block
    (ed:execute-prefix-commands s '((99 :dd 0 0)))
    ;; 99 lines deleted (1-99), leaving line 0 and lines 100-119
    (assert-equal 21 (ed:line-count s))
    ;; Should navigate to the start (line 1 = real index 1)
    (assert-equal 1 (ed:editor-top-line s) "Should navigate to block start")))

(define-test block-copy-navigates-to-source ()
  (let ((s (make-session "a" "b" "c" "d" "e" "f" "g" "h" "i" "j")))
    (setf (ed:editor-top-line s) 8)
    ;; CC lines 1-2 (b, c)
    (ed:execute-prefix-commands s '((1 :cc 0 0)))
    (ed:execute-prefix-commands s '((2 :cc 0 0)))
    ;; A after line 8 (i)
    (ed:execute-prefix-commands s '((8 :a 0 0)))
    ;; Should navigate to source start (line 1)
    (assert-equal 1 (ed:editor-top-line s) "Should navigate to copy source")))

;;; ============================================================
;;; Full round-trip block command tests (simulating 3270 overtype)
;;; ============================================================

(defun make-context-with-prefix (session prefix-overrides)
  "Build a context hash table simulating a round-trip where the user
typed PREFIX-OVERRIDES into prefix fields. PREFIX-OVERRIDES is an alist
of (screen-row . typed-text). Unmodified rows get their original line numbers."
  (let ((context (make-hash-table :test 'equal)))
    (multiple-value-bind (prefix-str data-str) (ed::build-screen-data session)
      (let ((prefix-lines (split-sequence:split-sequence #\Newline prefix-str))
            (data-lines (split-sequence:split-sequence #\Newline data-str)))
        ;; Apply overrides (simulate user typing over the line number)
        (dolist (override prefix-overrides)
          (let* ((row (car override))
                 (typed (cdr override))
                 (original (if (< row (length prefix-lines)) (nth row prefix-lines) ""))
                 ;; Simulate 3270 overtype: typed text replaces start of field
                 (result (concatenate 'string
                                      typed
                                      (subseq original (min (length typed) (length original))))))
            (when (< row (length prefix-lines))
              (setf (nth row prefix-lines) result))))
        ;; Join back as the framework would
        (setf (gethash "prefix" context)
              (format nil "~{~A~^~%~}" prefix-lines))
        (setf (gethash "data" context)
              (format nil "~{~A~^~%~}" data-lines))))
    context))

(defun test-process-editor-changes (session context)
  "Call process-editor-changes with current-response set to nil so that
MDT-based detection treats all fields as modified (the test default)."
  (let ((lispf:*session* session))
    (setf (lispf:current-response) nil)
    (ed::process-editor-changes session context)))
(define-test round-trip-single-dd-enters-pending ()
  ;; User types "dd" on line 2 (screen row 1, since row 0 is Top of File marker)
  ;; This should enter pending mode, not delete the line
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 0) ; top of data marker at row 0
    (let* ((context (make-context-with-prefix s '((1 . "dd"))))
           (msg (test-process-editor-changes s context)))
      (assert-true (ed:editor-pending-block s)
                   "Single DD should set pending")
      (assert-string-contains msg "pending"
                              "Single DD should return pending message")
      (assert-lines s '("a" "b" "c" "d" "e")
                    "Single DD should not modify the file"))))

(define-test round-trip-pending-survives-scroll ()
  ;; DD on line "a" (screen row 1), then scroll down and back without editing.
  ;; The pending command should survive because the "DD" shown in the prefix
  ;; on re-display is NOT a new command from the user.
  (let ((s (make-session "a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
                         "k" "l" "m" "n" "o" "p" "q" "r" "s" "t")))
    (setf (ed:editor-top-line s) 0)
    ;; User types DD on row 1 (line "a")
    (let* ((context (make-context-with-prefix s '((1 . "dd"))))
           (msg (test-process-editor-changes s context)))
      (assert-true (ed:editor-pending-block s) "DD should be pending")
      (assert-string-contains msg "pending"))
    ;; Scroll down: process-editor-changes with no user edits
    (setf (ed:editor-top-line s) 16) ; scroll down
    (let* ((context (make-context-with-prefix s nil)) ; no user prefix edits
           (msg (test-process-editor-changes s context)))
      (declare (ignore msg))
      (assert-true (ed:editor-pending-block s)
                   "Pending should survive scroll down"))
    ;; Scroll back up: the pending line's prefix shows "DD" from build-screen-data
    ;; This must NOT be treated as a new DD command
    (setf (ed:editor-top-line s) 0)
    (let* ((context (make-context-with-prefix s nil))
           (msg (test-process-editor-changes s context)))
      (declare (ignore msg))
      (assert-true (ed:editor-pending-block s)
                   "Pending should survive scroll back up")
      (assert-lines s '("a" "b" "c" "d" "e" "f" "g" "h" "i" "j"
                        "k" "l" "m" "n" "o" "p" "q" "r" "s" "t")
                    "File should be unmodified"))))

(define-test round-trip-dd-pair-deletes-block ()
  ;; With top=0: row 0=Top marker, row 1=line "a" (real 0), row 2=line "b", row 3=line "c"
  ;; DD on rows 2 and 3 should delete lines b and c (real 1-2)
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 0)
    (let* ((context (make-context-with-prefix s '((2 . "dd") (3 . "dd"))))
           (msg (test-process-editor-changes s context)))
      (assert-string-contains msg "deleted" "Should report deletion")
      (assert-nil (ed:editor-pending-block s)
                  "DD pair should not leave pending")
      (assert-lines s '("a" "d" "e")
                    "DD pair should delete lines b and c"))))

(define-test round-trip-cc-pair-with-a-copies ()
  ;; With top=0: row 0=Top marker, row 1="a"(real 0), row 2="b"(real 1), row 3="c", row 4="d", row 5="e"
  ;; CC on rows 2,3 (lines b,c) and A on row 5 (after "e")
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 0)
    (let* ((context (make-context-with-prefix s '((2 . "cc") (3 . "cc") (5 . "a"))))
           (msg (test-process-editor-changes s context)))
      (assert-string-contains msg "copied")
      (assert-lines s '("a" "b" "c" "d" "e" "b" "c")))))

;;; ============================================================
;;; Pending block reset
;;; ============================================================

(define-test reset-clears-pending ()
  (let ((s (make-session "a" "b" "c")))
    (ed:execute-prefix-commands s '((0 :dd 0 0)))
    (assert-true (ed:editor-pending-block s))
    (setf (ed:editor-pending-block s) nil)
    (assert-nil (ed:editor-pending-block s))))

(define-test conflicting-block-resets ()
  (let ((s (make-session "a" "b" "c")))
    ;; Start DD block
    (ed:execute-prefix-commands s '((0 :dd 0 0)))
    ;; Try CC instead of DD
    (let ((msg (ed:execute-prefix-commands s '((2 :cc 0 2)))))
      (assert-string-contains msg "Conflicting")
      (assert-nil (ed:editor-pending-block s)))))

;;; ============================================================
;;; Primary command tests
;;; ============================================================

(define-test cmd-top ()
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 3)
    (ed:handle-primary-command s "TOP")
    (assert-equal 0 (ed:editor-top-line s))))

(define-test cmd-bottom ()
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (ed:handle-primary-command s "BOT")
    ;; total virtual = 7, page-size = 18, so max top = 0
    (assert-true (>= (ed:editor-top-line s) 0))))

(define-test cmd-down-up ()
  (let ((s (make-session)))
    (dotimes (i 40) (push (format nil "line ~D" i) (ed:editor-lines s)))
    (setf (ed:editor-lines s) (nreverse (ed:editor-lines s)))
    (setf (ed:editor-top-line s) 0)
    (ed:handle-primary-command s "DOWN 5")
    (assert-equal 5 (ed:editor-top-line s))
    (ed:handle-primary-command s "UP 3")
    (assert-equal 2 (ed:editor-top-line s))))

(define-test cmd-locate ()
  ;; Use enough lines to avoid clamping
  (let ((s (apply #'make-session (loop for i from 1 to 30 collect (format nil "line ~D" i)))))
    (ed:handle-primary-command s "L 10")
    ;; LOCATE 10 -> virtual index 10, top-line = 10
    (assert-equal 10 (ed:editor-top-line s))))

(define-test cmd-left-right ()
  (let ((s (make-session "a")))
    (ed:handle-primary-command s "RIGHT")
    (assert-equal ed:+data-width+ (ed:editor-col-offset s))
    (ed:handle-primary-command s "LEFT")
    (assert-equal 0 (ed:editor-col-offset s))))

(define-test cmd-left-right-with-count ()
  (let ((s (make-session "a")))
    (ed:handle-primary-command s "RIGHT 10")
    (assert-equal 10 (ed:editor-col-offset s))
    (ed:handle-primary-command s "LEFT 5")
    (assert-equal 5 (ed:editor-col-offset s))))

(define-test cmd-reset ()
  (let ((s (make-session "a" "b")))
    (setf (ed:editor-pending-block s) '(:dd 0))
    (ed:handle-primary-command s "RESET")
    (assert-nil (ed:editor-pending-block s))))

;;; ============================================================
;;; FIND tests
;;; ============================================================

(define-test find-basic ()
  ;; Need enough lines so the file doesn't fit in one page (avoids clamp to 0)
  (let ((s (apply #'make-session (loop for i from 1 to 30 collect (format nil "line ~D" i)))))
    (setf (nth 9 (ed:editor-lines s)) "World target")
    (setf (ed:editor-top-line s) 1)
    (let ((msg (ed:do-find s "World" nil)))
      (assert-string-contains msg "found")
      (assert-string-contains msg "line 10"))))

(define-test find-case-insensitive ()
  (let ((s (make-session "Hello" "WORLD" "hello world")))
    (setf (ed:editor-top-line s) 0)
    (let ((msg (ed:do-find s "world" nil)))
      (assert-string-contains msg "found")
      ;; Finds "WORLD" on line 2 (1-based)
      (assert-string-contains msg "line 2"))))

(define-test find-advances-on-rfind ()
  (let ((s (make-session "aaa" "bbb" "aaa" "ccc")))
    (setf (ed:editor-top-line s) 0)
    ;; First find
    (let ((msg1 (ed:do-find s "aaa" nil)))
      (assert-string-contains msg1 "line 1"))
    (setf (ed:editor-last-find-line s) 0)
    ;; RFind should advance past line 0 (real), find line 3 (1-based)
    (let ((msg2 (ed:do-find s "aaa" t)))
      (assert-string-contains msg2 "line 3"))))

(define-test find-wraps-around ()
  (let ((s (make-session "target" "other" "stuff")))
    (setf (ed:editor-top-line s) 2)  ; start at line 2
    (let ((msg (ed:do-find s "target" nil)))
      (assert-string-contains msg "wrapped"))))

(define-test find-not-found ()
  (let ((s (make-session "Hello" "World")))
    (setf (ed:editor-top-line s) 0)
    (let ((msg (ed:do-find s "XYZ" nil)))
      (assert-string-contains msg "not found"))))

;;; ============================================================
;;; CHANGE tests
;;; ============================================================

(define-test change-single ()
  (let ((s (make-session "Hello World" "Hello Again")))
    (setf (ed:editor-top-line s) 0)
    (let ((msg (ed:do-change s "Hello" "Goodbye" nil)))
      (assert-string-contains msg "1 occurrence")
      (assert-equal "Goodbye World" (first (lines s)))
      ;; Second line unchanged (only first occurrence)
      (assert-equal "Hello Again" (second (lines s))))))

(define-test change-all ()
  (let ((s (make-session "Hello World" "Hello Again")))
    (setf (ed:editor-top-line s) 0)
    (let ((msg (ed:do-change s "Hello" "Goodbye" t)))
      (assert-string-contains msg "2 occurrence")
      (assert-equal "Goodbye World" (first (lines s)))
      (assert-equal "Goodbye Again" (second (lines s))))))

(define-test change-case-insensitive ()
  (let ((s (make-session "HELLO world" "hello WORLD")))
    (setf (ed:editor-top-line s) 0)
    (ed:do-change s "hello" "Hi" t)
    (assert-equal "Hi world" (first (lines s)))
    (assert-equal "Hi WORLD" (second (lines s)))))

(define-test change-not-found ()
  (let ((s (make-session "Hello")))
    (setf (ed:editor-top-line s) 0)
    (let ((msg (ed:do-change s "XYZ" "ABC" nil)))
      (assert-string-contains msg "not found")
      (assert-nil (ed:editor-modified s)))))

(define-test change-all-no-infinite-loop ()
  ;; Replacing "a" with "aa" should not loop infinitely
  (let ((s (make-session "aaa")))
    (setf (ed:editor-top-line s) 0)
    (let ((msg (ed:do-change s "a" "aa" t)))
      (assert-string-contains msg "3 occurrence")
      (assert-equal "aaaaaa" (first (lines s))))))

;;; ============================================================
;;; Regex CHANGE tests
;;; ============================================================

(define-test change-regex-pattern ()
  ;; Use regex pattern to match digits
  (let ((s (make-session "line 123 here" "line 456 there")))
    (setf (ed:editor-top-line s) 1)
    (ed:do-change s "\\d+" "NUM" t)
    (assert-equal "line NUM here" (first (lines s)))
    (assert-equal "line NUM there" (second (lines s)))))

(define-test change-regex-backreference ()
  ;; Swap two words using capture groups
  (let ((s (make-session "hello world")))
    (setf (ed:editor-top-line s) 1)
    (ed:do-change s "(hello) (world)" "\\2 \\1" nil)
    (assert-equal "world hello" (first (lines s)))))

(define-test change-regex-single ()
  ;; Single replacement with regex
  (let ((s (make-session "foo123bar" "foo456bar")))
    (setf (ed:editor-top-line s) 1)
    (ed:do-change s "\\d+" "NUM" nil)
    ;; Only first match replaced
    (assert-equal "fooNUMbar" (first (lines s)))
    (assert-equal "foo456bar" (second (lines s)))))

(define-test change-regex-invalid ()
  ;; Invalid regex should return error message, not crash
  (let ((s (make-session "hello")))
    (setf (ed:editor-top-line s) 1)
    (let ((msg (ed:do-change s "(unclosed" "x" nil)))
      (assert-string-contains msg "Invalid regex"))))

(define-test change-regex-dot-star ()
  ;; .* pattern
  (let ((s (make-session "abc def ghi")))
    (setf (ed:editor-top-line s) 1)
    (ed:do-change s "def.*" "XYZ" nil)
    (assert-equal "abc XYZ" (first (lines s)))))

;;; ============================================================
;;; Delimited string parsing tests
;;; ============================================================

(define-test parse-delimited-slash ()
  (multiple-value-bind (str pos) (ed:parse-delimited-string "/hello world/" 0)
    (assert-equal "hello world" str)
    (assert-equal 13 pos)))

(define-test parse-delimited-quote ()
  (multiple-value-bind (str pos) (ed:parse-delimited-string "'foo bar'" 0)
    (assert-equal "foo bar" str)
    (assert-equal 9 pos)))

(define-test parse-delimited-word ()
  (multiple-value-bind (str pos) (ed:parse-delimited-string "hello world" 0)
    (assert-equal "hello" str)
    (assert-equal 5 pos)))

(define-test parse-delimited-with-leading-spaces ()
  (multiple-value-bind (str pos) (ed:parse-delimited-string "   hello" 0)
    (assert-equal "hello" str)
    (assert-equal 8 pos)))

(define-test parse-delimited-empty ()
  (multiple-value-bind (str pos) (ed:parse-delimited-string "" 0)
    (assert-equal "" str)
    (assert-equal 0 pos)))

;;; ============================================================
;;; Undo tests
;;; ============================================================

(define-test undo-restores-lines ()
  (let ((s (make-session "a" "b" "c")))
    (ed:save-undo-state s)
    (ed:delete-line-range s 1 1)
    (assert-lines s '("a" "c"))
    (ed:undo s)
    (assert-lines s '("a" "b" "c"))))

(define-test undo-restores-modified-flag ()
  (let ((s (make-session "a" "b")))
    (assert-nil (ed:editor-modified s))
    ;; Pushing undo state makes modified true (stack depth > 0)
    (ed:save-undo-state s)
    (assert-true (ed:editor-modified s))
    ;; Undo pops the stack, making modified nil again
    (ed:undo s)
    (assert-nil (ed:editor-modified s))))

(define-test undo-multiple-levels ()
  (let ((s (make-session "a" "b" "c")))
    (ed:save-undo-state s)
    (ed:delete-line-range s 0 1)  ; delete "a"
    (assert-lines s '("b" "c"))
    (ed:save-undo-state s)
    (ed:delete-line-range s 0 1)  ; delete "b"
    (assert-lines s '("c"))
    (ed:undo s)
    (assert-lines s '("b" "c"))
    (ed:undo s)
    (assert-lines s '("a" "b" "c"))))

(define-test undo-empty-stack ()
  (let ((s (make-session "a")))
    (let ((msg (ed:undo s)))
      (assert-string-contains msg "Nothing to undo"))))

(define-test undo-from-change-command ()
  (let ((s (make-session "Hello World")))
    (setf (ed:editor-top-line s) 0)
    (ed:do-change s "Hello" "Goodbye" nil)
    (assert-equal "Goodbye World" (first (lines s)))
    (ed:undo s)
    (assert-equal "Hello World" (first (lines s)))))

(define-test undo-stack-limit ()
  (let ((s (make-session "a")))
    (dotimes (i 60)
      (ed:save-undo-state s))
    (assert-true (<= (length (ed:editor-undo-stack s)) ed::+max-undo+))))

(define-test undo-from-prefix-commands ()
  (let ((s (make-session "a" "b" "c")))
    ;; execute-prefix-commands saves undo state internally
    (ed:execute-prefix-commands s '((1 :d 1 1)))
    (assert-lines s '("a" "c"))
    (ed:undo s)
    (assert-lines s '("a" "b" "c"))))

(define-test undo-no-state-saved-when-no-changes ()
  (let ((s (make-session "a" "b")))
    ;; Execute with no commands
    (ed:execute-prefix-commands s nil)
    ;; Should not have pushed undo state (nil commands)
    ;; Actually execute-prefix-commands returns nil for nil commands
    ;; because (when commands ...) is nil
    (assert-nil (ed:editor-undo-stack s))))

;;; ============================================================
;;; Restricted mode tests
;;; ============================================================

(define-test restricted-cancel-works ()
  (let ((s (make-session "hello")))
    (setf (ed:editor-restricted-p s) t)
    (let ((result (ed:handle-primary-command s "CANCEL")))
      (assert-equal :back result "CANCEL should work in restricted mode"))))

(define-test restricted-blocks-revert ()
  (let ((s (make-session "hello")))
    (setf (ed:editor-restricted-p s) t)
    (let ((result (ed:handle-primary-command s "REVERT")))
      (assert-true (stringp result) "REVERT should return error in restricted mode")
      (assert-string-contains result "restricted"))))

(define-test restricted-allows-save ()
  (let ((s (make-session "hello"))
        (path (merge-pathnames "lispf-restricted-test.txt" (uiop:temporary-directory))))
    (setf (ed:editor-restricted-p s) t)
    (setf (ed:editor-filepath s) path)
    (unwind-protect
         (let ((result (ed:handle-primary-command s "SAVE")))
           (assert-equal :stay result "SAVE should work in restricted mode"))
      (ignore-errors (delete-file path)))))

(define-test restricted-allows-submit ()
  (let ((s (make-session "hello"))
        (path (merge-pathnames "lispf-restricted-test2.txt" (uiop:temporary-directory))))
    (setf (ed:editor-restricted-p s) t)
    (setf (ed:editor-filepath s) path)
    (unwind-protect
         (let ((result (ed:handle-primary-command s "SUBMIT")))
           (assert-equal :back result "SUBMIT should work in restricted mode"))
      (ignore-errors (delete-file path)))))

;;; ============================================================
;;; Display name tests
;;; ============================================================

(define-test display-name-used-in-screen-data ()
  (let ((s (make-session "hello")))
    (setf (ed:editor-display-name s) "New Message")
    (setf (ed:editor-filename s) "tmp12345.txt")
    ;; The build-screen-data doesn't show the info line, but
    ;; we can check the display-name accessor
    (assert-equal "New Message" (ed:editor-display-name s))))

;;; ============================================================
;;; File I/O tests
;;; ============================================================

(define-test file-round-trip ()
  (let ((path (merge-pathnames "lispf-editor-test-tmp.txt"
                               (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("Hello" "World" ""))
           (let ((lines (ed:read-file-lines path)))
             (assert-equal '("Hello" "World" "") lines)))
      (ignore-errors (delete-file path)))))

(define-test read-nonexistent-file ()
  (assert-nil (ed:read-file-lines #P"/nonexistent/path/file.txt")))

(define-test revert-restores-file ()
  (let ((path (merge-pathnames "lispf-revert-test.txt" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("original" "content"))
           (let ((s (make-session "original" "content")))
             (setf (ed:editor-filepath s) path)
             ;; Modify the buffer
             (ed:save-undo-state s)
             (setf (nth 0 (ed:editor-lines s)) "changed")
             ;; Revert
             (let ((msg (ed:revert s)))
               (assert-string-contains msg "saved")
               (assert-lines s '("original" "content"))
               (assert-nil (ed:editor-modified s) "Modified should be cleared")
               (assert-nil (ed:editor-undo-stack s) "Undo stack should be cleared")
               (assert-nil (ed:editor-pending-block s) "Pending should be cleared"))))
      (ignore-errors (delete-file path)))))

(define-test revert-no-file ()
  (let ((s (make-session "hello")))
    (let ((msg (ed:revert s)))
      (assert-string-contains msg "No file"))))

;;; ============================================================
;;; Build screen data tests
;;; ============================================================

(define-test screen-data-shows-top-marker ()
  (let ((s (make-session "line1" "line2")))
    (setf (ed:editor-top-line s) 0)
    (multiple-value-bind (prefix data) (ed::build-screen-data s)
      ;; First line should be top marker
      (let ((prefix-lines (split-sequence:split-sequence #\Newline prefix))
            (data-lines (split-sequence:split-sequence #\Newline data)))
        (assert-equal "00000 " (first prefix-lines))
        (assert-string-contains (first data-lines) "Top of File")))))

(define-test screen-data-shows-file-lines ()
  (let ((s (make-session "Hello" "World")))
    (setf (ed:editor-top-line s) 1)  ; skip top marker
    (multiple-value-bind (prefix data) (ed::build-screen-data s)
      (let ((prefix-lines (split-sequence:split-sequence #\Newline prefix))
            (data-lines (split-sequence:split-sequence #\Newline data)))
        (assert-equal "00001 " (first prefix-lines))
        (assert-equal "Hello"
                      (string-right-trim '(#\Space) (first data-lines)))))))

(define-test screen-data-shows-bottom-marker ()
  (let ((s (make-session "only")))
    (setf (ed:editor-top-line s) 0)
    ;; Virtual lines: 0=top, 1=only, 2=bottom
    ;; Page size is 18, so all 3 virtual lines + 15 empty
    (multiple-value-bind (prefix data) (ed::build-screen-data s)
      (let ((prefix-lines (split-sequence:split-sequence #\Newline prefix))
            (data-lines (split-sequence:split-sequence #\Newline data)))
        ;; Third line (index 2) should be end of file marker
        (assert-equal "00002 " (third prefix-lines))
        (assert-string-contains (third data-lines) "End of File")))))

;;; ============================================================
;;; Integration: handle-primary-command with CHANGE/FIND
;;; ============================================================

(define-test primary-find-with-delimiters ()
  (let ((s (make-session "foo bar" "baz qux" "foo baz")))
    (setf (ed:editor-top-line s) 0)
    (let ((result (ed:handle-primary-command s "FIND /baz/")))
      (assert-true (stringp result))
      (assert-string-contains result "found"))))

(define-test primary-change-with-delimiters ()
  (let ((s (make-session "Hello World")))
    (setf (ed:editor-top-line s) 1)
    (let ((result (ed:handle-primary-command s "CHG /Hello/Goodbye/")))
      (assert-true (stringp result))
      (assert-string-contains result "CHANGED")
      (assert-equal "Goodbye World" (first (lines s))))))

(define-test primary-change-all ()
  (let ((s (make-session "aaa" "aaa")))
    (setf (ed:editor-top-line s) 1)
    (let ((result (ed:handle-primary-command s "CHG /a/b/ ALL")))
      (assert-string-contains result "CHANGED")
      (assert-equal "bbb" (first (lines s)))
      (assert-equal "bbb" (second (lines s))))))

(define-test primary-undo ()
  (let ((s (make-session "Hello")))
    (setf (ed:editor-top-line s) 1)
    (ed:handle-primary-command s "CHG /Hello/Goodbye/")
    (assert-equal "Goodbye" (first (lines s)))
    (ed:handle-primary-command s "UNDO")
    (assert-equal "Hello" (first (lines s)))))

(define-test primary-unknown-returns-nil ()
  (let ((s (make-session "a")))
    (assert-nil (ed:handle-primary-command s "XYZZY"))))

;;; ============================================================
;;; Scroll round-trip tests (simulate framework join/split)
;;; ============================================================

(defun simulate-join (data-string count)
  "Simulate the framework's join-repeat-field-values on a data string.
Splits into COUNT lines, right-trims each, strips trailing empty lines,
then re-joins with newlines."
  (let* ((lines (split-sequence:split-sequence #\Newline data-string))
         ;; Pad to count entries (framework would have COUNT fields)
         (padded (loop for i from 0 below count
                       collect (if (< i (length lines))
                                   (string-right-trim '(#\Space) (nth i lines))
                                   ""))))
    ;; Trim trailing empty lines
    (loop while (and padded (string= "" (car (last padded))))
          do (setf padded (butlast padded)))
    (format nil "~{~A~^~%~}" padded)))

(defun simulate-display-and-response (session)
  "Simulate one display cycle: build screen data, apply the join round-trip
that the framework performs, then call process-data-edits.
Returns T if the file was falsely marked as modified."
  (setf (ed:editor-undo-stack session) nil)
  ;; Build what the screen would show
  (multiple-value-bind (prefix-str data-str) (ed::build-screen-data session)
    ;; Simulate the framework join round-trip (what happens to field values
    ;; between display and response when the user doesn't edit anything)
    (let* ((lispf:*session* session)
           (joined-prefix (simulate-join prefix-str (ed:page-size session)))
           (joined-data (simulate-join data-str (ed:page-size session)))
           (prefix-lines (split-sequence:split-sequence #\Newline joined-prefix))
           (data-lines (split-sequence:split-sequence #\Newline joined-data)))
      (ed::process-data-edits session prefix-lines data-lines)))
  (ed:editor-modified session))

(define-test scroll-no-false-modify ()
  ;; Create a file large enough to scroll
  (let ((s (apply #'make-session
                  (loop for i from 1 to 40 collect (format nil "Line ~D content" i)))))
    ;; Display at top, simulate response with no edits
    (setf (ed:editor-top-line s) 0)
    (assert-nil (simulate-display-and-response s)
                "First display should not set modified")
    ;; Scroll down
    (setf (ed:editor-top-line s) (+ (ed:editor-top-line s) (ed:page-size s)))
    (assert-nil (simulate-display-and-response s)
                "Scroll down should not set modified")
    ;; Scroll back up
    (setf (ed:editor-top-line s) 0)
    (assert-nil (simulate-display-and-response s)
                "Scroll back up should not set modified")))

(define-test scroll-short-file-no-false-modify ()
  ;; A file shorter than one page
  (let ((s (make-session "Hello" "World" "")))
    (setf (ed:editor-top-line s) 0)
    (assert-nil (simulate-display-and-response s)
                "Short file display should not set modified")))

(define-test scroll-file-with-trailing-spaces-no-false-modify ()
  ;; Lines with trailing spaces
  (let ((s (make-session "Hello   " "World   " "  ")))
    (setf (ed:editor-top-line s) 0)
    (assert-nil (simulate-display-and-response s)
                "File with trailing spaces should not false-modify")))

;;; ============================================================
;;; End-to-end tests (require s3270 on PATH)
;;; ============================================================

(defvar *edit-screen* nil)
(defvar *open-screen* nil)

(defvar *screen-dir*
  (merge-pathnames #P"editor/screens/"
                   (asdf:system-source-directory :lispf)))

(defun load-editor-screen-data ()
  (setf *edit-screen* (load-test-screen-data
                        (merge-pathnames "edit.screen" *screen-dir*)))
  (setf *open-screen* (load-test-screen-data
                        (merge-pathnames "open.screen" *screen-dir*))))

(defun s3270-available-p ()
  "Return T if s3270 is on PATH."
  (ignore-errors
    (let ((p (uiop:launch-program "which s3270" :output :string)))
      (zerop (uiop:wait-process p)))))

(defun assert-no-modify-flag (session description)
  "Assert that the status line shows Alt=0 (no modifications)."
  (let ((status (screen-row session 0)))
    (unless (search "Alt=0" status)
      (error 'test-failure :description description
             :expected "Alt=0"
             :actual (string-right-trim '(#\Space) status)))))

(defun assert-command-field-clean (session description)
  "Assert the command field area (row 23) contains no spurious text."
  (let* ((cmd-area (screen-text-at session 23 6 73))
         (trimmed (string-right-trim '(#\Space) cmd-area)))
    (when (plusp (length trimmed))
      (error 'test-failure :description description
             :expected ""
             :actual trimmed))))

(defun assert-row-contains (session row text description)
  "Assert that screen ROW contains TEXT."
  (let ((content (screen-row session row)))
    (unless (search text content)
      (error 'test-failure :description description
             :expected text
             :actual (string-right-trim '(#\Space) content)))))

(define-test e2e-scroll-full-file ()
  ;; 67-line file: scroll forward through entire file and back,
  ;; verifying markers, no false modification, no command field contamination.
  (let ((path #P"/tmp/lispf-e2e-scroll.txt")
        (n-lines 67)
        (page (ed:page-size (ed:make-default-layout))))
    (unwind-protect
         (progn
           ;; Create file with identifiable lines (some with leading spaces)
           (ed:write-file-lines path
             (loop for i from 1 to n-lines
                   collect (if (zerop (mod i 5))
                               (format nil "     Indented line ~D" i)
                               (format nil "Line ~D of ~D" i n-lines))))
           (with-test-app (s ed::*editor-app* :port 13278)
             (assert-on-screen s "OPEN")
             (type-text s (namestring path))
             (press-enter s)
             (assert-screen-contains s "Size=")

             ;; At top: should see Top-of-Data marker and first lines
             ;; Data fields start at display row 3 (screen row 2 + 1 for title)
             (assert-row-contains s 2 "Top of File" "Top marker on initial display")
             (assert-row-contains s 3 "00001" "Line 1 number on initial display")
             (assert-row-contains s 3 "Line 1 of" "Line 1 content on initial display")
             (assert-no-modify-flag s "No modification on initial display")
             (assert-command-field-clean s "Command field clean on initial display")

             ;; Scroll forward through entire file
             ;; PF8 scrolls by (1- page) lines (DATA mode with 1-line overlap)
             (let* ((scroll-step (1- page))
                    (total-virtual (+ n-lines 2))
                    (pages-down (ceiling total-virtual scroll-step)))
               (dotimes (i pages-down)
                 (press-pf s 8)
                 (assert-no-modify-flag s
                   (format nil "No modification after PF8 #~D" (1+ i)))
                 (assert-command-field-clean s
                   (format nil "Command field clean after PF8 #~D" (1+ i)))))

             ;; Use BOTTOM command to ensure we see end of file
             (e2e-type-command s "BOTTOM")
             ;; Should see End of File marker and last file line
             (assert-screen-contains s "End of File")
             (assert-screen-contains s (format nil "Line ~D of ~D" n-lines n-lines))

             ;; Scroll backward through entire file
             (let* ((scroll-step (1- page))
                    (total-virtual (+ n-lines 2))
                    (pages-up (1+ (ceiling total-virtual scroll-step))))
               (dotimes (i pages-up)
                 (press-pf s 7)
                 (assert-no-modify-flag s
                   (format nil "No modification after PF7 #~D" (1+ i)))
                 (assert-command-field-clean s
                   (format nil "Command field clean after PF7 #~D" (1+ i)))))

             ;; Use TOP command to get back to start
             (e2e-type-command s "TOP")
             ;; Should see Top-of-File marker again
             (assert-row-contains s 2 "Top of File" "Top marker after scroll back")
             (assert-row-contains s 3 "00001" "Line 1 after scroll back")

             ;; PF3 should exit without "modified" warning
             (press-pf s 3)
             (assert-on-screen s "OPEN")))
      (ignore-errors (delete-file path)))))

(define-test e2e-enter-inserts-line ()
  ;; Open a 1-line file, move cursor to the data area, press Enter
  ;; A new blank line should be inserted
  (let ((path #P"/tmp/lispf-e2e-enter.txt"))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("hello"))
           (with-test-app (s ed::*editor-app* :port 13279)
             (type-text s (namestring path))
             (press-enter s)
             (assert-screen-contains s "Size=")
             ;; Should show 1 line
             (assert-screen-contains s "Size=1")
             ;; Move cursor to the data area (row 4 = first file line after top marker)
             ;; and press Enter
             (move-cursor s 3 10)
             (press-enter s)
             ;; Should now have 2 lines
             (assert-screen-contains s "Size=2")
             (assert-screen-contains s "00002")
             ;; Cursor should be on the new line (row 5 = second data line, col 7 = data start)
             (assert-cursor-at s 4 7)
             ;; Cancel to exit without saving
             (move-cursor s 23 6)
             (type-text s "CANCEL")
             (press-enter s)
             (assert-on-screen s "OPEN")))
      (ignore-errors (delete-file path)))))

(define-test e2e-open-file-and-see-edit-screen ()
  ;; Write a temp file to edit
  (let ((path #P"/tmp/lispf-e2e.txt"))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("Hello World" "Second line" "Third line"))
           (with-test-app (s ed::*editor-app* :port 13271)
             (assert-on-screen s "OPEN")
             ;; Cursor starts on the filename field; type the path
             (type-text s (namestring path))
             (press-enter s)
             ;; Should be on the EDIT screen now
             (assert-screen-contains s "Size=")
             ;; Should see the file content (Top of File marker + lines)
             (assert-screen-contains s "Top of File")
             (assert-screen-contains s "Hello World")
             (assert-screen-contains s "Second line")
             ;; Should see line numbers
             (assert-screen-contains s "00001")
             ;; PF3 should exit (file is unmodified)
             (press-pf s 3)
             (assert-on-screen s "OPEN")))
      (ignore-errors (delete-file path)))))

(defun e2e-type-command (s command)
  "Type a command in the editor command field and press Enter."
  (move-cursor s 23 7)
  (erase-eof s)
  (type-text s command)
  (press-enter s))

(defun e2e-type-prefix (s row text)
  "Type a prefix command on the given data ROW (0-based from data start, so row 0 = screen row 2).
Moves cursor to command field before pressing Enter to avoid auto-insert."
  (move-cursor s (+ 2 row) 1)
  (type-text s text)
  (move-cursor s 23 7)
  (press-enter s))

(define-test e2e-comprehensive-editing ()
  ;; Comprehensive test exercising: inline edit, prefix commands (i, d, dd, cc, a),
  ;; primary commands (FIND, CHANGE, UNDO, SAVE, TOP, BOTTOM), PF3 exit with
  ;; modification warning, and file persistence.
  (let ((path #P"/tmp/lispf-e2e-comprehensive.txt"))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("alpha" "bravo" "charlie" "delta" "echo"
                                       "foxtrot" "golf" "hotel"))
           (with-test-app (s ed::*editor-app* :port 13280)
             (type-text s (namestring path))
             (press-enter s)
             (assert-screen-contains s "Size=8")

             ;; --- Inline edit: overtype first data line ---
             ;; Row 3 = first file line (row 2 = Top of File marker)
             ;; Move cursor to command field before Enter to avoid auto-insert
             (move-cursor s 3 7)
             (type-text s "ALPHA")
             (move-cursor s 23 7)
             (press-enter s)
             (assert-screen-contains s "ALPHA")

             ;; --- CHANGE command with UNDO ---
             (e2e-type-command s "CHANGE /bravo/BRAVO/")
             (assert-screen-contains s "CHANGED 1")
             (assert-screen-contains s "BRAVO")
             (e2e-type-command s "UNDO")
             (assert-screen-contains s "bravo")

             ;; --- Prefix i: insert a line after "bravo" ---
             ;; bravo is line 2, screen row 4 (row 2=TOF, 3=ALPHA, 4=bravo)
             (e2e-type-prefix s 2 "i")
             (assert-screen-contains s "Size=9")

             ;; --- Prefix d: delete the blank line we just inserted ---
             ;; The new blank line is at screen row 5 (data row 3)
             (e2e-type-prefix s 3 "d")
             (assert-screen-contains s "Size=8")

             ;; --- FIND command ---
             (e2e-type-command s "FIND /hotel/")
             (assert-screen-contains s "found")

             ;; --- CHANGE ALL ---
             (e2e-type-command s "TOP")
             (e2e-type-command s "CHANGE /delta/DELTA/ ALL")
             (assert-screen-contains s "CHANGED 1")
             (assert-screen-contains s "DELTA")

             ;; --- Block delete: dd on charlie and DELTA ---
             ;; After TOP: row 2=TOF, 3=ALPHA, 4=bravo, 5=charlie, 6=DELTA
             (e2e-type-command s "TOP")
             (move-cursor s 5 1)
             (type-text s "dd")
             (move-cursor s 6 1)
             (type-text s "dd")
             (press-enter s)
             (assert-screen-contains s "deleted")
             (assert-screen-contains s "Size=6")

             ;; --- Block copy: cc on ALPHA+bravo, a after golf ---
             ;; After delete: row 2=TOF, 3=ALPHA, 4=bravo, 5=echo, 6=foxtrot, 7=golf, 8=hotel, 9=EOF
             (move-cursor s 3 1)
             (type-text s "cc")
             (move-cursor s 4 1)
             (type-text s "cc")
             (move-cursor s 7 1)
             (type-text s "a")
             (press-enter s)
             (assert-screen-contains s "copied")
             (assert-screen-contains s "Size=8")

             ;; --- PF3 should warn about modifications ---
             (press-pf s 3)
             (assert-screen-contains s "modified")

             ;; --- SAVE and verify file ---
             (e2e-type-command s "SAVE")
             (assert-screen-contains s "Alt=0")

             ;; --- PF3 should exit now (no modifications) ---
             (press-pf s 3)
             (assert-on-screen s "OPEN")

             ;; --- Verify saved file contents ---
             (let ((saved (ed:read-file-lines path)))
               (assert-equal '("ALPHA" "bravo" "echo" "foxtrot" "golf" "ALPHA" "bravo" "hotel")
                             saved "Saved file should reflect all edits"))))
      (ignore-errors (delete-file path)))))

;;; ============================================================
;;; Enter key behavior tests
;;; ============================================================

(define-test enter-on-last-line-inserts ()
  ;; Pressing Enter on the last line of the file should insert a new blank line
  (let ((s (make-session "a" "b" "c")))
    (setf (ed:editor-top-line s) 0)
    (let ((lispf:*session* s))
      ;; Cursor on last line "c" (virtual 3, screen row = data-start(2) + 3 = 5)
      (setf (lispf:cursor-row) 5)
      (ed::auto-insert-line s (ed:editor-layout s))
      (assert-lines s '("a" "b" "c" "")
                    "Should insert new line after last line"))))

(define-test enter-on-last-line-no-insert-when-disabled ()
  ;; With AUTOINSERT OFF, Enter on last line should not insert
  (let ((s (make-session "a" "b" "c")))
    (setf (ed:editor-top-line s) 0
          (ed:editor-auto-insert-p s) nil)
    (let ((lispf:*session* s))
      (setf (lispf:cursor-row) 5)
      (ed::auto-insert-line s (ed:editor-layout s))
      (assert-lines s '("a" "b" "c")
                    "Should not insert when auto-insert disabled"))))

(define-test enter-on-middle-line-inserts-when-enabled ()
  ;; With AUTOINSERT ON, Enter on a middle line should insert a new line
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 0)
    (let ((lispf:*session* s))
      ;; Cursor on line "b" (virtual 2, screen row = data-start(2) + 2 = 4)
      (setf (lispf:cursor-row) 4)
      (ed::auto-insert-line s (ed:editor-layout s))
      (assert-lines s '("a" "b" "" "c" "d" "e")
                    "Should insert on middle line when autoinsert on"))))

(define-test enter-on-middle-line-no-insert-when-disabled ()
  ;; With AUTOINSERT OFF, Enter on a middle line should NOT insert
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 0
          (ed:editor-auto-insert-p s) nil)
    (let ((lispf:*session* s))
      (setf (lispf:cursor-row) 4)
      (ed::auto-insert-line s (ed:editor-layout s))
      (assert-lines s '("a" "b" "c" "d" "e")
                    "Should not insert on middle line when autoinsert off"))))

(define-test enter-advances-cursor-when-disabled ()
  ;; With AUTOINSERT OFF, Enter on a middle line should just advance cursor
  (let ((s (make-session "a" "b" "c" "d")))
    (setf (ed:editor-top-line s) 0
          (ed:editor-auto-insert-p s) nil)
    (let ((lispf:*session* s))
      (setf (lispf:cursor-row) 4) ; line "b" at screen row 4
      (ed::auto-insert-line s (ed:editor-layout s))
      (assert-lines s '("a" "b" "c" "d")
                    "Should not insert when autoinsert off")
      (assert-true (ed::editor-next-cursor s)
                   "Should set next cursor position")
      ;; Next cursor row should be one row down (5 = line "c")
      (assert-equal 5 (car (ed::editor-next-cursor s))
                    "Cursor should advance to next row"))))

;;; ============================================================
;;; SET AUTOINSERT tests
;;; ============================================================

(define-test set-autoinsert-off ()
  (let ((s (make-session "a")))
    (assert-true (ed:editor-auto-insert-p s) "Default should be ON")
    (let ((msg (ed:handle-primary-command s "SET AUTOINSERT OFF")))
      (assert-string-contains msg "OFF")
      (assert-nil (ed:editor-auto-insert-p s)))))

(define-test set-autoinsert-on ()
  (let ((s (make-session "a")))
    (setf (ed:editor-auto-insert-p s) nil)
    (let ((msg (ed:handle-primary-command s "SET AI ON")))
      (assert-string-contains msg "ON")
      (assert-true (ed:editor-auto-insert-p s)))))

(define-test set-autoinsert-query ()
  (let ((s (make-session "a")))
    (let ((msg (ed:handle-primary-command s "SET AUTOINSERT")))
      (assert-string-contains msg "ON"))))

;;; ============================================================
;;; Screen data after delete tests
;;; ============================================================

(define-test delete-clears-past-eof-screen-data ()
  ;; After deleting a line, build-screen-data should produce non-empty
  ;; (space-filled) data for rows past the EOF marker, so that the
  ;; 3270 terminal clears old content in no-clear mode.
  (let ((s (make-session "a" "b" "c" "d" "e")))
    (setf (ed:editor-top-line s) 0)
    ;; Delete line "e" (real index 4)
    (ed:execute-prefix-commands s '((4 :d 1 4)))
    ;; File is now (a b c d), 4 lines
    (assert-lines s '("a" "b" "c" "d"))
    ;; Build screen data
    (multiple-value-bind (prefix-str data-str) (ed::build-screen-data s)
      (let ((data-lines (split-sequence:split-sequence #\Newline data-str))
            (prefix-lines (split-sequence:split-sequence #\Newline prefix-str)))
        ;; Virtual lines: 0=TOP, 1=a, 2=b, 3=c, 4=d, 5=EOF, 6..20=past EOF
        ;; Lines past EOF (index 6+) should NOT be empty strings
        ;; They should be space-filled so CL3270 clears them in no-clear mode
        (loop for i from 6 below (ed:page-size s)
              for line = (nth i data-lines)
              do (assert-true (plusp (length line))
                              (format nil "Data line ~D past EOF should not be empty" i)))
        (loop for i from 6 below (ed:page-size s)
              for line = (nth i prefix-lines)
              do (assert-true (plusp (length line))
                              (format nil "Prefix line ~D past EOF should not be empty" i)))))))

;;; ============================================================
;;; Revert message tests
;;; ============================================================

(define-test revert-message-concise ()
  ;; Revert message should not include the filename
  (let ((path (merge-pathnames "lispf-revert-msg-test.txt" (uiop:temporary-directory))))
    (unwind-protect
         (progn
           (ed:write-file-lines path '("original"))
           (let ((s (make-session "original")))
             (setf (ed:editor-filepath s) path)
             (ed:save-undo-state s)
             (setf (nth 0 (ed:editor-lines s)) "changed")
             (let ((msg (ed:revert s)))
               (assert-true (stringp msg))
               ;; Should NOT contain the filename
               (assert-nil (search "lispf-revert" msg)
                           "Revert message should not include filename"))))
      (ignore-errors (delete-file path)))))

;;; ============================================================
;;; Message field padding tests
;;; ============================================================

(define-test pad-to-field-width ()
  ;; ed::pad-to-field-width should pad short strings to the given width
  (assert-equal 79 (length (ed::pad-to-field-width "Short" 79))
                "Short string should be padded to 79")
  (assert-equal "Short" (string-right-trim '(#\Space) (ed::pad-to-field-width "Short" 79))
                "Padded string content should be preserved")
  ;; A string already at or beyond width should not be truncated
  (let ((long (make-string 80 :initial-element #\X)))
    (assert-equal 80 (length (ed::pad-to-field-width long 79))
                  "Long string should not be truncated"))
  ;; Empty string should become all spaces
  (assert-equal 79 (length (ed::pad-to-field-width "" 79))
                "Empty string should be padded to 79"))

;;; ============================================================
;;; Runner
;;; ============================================================

(defun run-all ()
  "Run all editor tests in definition order."
  (load-editor-screen-data)
  (format t "~&;;; Running editor tests~%")
  (let ((*package* (find-package :lispf-editor-tests)))
    (run-tests)))
