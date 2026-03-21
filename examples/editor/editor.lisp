;;; -*- Mode: Lisp -*-

;;; LISPF Editor - A mainframe-style file editor inspired by ISPF/XEDIT
;;;
;;; Usage:
;;;   (asdf:load-system "lispf")
;;;   (load "examples/editor/editor.lisp")
;;;   (lispf-editor:start)
;;;
;;; Then connect with a 3270 terminal emulator (e.g. wx3270) on port 3270.
;;;
;;; Primary commands (command line):
;;;   SAVE          - Save file to disk
;;;   FILE / SUBMIT - Save and exit
;;;   CANCEL / CAN  - Quit without saving
;;;   TOP           - Go to top of file
;;;   BOTTOM / BOT  - Go to bottom of file
;;;   UP [n]        - Scroll up n lines (default: page)
;;;   DOWN [n]      - Scroll down n lines (default: page)
;;;   LEFT [n]      - Scroll left n columns (default: data width)
;;;   RIGHT [n]     - Scroll right n columns (default: data width)
;;;   FIND string   - Find text (case-insensitive)
;;;   CHANGE /s1/s2/ [ALL] - Replace text
;;;   LOCATE n / L n - Go to line n
;;;   RESET / RES   - Clear pending prefix commands
;;;   UNDO          - Undo last change
;;;
;;; Prefix commands (line number area):
;;;   I[n]    - Insert n blank lines after
;;;   D[n]    - Delete n lines
;;;   DD...DD - Delete block
;;;   R[n]    - Repeat (duplicate) line n times
;;;   RR...RR - Repeat block
;;;   C[n]    - Copy n lines (needs A or B target)
;;;   CC...CC - Copy block (needs A or B target)
;;;   M[n]    - Move n lines (needs A or B target)
;;;   MM...MM - Move block (needs A or B target)
;;;   A       - After target for copy/move
;;;   B       - Before target for copy/move
;;;   UC[n]   - Uppercase n lines
;;;   LC[n]   - Lowercase n lines
;;;   ([n]    - Shift left n columns
;;;   )[n]    - Shift right n columns
;;;
;;; Function keys:
;;;   PF3     - Exit (prompts if modified)
;;;   PF5     - Repeat last FIND
;;;   PF6     - Repeat last CHANGE
;;;   PF7     - Scroll up one page
;;;   PF8     - Scroll down one page
;;;   PF10    - Scroll left
;;;   PF11    - Scroll right

(defpackage #:lispf-editor
  (:use #:cl)
  (:local-nicknames (#:bt #:bordeaux-threads)
                    (#:lspf #:lispf)
                    (#:cl3270 #:cl3270))
  (:export #:start
           ;; Exported for testing
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
           #:parse-prefix-command
           #:parse-delimited-string
           #:execute-prefix-commands
           #:do-find
           #:do-change
           #:save-undo-state
           #:undo
           #:handle-primary-command
           #:read-file-lines
           #:write-file-lines
           #:make-test-session
           #:+page-size+
           #:+data-width+
           #:+max-undo+))

(in-package #:lispf-editor)

;;; ============================================================
;;; Constants
;;; ============================================================

(defconstant +page-size+ 17
  "Number of data lines visible on the edit screen.")

(defconstant +data-width+ 72
  "Width of the data area on each line.")

(defconstant +prefix-width+ 6
  "Width of the prefix area on each line.")

(defconstant +max-undo+ 50
  "Maximum number of undo states to keep.")

;;; ============================================================
;;; Editor session
;;; ============================================================

(defclass editor-session (lspf:session)
  ((lines :initform (list "") :accessor editor-lines
          :documentation "List of strings, one per line.")
   (filename :initform nil :accessor editor-filename)
   (filepath :initform nil :accessor editor-filepath)
   (modified :initform nil :accessor editor-modified)
   (top-line :initform 0 :accessor editor-top-line
             :documentation "0-based virtual index of the first visible row.
Virtual 0 = Top-of-Data marker, 1..N = file lines, N+1 = Bottom-of-Data marker.")
   (col-offset :initform 0 :accessor editor-col-offset
               :documentation "0-based column offset for horizontal scrolling.")
   (last-find :initform nil :accessor editor-last-find
              :documentation "Last FIND search string for RFind.")
   (last-find-line :initform 0 :accessor editor-last-find-line
                   :documentation "Line where last FIND matched, for advancing on RFind.")
   (last-change :initform nil :accessor editor-last-change
                :documentation "Last CHANGE arguments (from to all-p) for RChange.")
   (pending-block :initform nil :accessor editor-pending-block
                  :documentation "Pending block command: (cmd start-line [count]) or nil.")
   (undo-stack :initform nil :accessor editor-undo-stack
               :documentation "List of (lines-copy . modified-flag) for undo.")))

(defun make-test-session (lines)
  "Create an editor session for testing (no application binding needed)."
  (let ((s (make-instance 'editor-session)))
    (setf (editor-lines s) (copy-list lines))
    s))

;;; ============================================================
;;; File I/O
;;; ============================================================

(defun read-file-lines (path)
  "Read a file into a list of strings (one per line)."
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (when s
      (loop for line = (read-line s nil nil)
            while line collect line))))

(defun write-file-lines (path lines)
  "Write a list of strings to a file (one per line)."
  (with-open-file (s path :direction :output
                          :if-exists :supersede
                          :if-does-not-exist :create)
    (dolist (line lines)
      (write-line line s))))

;;; ============================================================
;;; Undo
;;; ============================================================

(defun save-undo-state (session)
  "Save current lines and modified flag for undo."
  (push (cons (copy-list (editor-lines session))
              (editor-modified session))
        (editor-undo-stack session))
  (when (> (length (editor-undo-stack session)) +max-undo+)
    (setf (editor-undo-stack session)
          (subseq (editor-undo-stack session) 0 +max-undo+))))

(defun undo (session)
  "Restore the previous state. Returns a message string."
  (let ((state (pop (editor-undo-stack session))))
    (if state
        (progn
          (setf (editor-lines session) (car state))
          (setf (editor-modified session) (cdr state))
          "UNDO completed")
        "Nothing to undo")))

;;; ============================================================
;;; Line buffer utilities
;;; ============================================================

(defun line-count (session)
  "Return the number of lines in the editor buffer."
  (length (editor-lines session)))

(defun total-virtual-lines (session)
  "Total lines including Top-of-Data and Bottom-of-Data markers."
  (+ (line-count session) 2))

(defun virtual-to-real (session virtual-index)
  "Convert a virtual line index to a real (0-based) line index.
Returns nil for marker lines or out-of-range indices.
Virtual 0 = Top-of-Data, 1..N = file lines, N+1 = Bottom-of-Data."
  (let ((real (1- virtual-index)))
    (when (and (>= real 0) (< real (line-count session)))
      real)))

(defun marker-line-p (session virtual-index)
  "Return T if VIRTUAL-INDEX is a Top-of-Data or Bottom-of-Data marker."
  (or (= virtual-index 0)
      (= virtual-index (1+ (line-count session)))))

(defun line-at (session real-index)
  "Get the line at REAL-INDEX, or nil if out of range."
  (when (and (>= real-index 0) (< real-index (line-count session)))
    (nth real-index (editor-lines session))))

(defun (setf line-at) (value session real-index)
  "Set the line at REAL-INDEX."
  (when (and (>= real-index 0) (< real-index (line-count session)))
    (setf (nth real-index (editor-lines session)) value)))

(defun insert-lines-after (session real-index new-lines)
  "Insert NEW-LINES after REAL-INDEX. If REAL-INDEX is -1, insert at beginning."
  (let ((lines (editor-lines session)))
    (if (= real-index -1)
        (setf (editor-lines session) (append new-lines lines))
        (let ((tail (nthcdr real-index lines)))
          (setf (cdr tail) (append new-lines (cdr tail))))))
  (setf (editor-modified session) t))

(defun insert-lines-before (session real-index new-lines)
  "Insert NEW-LINES before REAL-INDEX."
  (insert-lines-after session (1- real-index) new-lines))

(defun delete-line-range (session start-index count)
  "Delete COUNT lines starting at START-INDEX."
  (let ((lines (editor-lines session)))
    (setf (editor-lines session)
          (append (subseq lines 0 (min start-index (length lines)))
                  (subseq lines (min (+ start-index count) (length lines))))))
  (setf (editor-modified session) t))

(defun extract-line-range (session start-index count)
  "Extract COUNT lines starting at START-INDEX without removing them."
  (let ((lines (editor-lines session)))
    (subseq lines
            (min start-index (length lines))
            (min (+ start-index count) (length lines)))))

(defun visible-portion (line col-offset)
  "Return the portion of LINE visible with COL-OFFSET, padded/truncated to +data-width+."
  (let* ((len (length line))
         (start (min col-offset len))
         (end (min (+ col-offset +data-width+) len))
         (visible (subseq line start end)))
    (if (< (length visible) +data-width+)
        (concatenate 'string visible
                     (make-string (- +data-width+ (length visible))
                                  :initial-element #\Space))
        visible)))

(defun apply-edit (session real-index new-visible-text)
  "Apply an edit to the visible portion of a line.
Correctly handles horizontal scrolling by preserving content before and after
the visible window."
  (when (and (>= real-index 0) (< real-index (line-count session)))
    (let* ((line (nth real-index (editor-lines session)))
           (col-offset (editor-col-offset session))
           (before (if (< (length line) col-offset)
                       (concatenate 'string line
                                    (make-string (- col-offset (length line))
                                                 :initial-element #\Space))
                       (subseq line 0 col-offset)))
           (after (if (> (length line) (+ col-offset +data-width+))
                      (subseq line (+ col-offset +data-width+))
                      ""))
           ;; When there is content after the visible window, pad the visible
           ;; portion to full width to preserve the splice boundaries.
           (middle (if (plusp (length after))
                       (let ((vis new-visible-text))
                         (if (< (length vis) +data-width+)
                             (concatenate 'string vis
                                          (make-string (- +data-width+ (length vis))
                                                       :initial-element #\Space))
                             vis))
                       new-visible-text))
           (new-line (string-right-trim
                      '(#\Space)
                      (concatenate 'string before middle after))))
      (unless (string= new-line line)
        (setf (nth real-index (editor-lines session)) new-line)
        (setf (editor-modified session) t)))))

(defun clamp-top-line (session)
  "Clamp top-line to valid range.
Allows scrolling past the end so Bottom-of-Data can reach the top of the screen."
  (setf (editor-top-line session)
        (max 0 (min (editor-top-line session)
                     (max 0 (1- (total-virtual-lines session)))))))

;;; ============================================================
;;; Prefix command parsing
;;; ============================================================

(defun parse-count (text start)
  "Parse a numeric count from TEXT starting at START. Returns 1 if no digits."
  (let ((digits (subseq text start)))
    (if (and (plusp (length digits)) (every #'digit-char-p digits))
        (parse-integer digits)
        1)))

(defun strip-line-number-suffix (text cmd-len)
  "Extract the command portion from TEXT, ignoring trailing remnants of line numbers.
In a 3270 overtype field, the user types over the start of '000123', leaving
digits from the old value after the command. CMD-LEN is the length of the
command prefix. Returns the meaningful part after the command (spaces + digits
that are part of the command, not remnants)."
  (let ((rest (subseq text cmd-len)))
    ;; If rest starts with a space, the user explicitly separated command from count
    ;; If rest is all digits, it could be a count OR leftover line number digits
    ;; We treat digits immediately after the command letter as a count only if
    ;; they are followed by non-digits or the string is short enough to be intentional
    rest))

(defun parse-prefix-count (text cmd-len)
  "Parse a count from TEXT after CMD-LEN characters of command.
Handles the 3270 overtype case where old line number digits remain.
A count is a non-zero digit (1-9) immediately after the command letter.
Leading zeros indicate line number remnants, not a count."
  (let ((rest (subseq text cmd-len)))
    (cond
      ;; Nothing after command
      ((zerop (length rest)) 1)
      ;; Spaces after command (user cleared the rest)
      ((char= (char rest 0) #\Space) 1)
      ;; Non-zero digit = explicit count
      ((and (digit-char-p (char rest 0))
            (char/= (char rest 0) #\0))
       (digit-char-p (char rest 0)))
      ;; Zero or non-digit = no explicit count (line number remnant)
      (t 1))))

(defun parse-prefix-command (text)
  "Parse a prefix command from a 3270 prefix field value.
Handles overtype mode where the user types over an existing line number
like '000001', leaving remnants (e.g. 'DD0001' means block delete).
Returns (values command count) or nil."
  (let ((trimmed (string-trim '(#\Space) (string-upcase text))))
    (when (plusp (length trimmed))
      (cond
        ;; Block commands: match at start (DD, CC, MM, RR followed by anything)
        ((and (>= (length trimmed) 2) (string= trimmed "DD" :end1 2))
         (values :dd 0))
        ((and (>= (length trimmed) 2) (string= trimmed "CC" :end1 2))
         (values :cc 0))
        ((and (>= (length trimmed) 2) (string= trimmed "MM" :end1 2))
         (values :mm 0))
        ((and (>= (length trimmed) 2) (string= trimmed "RR" :end1 2))
         (values :rr 0))
        ;; UC/LC with optional count
        ((and (>= (length trimmed) 2) (string= trimmed "UC" :end1 2))
         (values :uc (parse-prefix-count trimmed 2)))
        ((and (>= (length trimmed) 2) (string= trimmed "LC" :end1 2))
         (values :lc (parse-prefix-count trimmed 2)))
        ;; Text split
        ((and (>= (length trimmed) 2) (string= trimmed "TS" :end1 2))
         (values :ts 0))
        ;; Targets: A or B at start, rest is line number remnants
        ((char= (char trimmed 0) #\A)
         (values :a 0))
        ((char= (char trimmed 0) #\B)
         (values :b 0))
        ;; Single-line commands with optional count
        ((char= (char trimmed 0) #\I)
         (values :i (parse-prefix-count trimmed 1)))
        ((char= (char trimmed 0) #\D)
         (values :d (parse-prefix-count trimmed 1)))
        ((char= (char trimmed 0) #\R)
         (values :r (parse-prefix-count trimmed 1)))
        ((char= (char trimmed 0) #\C)
         (values :c (parse-prefix-count trimmed 1)))
        ((char= (char trimmed 0) #\M)
         (values :m (parse-prefix-count trimmed 1)))
        ;; Shift commands
        ((char= (char trimmed 0) #\()
         (values :shift-left (parse-prefix-count trimmed 1)))
        ((char= (char trimmed 0) #\))
         (values :shift-right (parse-prefix-count trimmed 1)))
        (t nil)))))

;;; ============================================================
;;; Prefix command execution
;;; ============================================================

(defun find-block-markers (commands cmd-type)
  "Find all entries in COMMANDS with command CMD-TYPE. Returns a list."
  (remove-if-not (lambda (entry) (eq (second entry) cmd-type)) commands))

(defun find-target (commands)
  "Find the first A or B target in COMMANDS. Returns the entry or nil."
  (find-if (lambda (entry) (member (second entry) '(:a :b))) commands))

(defun execute-block-with-target (session source-cmd source-start source-count
                                  target-cmd target-real)
  "Execute a copy or move block command with its target. Returns error message or nil."
  (let ((insert-at (if (eq target-cmd :a)
                       (if (>= target-real 0) (1+ target-real) 0)
                       (max 0 target-real))))
    ;; Validate: target cannot be inside the source block
    (when (and (>= insert-at source-start)
               (< insert-at (+ source-start source-count)))
      (return-from execute-block-with-target
        "Target cannot be inside the source block"))
    (let ((copies (extract-line-range session source-start source-count)))
      (cond
        ((member source-cmd '(:cc :c))
         (insert-lines-after session (1- insert-at) (copy-list copies)))
        ((member source-cmd '(:mm :m))
         (let ((adj-target (if (>= insert-at (+ source-start source-count))
                               (- insert-at source-count)
                               insert-at)))
           (delete-line-range session source-start source-count)
           (insert-lines-after session (1- adj-target) (copy-list copies))))))
    nil))

(defun execute-prefix-commands (session commands)
  "Execute collected prefix commands. Returns an error/info message or nil.
COMMANDS is a list of (real-line-index command count screen-row).
When both block markers (and optionally A/B targets) appear in the same
batch, they are executed immediately without pending."
  (let ((pending (editor-pending-block session))
        (did-modify nil)
        (result-message nil)
        (navigate-to nil))
    ;; Save undo state before any modifications
    (save-undo-state session)

    ;; First pass: handle block commands
    ;; Check for paired block markers within this batch
    (dolist (block-cmd '(:dd :cc :mm :rr))
      (let ((markers (find-block-markers commands block-cmd)))
        (when (>= (length markers) 2)
          ;; Two markers in same batch - check for conflicts with pending
          (when (and pending (not (eq block-cmd (first pending))))
            (setf (editor-pending-block session) nil)
            (unless did-modify (pop (editor-undo-stack session)))
            (return-from execute-prefix-commands
              "Conflicting pending command - use RESET"))
          (let* ((first-marker (first markers))
                 (second-marker (second markers))
                 (idx1 (first first-marker))
                 (idx2 (first second-marker)))
            (when (or (< idx1 0) (< idx2 0))
              (unless did-modify (pop (editor-undo-stack session)))
              (return-from execute-prefix-commands
                "Cannot use marker line as block endpoint"))
            (let* ((start (min idx1 idx2))
                   (end (max idx1 idx2))
                   (bcount (1+ (- end start))))
              (setf (editor-pending-block session) nil)
              (case block-cmd
                (:dd (delete-line-range session start bcount)
                     (setf did-modify t
                           navigate-to start
                           result-message (format nil "~D line~:P deleted" bcount)))
                (:rr (let ((copies (extract-line-range session start bcount)))
                       (insert-lines-after session (+ start bcount -1)
                                           (copy-list copies))
                       (setf did-modify t
                             navigate-to start
                             result-message (format nil "~D line~:P duplicated" bcount))))
                ((:cc :mm)
                 ;; Check for A/B target in same batch
                 (let ((target (find-target commands)))
                   (if target
                       (let ((msg (execute-block-with-target
                                   session block-cmd start bcount
                                   (second target) (first target))))
                         (if msg
                             (progn
                               (unless did-modify (pop (editor-undo-stack session)))
                               (return-from execute-prefix-commands msg))
                             (setf did-modify t
                                   navigate-to start
                                   result-message (format nil "~D line~:P ~A"
                                                          bcount
                                                          (if (eq block-cmd :cc) "copied" "moved")))))
                       ;; No target yet - pend
                       (progn
                         (setf (editor-pending-block session)
                               (list block-cmd start bcount))
                         (unless did-modify (pop (editor-undo-stack session)))
                         (return-from execute-prefix-commands
                           (format nil "~A block marked - enter A or B target"
                                   (if (eq block-cmd :cc) "Copy" "Move")))))))))))
        ;; Single marker - start or complete a pending block
        (when (= (length markers) 1)
          (let* ((marker (first markers))
                 (real-index (first marker)))
            (cond
              ;; No pending block - start one
              ((null pending)
               (setf (editor-pending-block session) (list block-cmd real-index))
               (unless did-modify (pop (editor-undo-stack session)))
               (return-from execute-prefix-commands
                 (format nil "~A pending - mark end line"
                         (string-upcase (symbol-name block-cmd)))))
              ;; Completing pending block (same type)
              ((eq block-cmd (first pending))
               (when (< real-index 0)
                 (setf (editor-pending-block session) nil)
                 (unless did-modify (pop (editor-undo-stack session)))
                 (return-from execute-prefix-commands
                   "Cannot use marker line as block endpoint"))
               (let* ((start (min real-index (second pending)))
                      (end (max real-index (second pending)))
                      (bcount (1+ (- end start))))
                 (setf (editor-pending-block session) nil)
                 (case block-cmd
                   (:dd (delete-line-range session start bcount)
                        (setf did-modify t
                              navigate-to start
                              result-message (format nil "~D line~:P deleted" bcount)))
                   (:rr (let ((copies (extract-line-range session start bcount)))
                          (insert-lines-after session (+ start bcount -1)
                                              (copy-list copies))
                          (setf did-modify t
                                navigate-to start
                                result-message (format nil "~D line~:P duplicated" bcount))))
                   ((:cc :mm)
                    ;; Check for A/B target in same batch
                    (let ((target (find-target commands)))
                      (if target
                          (let ((msg (execute-block-with-target
                                      session block-cmd start bcount
                                      (second target) (first target))))
                            (if msg
                                (progn
                                  (unless did-modify (pop (editor-undo-stack session)))
                                  (return-from execute-prefix-commands msg))
                                (setf did-modify t
                                      navigate-to start
                                      result-message (format nil "~D line~:P ~A"
                                                             bcount
                                                             (if (eq block-cmd :cc) "copied" "moved")))))
                          (progn
                            (setf (editor-pending-block session)
                                  (list block-cmd start bcount))
                            (unless did-modify (pop (editor-undo-stack session)))
                            (return-from execute-prefix-commands
                              (format nil "~A block marked - enter A or B target"
                                      (if (eq block-cmd :cc) "Copy" "Move"))))))))))
              ;; Conflicting type
              (t
               (setf (editor-pending-block session) nil)
               (unless did-modify (pop (editor-undo-stack session)))
               (return-from execute-prefix-commands
                 "Conflicting pending command - use RESET")))))))

    ;; Second pass: handle A/B targets for pending copy/move
    (when (and pending (member (first pending) '(:cc :mm :c :m)))
      (let ((target (find-target commands)))
        (when target
          (let ((msg (execute-block-with-target
                      session (first pending) (second pending) (third pending)
                      (second target) (first target))))
            (setf (editor-pending-block session) nil)
            (if msg
                (progn
                  (unless did-modify (pop (editor-undo-stack session)))
                  (return-from execute-prefix-commands msg))
                (let ((src-cmd (first pending))
                      (src-start (second pending))
                      (src-count (third pending)))
                  (setf did-modify t
                        navigate-to src-start
                        result-message (format nil "~D line~:P ~A"
                                               src-count
                                               (if (member src-cmd '(:cc :c))
                                                   "copied" "moved")))))))))

    ;; Third pass: single-line commands (process in order, tracking index shifts)
    (let ((offset 0))
      (dolist (cmd-entry commands)
        (destructuring-bind (real-index cmd count screen-row) cmd-entry
          (declare (ignore screen-row))
          (when (and (>= real-index 0) (< real-index (+ (line-count session) offset)))
            (let ((adjusted (+ real-index offset)))
              (case cmd
                (:i
                 (let ((new-lines (make-list count :initial-element "")))
                   (insert-lines-after session adjusted new-lines)
                   (incf offset count)
                   (setf did-modify t)))
                (:d
                 (let ((actual-count (min count (- (line-count session) adjusted))))
                   (when (plusp actual-count)
                     (delete-line-range session adjusted actual-count)
                     (decf offset actual-count)
                     (setf did-modify t))))
                (:r
                 (let ((line (line-at session adjusted)))
                   (when line
                     (let ((copies (make-list count :initial-element line)))
                       (insert-lines-after session adjusted copies)
                       (incf offset count)
                       (setf did-modify t)))))
                (:c
                 (setf (editor-pending-block session) (list :c adjusted count))
                 (return-from execute-prefix-commands
                   "Copy pending - enter A or B target"))
                (:m
                 (setf (editor-pending-block session) (list :m adjusted count))
                 (return-from execute-prefix-commands
                   "Move pending - enter A or B target"))
                (:uc
                 (dotimes (j count)
                   (let ((idx (+ adjusted j)))
                     (when (line-at session idx)
                       (setf (line-at session idx) (string-upcase (line-at session idx)))
                       (setf did-modify t)))))
                (:lc
                 (dotimes (j count)
                   (let ((idx (+ adjusted j)))
                     (when (line-at session idx)
                       (setf (line-at session idx) (string-downcase (line-at session idx)))
                       (setf did-modify t)))))
                (:shift-left
                 (when (line-at session adjusted)
                   (let* ((line (line-at session adjusted))
                          (shifted (if (> (length line) count)
                                       (subseq line count)
                                       "")))
                     (setf (line-at session adjusted) shifted)
                     (setf did-modify t))))
                (:shift-right
                 (when (line-at session adjusted)
                   (let* ((line (line-at session adjusted))
                          (shifted (concatenate 'string
                                                (make-string count :initial-element #\Space)
                                                line)))
                     (setf (line-at session adjusted) shifted)
                     (setf did-modify t))))
                ((:a :b)
                 nil)  ; standalone A/B without pending
                ((:dd :cc :mm :rr)
                 nil)))))))  ; already handled in first pass

    ;; If nothing was actually modified, pop the undo state we saved
    (unless did-modify
      (pop (editor-undo-stack session)))
    ;; Navigate to block start after execution
    (when navigate-to
      (setf (editor-top-line session) (max 0 navigate-to))
      (clamp-top-line session))
    result-message))

;;; ============================================================
;;; Primary command processing
;;; ============================================================

(defun parse-delimited-string (text pos)
  "Parse a delimited string starting at POS in TEXT.
If TEXT[POS] is a quote or slash, use it as delimiter. Otherwise parse a word.
Returns (values string new-pos)."
  (when (>= pos (length text))
    (return-from parse-delimited-string (values "" pos)))
  ;; Skip leading spaces
  (loop while (and (< pos (length text)) (char= (char text pos) #\Space))
        do (incf pos))
  (when (>= pos (length text))
    (return-from parse-delimited-string (values "" pos)))
  (let ((delim (char text pos)))
    (if (member delim '(#\/ #\' #\"))
        ;; Delimited string
        (let ((start (1+ pos))
              (end (position delim text :start (1+ pos))))
          (if end
              (values (subseq text start end) (1+ end))
              (values (subseq text start) (length text))))
        ;; Undelimited word
        (let ((end (or (position #\Space text :start pos) (length text))))
          (values (subseq text pos end) end)))))

(defun parse-change-args (text)
  "Parse CHANGE command arguments: /from/to/ [ALL] or from to [ALL].
Returns (values from-string to-string remainder)."
  (let ((pos 0)
        (len (length text)))
    ;; Skip leading spaces
    (loop while (and (< pos len) (char= (char text pos) #\Space))
          do (incf pos))
    (when (>= pos len)
      (return-from parse-change-args (values "" "" "")))
    (let ((delim (char text pos)))
      (if (member delim '(#\/ #\' #\"))
          ;; Delimited: /from/to/ remainder
          (let* ((start1 (1+ pos))
                 (end1 (position delim text :start start1))
                 (from (if end1 (subseq text start1 end1) (subseq text start1)))
                 (start2 (if end1 (1+ end1) len))
                 (end2 (when (< start2 len) (position delim text :start start2)))
                 (to (cond (end2 (subseq text start2 end2))
                           ((< start2 len) (subseq text start2))
                           (t "")))
                 (rest-start (if end2 (1+ end2) len))
                 (remainder (if (< rest-start len) (subseq text rest-start) "")))
            (values from to remainder))
          ;; Undelimited: word word [ALL]
          (multiple-value-bind (from pos1) (parse-delimited-string text pos)
            (multiple-value-bind (to pos2) (parse-delimited-string text pos1)
              (values from to (if (< pos2 len) (subseq text pos2) ""))))))))

(declaim (ftype (function (t t t) string) do-find))
(declaim (ftype (function (t t t t) string) do-change))
(declaim (ftype (function (t) t) save-editor-file))

(defun handle-primary-command (session command)
  "Process a primary (command-line) command.
Returns :stay, :back, or an error message string. NIL means unrecognized."
  (let* ((trimmed (string-trim '(#\Space) command))
         (upper (string-upcase trimmed))
         (parts (split-sequence:split-sequence #\Space upper
                                                :remove-empty-subseqs t)))
    (when (null parts)
      (return-from handle-primary-command :stay))
    (let ((cmd (first parts)))
      (cond
        ;; SAVE
        ((string= cmd "SAVE")
         (save-editor-file session)
         :stay)

        ;; CANCEL / CAN
        ((or (string= cmd "CANCEL") (string= cmd "CAN"))
         (setf (editor-modified session) nil)
         :back)

        ;; SUBMIT / FILE (save and exit)
        ((or (string= cmd "SUBMIT") (string= cmd "FILE"))
         (save-editor-file session)
         :back)

        ;; TOP
        ((string= cmd "TOP")
         (setf (editor-top-line session) 0)
         :stay)

        ;; BOTTOM / BOT
        ((or (string= cmd "BOTTOM") (string= cmd "BOT"))
         (setf (editor-top-line session)
               (max 0 (- (total-virtual-lines session) +page-size+)))
         (clamp-top-line session)
         :stay)

        ;; UP n
        ((string= cmd "UP")
         (let ((n (if (second parts) (parse-integer (second parts) :junk-allowed t) +page-size+)))
           (setf (editor-top-line session)
                 (max 0 (- (editor-top-line session) (or n +page-size+))))
           :stay))

        ;; DOWN n
        ((string= cmd "DOWN")
         (let ((n (if (second parts) (parse-integer (second parts) :junk-allowed t) +page-size+)))
           (setf (editor-top-line session)
                 (+ (editor-top-line session) (or n +page-size+)))
           (clamp-top-line session)
           :stay))

        ;; LOCATE n / L n
        ((or (string= cmd "LOCATE") (string= cmd "L"))
         (let ((n (when (second parts) (parse-integer (second parts) :junk-allowed t))))
           (if n
               (progn
                 ;; Line number is 1-based, virtual index = real + 1
                 (setf (editor-top-line session) (max 0 n))
                 (clamp-top-line session)
                 :stay)
               "LOCATE requires a line number")))

        ;; FIND string
        ((or (string= cmd "FIND") (string= cmd "F"))
         (let ((search-str (if (> (length trimmed) (length cmd))
                               (string-trim '(#\Space) (subseq trimmed (length cmd)))
                               "")))
           (multiple-value-bind (str end) (parse-delimited-string search-str 0)
             (declare (ignore end))
             (if (plusp (length str))
                 (progn
                   (setf (editor-last-find session) str)
                   (setf (editor-last-find-line session) (editor-top-line session))
                   (do-find session str nil))
                 "FIND requires a search string"))))

        ;; CHANGE /s1/s2/ [ALL]
        ((or (string= cmd "CHANGE") (string= cmd "CHG"))
         (let ((rest (if (> (length trimmed) (length cmd))
                         (string-trim '(#\Space) (subseq trimmed (length cmd)))
                         "")))
           (multiple-value-bind (from to remainder)
               (parse-change-args rest)
             (let ((all-p (string-equal (string-trim '(#\Space) remainder) "ALL")))
               (if (plusp (length from))
                   (progn
                     (setf (editor-last-change session) (list from to all-p))
                     (do-change session from to all-p))
                   "CHANGE requires search and replace strings")))))

        ;; LEFT n
        ((string= cmd "LEFT")
         (let ((n (if (second parts) (parse-integer (second parts) :junk-allowed t) +data-width+)))
           (setf (editor-col-offset session)
                 (max 0 (- (editor-col-offset session) (or n +data-width+))))
           :stay))

        ;; RIGHT n
        ((string= cmd "RIGHT")
         (let ((n (if (second parts) (parse-integer (second parts) :junk-allowed t) +data-width+)))
           (setf (editor-col-offset session)
                 (+ (editor-col-offset session) (or n +data-width+)))
           :stay))

        ;; RESET
        ((or (string= cmd "RESET") (string= cmd "RES"))
         (setf (editor-pending-block session) nil)
         :stay)

        ;; UNDO
        ((string= cmd "UNDO")
         (undo session))

        ;; Bare number: jump to that line (shortcut for LOCATE)
        ((every #'digit-char-p cmd)
         (let ((n (parse-integer cmd)))
           (setf (editor-top-line session) (max 0 n))
           (clamp-top-line session)
           :stay))

        (t nil)))))

(defun do-find (session search-str advance-p)
  "Find the next occurrence of SEARCH-STR.
When ADVANCE-P is true, starts searching from the line after the last match.
Returns a message string."
  (let* ((start-line (if advance-p
                         (1+ (editor-last-find-line session))
                         (max 0 (1- (editor-top-line session)))))
         (lines (editor-lines session))
         (n (length lines))
         (upper-search (string-upcase search-str)))
    ;; Search from start-line to end
    (loop for i from start-line below n
          when (search upper-search (string-upcase (nth i lines)))
            do (setf (editor-top-line session) (1+ i))  ; virtual = real + 1
               (setf (editor-last-find-line session) i)
               (clamp-top-line session)
               (return-from do-find
                 (format nil "CHARS '~A' found on line ~D" search-str (1+ i))))
    ;; Wrap around from top
    (loop for i from 0 below (min start-line n)
          when (search upper-search (string-upcase (nth i lines)))
            do (setf (editor-top-line session) (1+ i))
               (setf (editor-last-find-line session) i)
               (clamp-top-line session)
               (return-from do-find
                 (format nil "CHARS '~A' found on line ~D (wrapped)" search-str (1+ i))))
    (format nil "CHARS '~A' not found" search-str)))

(defun do-change (session from to all-p)
  "Change FROM to TO in the file. If ALL-P, change all occurrences.
Both modes use case-insensitive matching. Returns a message string."
  (save-undo-state session)
  (let ((count 0)
        (start-line (max 0 (1- (editor-top-line session))))
        (upper-from (string-upcase from))
        (from-len (length from)))
    (if all-p
        ;; Change all occurrences in all lines (case-insensitive)
        (loop for i from 0 below (line-count session)
              do (let* ((line (nth i (editor-lines session)))
                        (result (make-string-output-stream))
                        (search-start 0)
                        (line-upper (string-upcase line))
                        (changed nil))
                   (loop for pos = (search upper-from line-upper :start2 search-start)
                         while pos
                         do (write-string (subseq line search-start pos) result)
                            (write-string to result)
                            (incf count)
                            (setf search-start (+ pos from-len))
                            (setf changed t))
                   (when changed
                     (write-string (subseq line search-start) result)
                     (setf (nth i (editor-lines session))
                           (get-output-stream-string result)))))
        ;; Change first occurrence from current position (case-insensitive)
        (loop for i from start-line below (line-count session)
              do (let* ((line (nth i (editor-lines session)))
                        (pos (search upper-from (string-upcase line))))
                   (when pos
                     (setf (nth i (editor-lines session))
                           (concatenate 'string
                                        (subseq line 0 pos)
                                        to
                                        (subseq line (+ pos from-len))))
                     (setf (editor-top-line session) (1+ i))
                     (clamp-top-line session)
                     (incf count)
                     (return)))))
    (if (plusp count)
        (progn
          (setf (editor-modified session) t)
          (format nil "CHANGED ~D occurrence~:P" count))
        (progn
          ;; Nothing changed, remove undo state
          (pop (editor-undo-stack session))
          (format nil "CHARS '~A' not found" from)))))

(defun save-editor-file (session)
  "Save the editor buffer to disk."
  (let ((path (editor-filepath session)))
    (when path
      (write-file-lines path (editor-lines session))
      (setf (editor-modified session) nil))))

;;; ============================================================
;;; Screen data population (used by screen-update)
;;; ============================================================

(defun build-screen-data (session)
  "Build prefix and data strings for display.
Returns (values prefix-string data-string) as newline-joined strings
suitable for the framework's repeat field split mechanism."
  (let ((top (editor-top-line session))
        (col-offset (editor-col-offset session))
        (n (line-count session))
        (pending (editor-pending-block session))
        (prefix-lines '())
        (data-lines '()))
    (dotimes (i +page-size+)
      (let ((virtual (+ top i)))
        (cond
          ;; Top of Data marker
          ((= virtual 0)
           (push "******" prefix-lines)
           (push "***************************** Top of Data ******************************"
                 data-lines))
          ;; Bottom of Data marker
          ((= virtual (1+ n))
           (push "******" prefix-lines)
           (push "**************************** Bottom of Data ****************************"
                 data-lines))
          ;; File line
          ((and (> virtual 0) (<= virtual n))
           (let* ((real (1- virtual))
                  (line (or (nth real (editor-lines session)) ""))
                  (pending-start-p (and pending (= real (second pending)))))
             (push (if pending-start-p
                       (format nil "~6A" (string-upcase (symbol-name (first pending))))
                       (format nil "~6,'0D" (1+ real)))
                   prefix-lines)
             (push (visible-portion line col-offset) data-lines)))
          ;; Past end
          (t
           (push "" prefix-lines)
           (push "" data-lines)))))
    (values (format nil "~{~A~^~%~}" (nreverse prefix-lines))
            (format nil "~{~A~^~%~}" (nreverse data-lines)))))

;;; ============================================================
;;; Edit processing (called before any key action)
;;; ============================================================

(defun pending-prefix-for-line-p (session real-index prefix-val)
  "Return T if PREFIX-VAL matches the pending command marker that
build-screen-data would have placed on this line. This means the user
did not type a new command - the prefix is just our own marker echoed back."
  (let ((pending (editor-pending-block session)))
    (when (and pending real-index (= real-index (second pending)))
      (let* ((cmd-name (string-upcase (symbol-name (first pending))))
             (expected (format nil "~6A" cmd-name))
             (trimmed (string-right-trim '(#\Space) prefix-val))
             (expected-trimmed (string-right-trim '(#\Space) expected)))
        (string-equal trimmed expected-trimmed)))))

(defun process-data-edits (session prefix-lines data-lines)
  "Compare data fields with current buffer and apply edits.
Also collects prefix commands. Returns a list of (real-index cmd count row)."
  (let ((top (editor-top-line session))
        (col-offset (editor-col-offset session))
        (commands '()))
    (dotimes (i +page-size+)
      (let* ((virtual (+ top i))
             (real (virtual-to-real session virtual))
             (is-marker (marker-line-p session virtual))
             (prefix-val (if (< i (length prefix-lines))
                             (nth i prefix-lines) ""))
             (data-val (if (< i (length data-lines))
                           (nth i data-lines) "")))
        ;; Apply data edits for real file lines
        (when (and real (not is-marker))
          (let ((original (visible-portion (line-at session real) col-offset)))
            (unless (string= (string-right-trim '(#\Space) data-val)
                             (string-right-trim '(#\Space) original))
              (apply-edit session real data-val))))
        ;; Parse prefix commands (skip if it's our own pending marker echoed back)
        (unless (pending-prefix-for-line-p session real prefix-val)
        (multiple-value-bind (cmd count) (parse-prefix-command prefix-val)
          (when cmd
            (cond
              ;; Top-of-Data marker: allow I (insert at top), A/B targets
              ((and is-marker (= virtual 0))
               (when (member cmd '(:i :a :b))
                 ;; Use real-index -1 to signal "before first line"
                 (push (list -1 cmd count i) commands)))
              ;; Bottom-of-Data marker: allow I (append), A/B targets
              ((and is-marker (= virtual (1+ (line-count session))))
               (when (member cmd '(:i :a :b))
                 (push (list (1- (line-count session)) cmd count i) commands)))
              ;; Regular file line
              (real
               (push (list real cmd count i) commands))
              ;; Empty row past EOF
              (t
               (when (member cmd '(:a :b))
                 (push (list (line-count session) cmd count i) commands)))))))))
    (nreverse commands)))

(defun process-editor-changes (session context)
  "Process data edits and prefix commands from the current response.
CONTEXT is the field-values hash table. Returns error/info message or nil."
  (let* ((prefix-str (or (gethash "prefix" context) ""))
         (data-str (or (gethash "data" context) ""))
         (prefix-lines (split-sequence:split-sequence #\Newline prefix-str))
         (data-lines (split-sequence:split-sequence #\Newline data-str))
         (commands (process-data-edits session prefix-lines data-lines)))
    (when commands
      (execute-prefix-commands session commands))))

;;; ============================================================
;;; Application definition
;;; ============================================================

(lspf:define-application *editor-app*
  :entry-screen open
  :screen-directory (merge-pathnames
                     #P"examples/editor/screens/"
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
