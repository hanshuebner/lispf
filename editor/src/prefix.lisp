;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; prefix.lisp - Prefix command parsing and execution

(in-package #:lispf-editor)

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
        ((and (>= (length trimmed) 2) (string= trimmed "JJ" :end1 2))
         (let ((width (parse-prefix-count trimmed 2)))
           (values :jj width)))
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

(defun justify-lines (lines width)
  "Justify/reflow a list of lines to fit within WIDTH columns.
Preserves paragraph breaks (empty lines). Returns a new list of lines."
  (let ((result '())
        (current-paragraph '()))
    (flet ((flush-paragraph ()
             (when current-paragraph
               (let* ((words (loop for line in (nreverse current-paragraph)
                                   nconc (split-sequence:split-sequence
                                          #\Space line :remove-empty-subseqs t)))
                      (output-lines '())
                      (current-line ""))
                 (dolist (word words)
                   (cond
                     ;; First word on line
                     ((zerop (length current-line))
                      (setf current-line word))
                     ;; Word fits on current line
                     ((<= (+ (length current-line) 1 (length word)) width)
                      (setf current-line (concatenate 'string current-line " " word)))
                     ;; Start new line
                     (t
                      (push current-line output-lines)
                      (setf current-line word))))
                 (when (plusp (length current-line))
                   (push current-line output-lines))
                 (dolist (line (nreverse output-lines))
                   (push line result)))
               (setf current-paragraph nil))))
      (dolist (line lines)
        (if (every (lambda (c) (char= c #\Space)) line)
            (progn
              (flush-paragraph)
              (push "" result))
            (push line current-paragraph)))
      (flush-paragraph))
    (nreverse result)))

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
    (dolist (block-cmd '(:dd :cc :mm :rr :jj))
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
                (:jj (let* ((marker-width (third first-marker))
                            (jj-width (if (and marker-width (> marker-width 1))
                                          marker-width +data-width+))
                            (old-lines (extract-line-range session start bcount))
                            (new-lines (justify-lines old-lines jj-width)))
                       (delete-line-range session start bcount)
                       (insert-lines-after session (1- start) new-lines)
                       (setf did-modify t
                             navigate-to start
                             result-message (format nil "~D line~:P justified to ~D at width ~D"
                                                    bcount (length new-lines) jj-width))))
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
              ;; No pending block - start one (store count for JJ width)
              ((null pending)
               (setf (editor-pending-block session)
                     (list block-cmd real-index (third marker)))
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
                   (:jj (let* ((pending-width (third pending))
                               (marker-width (third marker))
                               (jj-width (cond
                                           ((and pending-width (> pending-width 1))
                                            pending-width)
                                           ((and marker-width (> marker-width 1))
                                            marker-width)
                                           (t +data-width+)))
                               (old-lines (extract-line-range session start bcount))
                               (new-lines (justify-lines old-lines jj-width)))
                          (delete-line-range session start bcount)
                          (insert-lines-after session (1- start) new-lines)
                          (setf did-modify t
                                navigate-to start
                                result-message (format nil "~D line~:P justified to ~D at width ~D"
                                                       bcount (length new-lines) jj-width))))
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
                ((:dd :cc :mm :rr :jj)
                 nil)))))))  ; already handled in first pass

    ;; If nothing was actually modified, pop the undo state we saved
    (unless did-modify
      (pop (editor-undo-stack session)))
    ;; Navigate to block start after execution
    (when navigate-to
      (setf (editor-top-line session) (max 0 navigate-to))
      (clamp-top-line session))
    result-message))

