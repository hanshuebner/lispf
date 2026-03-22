;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; commands.lisp - Primary command processing (FIND, CHANGE, SAVE, etc.)

(in-package #:lispf-editor)


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

(defun command-keyword (cmd-string)
  "Intern CMD-STRING as a keyword symbol for case dispatch."
  (find-symbol cmd-string :keyword))

(defun parse-command-arg-n (parts)
  "Parse an optional numeric argument from the second element of PARTS."
  (when (second parts)
    (parse-integer (second parts) :junk-allowed t)))

(defun handle-find-command (session trimmed cmd-str)
  "Handle the FIND/F command. Returns a message string."
  (let ((search-str (if (> (length trimmed) (length cmd-str))
                        (string-trim '(#\Space) (subseq trimmed (length cmd-str)))
                        "")))
    (multiple-value-bind (str end) (parse-delimited-string search-str 0)
      (declare (ignore end))
      (if (plusp (length str))
          (progn
            (setf (editor-last-find session) str)
            (setf (editor-last-find-line session) (editor-top-line session))
            (do-find session str nil))
          "FIND requires a search string"))))

(defun handle-change-command (session trimmed cmd-str)
  "Handle the CHANGE/CHG command. Returns a message string."
  (let ((rest (if (> (length trimmed) (length cmd-str))
                  (string-trim '(#\Space) (subseq trimmed (length cmd-str)))
                  "")))
    (multiple-value-bind (from to remainder)
        (parse-change-args rest)
      (let ((all-p (string-equal (string-trim '(#\Space) remainder) "ALL")))
        (if (plusp (length from))
            (progn
              (setf (editor-last-change session) (list from to all-p))
              (do-change session from to all-p))
            "CHANGE requires search and replace strings")))))

(defun handle-justify-command (session parts)
  "Handle the JUSTIFY/JUS command. Returns a message string."
  (let* ((width (or (parse-command-arg-n parts) +data-width+))
         (range (editor-justify-range session))
         (start (if range (car range) 0))
         (count (if range (cdr range) (line-count session))))
    (when range
      (setf (editor-justify-range session) nil))
    (save-undo-state session)
    (let ((old-lines (extract-line-range session start count)))
      (multiple-value-bind (new-lines long-words)
          (justify-lines old-lines width)
        (delete-line-range session start count)
        (insert-lines-after session (1- start) new-lines)
        (setf (editor-modified session) t)
        (setf (editor-top-line session) (max 0 start))
        (clamp-top-line session)
        (if (plusp long-words)
            (format nil "Justified ~D line~:P at width ~D (~D word~:P exceed width)"
                    count width long-words)
            (format nil "Justified ~D line~:P to ~D at width ~D"
                    count (length new-lines) width))))))

(defun handle-set-command (session parts)
  "Handle SET subcommands. PARTS is the split command with SET already consumed.
Returns a message string or :stay."
  (let* ((layout (editor-layout session))
         (sub (command-keyword (or (second parts) "")))
         (arg (third parts)))
    (case sub
      (:CURLINE
       (let ((n (when arg (parse-integer arg :junk-allowed t))))
         (if (and n (>= n (layout-data-start-row layout))
                    (<= n (layout-data-end-row layout)))
             (progn
               (setf (layout-scale-row layout) n)  ; curline position for scale
               (format nil "CURLINE set to ~D" n))
             (format nil "CURLINE must be between ~D and ~D"
                     (layout-data-start-row layout) (layout-data-end-row layout)))))
      ((:SCALE :SCA)
       (cond
         ((null arg) "SET SCALE ON/OFF or SET SCALE n")
         ((string-equal arg "ON")
          (setf (layout-scale-row layout)
                (or (layout-scale-row layout)
                    (+ (layout-data-start-row layout)
                       (floor (page-size layout) 2))))
          "Scale line enabled")
         ((string-equal arg "OFF")
          (setf (layout-scale-row layout) nil)
          "Scale line disabled")
         (t (let ((n (parse-integer arg :junk-allowed t)))
              (if (and n (>= n (layout-data-start-row layout))
                         (<= n (layout-data-end-row layout)))
                  (progn
                    (setf (layout-scale-row layout) n)
                    (format nil "Scale line set to row ~D" n))
                  (format nil "Scale row must be between ~D and ~D"
                          (layout-data-start-row layout)
                          (layout-data-end-row layout)))))))
      (:ALT
       (let ((n (when arg (parse-integer arg :junk-allowed t))))
         (if n
             (progn
               (setf (editor-alteration-count session) n)
               (format nil "Alt set to ~D" n))
             "SET ALT requires a number")))
      (:TRUNC
       (let ((n (when arg (parse-integer arg :junk-allowed t))))
         (if (and n (> n 0))
             (format nil "TRUNC set to ~D (not yet implemented)" n)
             "SET TRUNC requires a positive number")))
      (:PROMPT
       (let ((text (if (> (length parts) 2)
                       (format nil "~{~A~^ ~}" (cddr parts))
                       "====>")))
         (setf (layout-command-prompt layout) text)
         (format nil "Prompt set to ~S" text)))
      (otherwise
       (format nil "Unknown SET option: ~A" (or (second parts) ""))))))

(defun handle-primary-command (session command)
  "Process a primary (command-line) command.
Returns :stay, :back, or an error message string. NIL means unrecognized."
  (let* ((trimmed (string-trim '(#\Space) command))
         (upper (string-upcase trimmed))
         (parts (split-sequence:split-sequence #\Space upper
                                               :remove-empty-subseqs t)))
    (when (null parts)
      (return-from handle-primary-command :stay))
    (let ((cmd-key (command-keyword (first parts))))
      (case cmd-key
        (:SAVE
         (save-editor-file session)
         :stay)

        ((:CANCEL :CAN)
         (if (editor-restricted-p session)
             "CANCEL not available in restricted mode"
             (progn (setf (editor-modified session) nil) :back)))

        ((:SUBMIT :FILE)
         (save-editor-file session)
         :back)

        (:TOP
         (setf (editor-top-line session) 0)
         :stay)

        ((:BOTTOM :BOT)
         (setf (editor-top-line session)
               (max 0 (- (total-virtual-lines session) (page-size session))))
         (clamp-top-line session)
         :stay)

        (:UP
         (let ((n (or (parse-command-arg-n parts) (page-size session))))
           (setf (editor-top-line session)
                 (max 0 (- (editor-top-line session) n)))
           :stay))

        (:DOWN
         (let ((n (or (parse-command-arg-n parts) (page-size session))))
           (setf (editor-top-line session)
                 (+ (editor-top-line session) n))
           (clamp-top-line session)
           :stay))

        ((:LOCATE :L)
         (let ((n (parse-command-arg-n parts)))
           (if n
               (progn
                 (setf (editor-top-line session) (max 0 n))
                 (clamp-top-line session)
                 :stay)
               "LOCATE requires a line number")))

        ((:FIND :F)
         (handle-find-command session trimmed (first parts)))

        ((:CHANGE :CHG)
         (handle-change-command session trimmed (first parts)))

        (:LEFT
         (let ((n (or (parse-command-arg-n parts) +data-width+)))
           (setf (editor-col-offset session)
                 (max 0 (- (editor-col-offset session) n)))
           :stay))

        (:RIGHT
         (let ((n (or (parse-command-arg-n parts) +data-width+)))
           (setf (editor-col-offset session)
                 (+ (editor-col-offset session) n))
           :stay))

        ((:RESET :RES)
         (setf (editor-pending-block session) nil)
         (setf (editor-justify-range session) nil)
         :stay)

        (:UNDO
         (undo session))

        (:SET
         (handle-set-command session parts))

        (:HELP
         (let* ((topic (second parts))
                (help-name (if topic
                               (format nil "help-~A" (string-downcase topic))
                               "help-edit"))
                (app-package (lspf::application-package lspf:*application*)))
           (if (lspf::find-screen-file help-name)
               (lspf::intern-screen-name help-name app-package)
               (format nil "~A: help topic not found" (or topic "HELP")))))

        ((:REVERT :REV)
         (if (editor-restricted-p session)
             "REVERT not available in restricted mode"
             (revert session)))

        ((:JUSTIFY :JUS)
         (handle-justify-command session parts))

        (otherwise
         (let ((cmd-str (first parts)))
           (when (every #'digit-char-p cmd-str)
             (let ((n (parse-integer cmd-str)))
               (setf (editor-top-line session) (max 0 n))
               (clamp-top-line session)
               :stay))))))))

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
FROM is a regular expression (cl-ppcre). Case-insensitive matching.
TO can use regex backreferences (\\1, \\2, etc.).
Returns a message string."
  (save-undo-state session)
  (handler-case
      (let ((scanner (cl-ppcre:create-scanner from :case-insensitive-mode t))
            (count 0)
            (start-line (max 0 (1- (editor-top-line session)))))
        (if all-p
            ;; Change all occurrences in all lines
            (loop for i from 0 below (line-count session)
                  do (let* ((line (nth i (editor-lines session)))
                            (matches (cl-ppcre:all-matches scanner line))
                            (new-line (cl-ppcre:regex-replace-all scanner line to)))
                       (incf count (/ (length matches) 2))
                       (unless (string= line new-line)
                         (setf (nth i (editor-lines session)) new-line))))
            ;; Change first occurrence from current position
            (loop for i from start-line below (line-count session)
                  do (multiple-value-bind (new-line match-p)
                         (cl-ppcre:regex-replace scanner (nth i (editor-lines session)) to)
                       (when match-p
                         (setf (nth i (editor-lines session)) new-line)
                         (setf (editor-top-line session) (1+ i))
                         (clamp-top-line session)
                         (incf count)
                         (return)))))
        (if (plusp count)
            (progn
              (setf (editor-modified session) t)
              (format nil "CHANGED ~D occurrence~:P" count))
            (progn
              (pop (editor-undo-stack session))
              (format nil "'~A' not found" from))))
    (cl-ppcre:ppcre-syntax-error (c)
      (pop (editor-undo-stack session))
      (format nil "Invalid regex: ~A" c))))


