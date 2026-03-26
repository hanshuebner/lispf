;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; help-parser.lisp
;;;;
;;;; Parser for .help files. Format:
;;;;   First line: page title
;;;;   Remaining lines: body text with optional inline links
;;;;   Link syntax: {target:visible text} or {target} (shorthand)
;;;;   Lines starting with # are comments (stripped on display)

(in-package #:lispf)

;;; Help page data structures

(defclass help-page ()
  ((title :initarg :title :accessor help-page-title)
   (lines :initarg :lines :accessor help-page-lines
          :documentation "List of parsed lines. Each line is a list of segments:
strings and help-link instances.")
   (source-path :initarg :source-path :accessor help-page-source-path
                :initform nil)))

(defclass help-link ()
  ((target :initarg :target :accessor help-link-target)
   (text :initarg :text :accessor help-link-text)))

;;; Parsing

(defun parse-help-line (line)
  "Parse a single line of help text, extracting link markup.
Returns a list of segments: strings and help-link instances."
  (let ((segments '())
        (pos 0)
        (len (length line)))
    (loop while (< pos len)
          for open = (position #\{ line :start pos)
          do (if (null open)
                 (progn
                   (when (< pos len)
                     (push (subseq line pos) segments))
                   (setf pos len))
                 (let ((close (position #\} line :start (1+ open))))
                   (unless close
                     ;; No matching close brace, treat as literal text
                     (push (subseq line pos) segments)
                     (setf pos len)
                     (return))
                   ;; Text before the link
                   (when (> open pos)
                     (push (subseq line pos open) segments))
                   ;; Parse link content
                   (let* ((content (subseq line (1+ open) close))
                          (colon (position #\: content)))
                     (if colon
                         (push (make-instance 'help-link
                                              :target (subseq content 0 colon)
                                              :text (subseq content (1+ colon)))
                               segments)
                         (push (make-instance 'help-link
                                              :target content
                                              :text content)
                               segments)))
                   (setf pos (1+ close)))))
    (nreverse segments)))

(defun parse-help-file (path)
  "Read and parse a .help file. Returns a help-page instance."
  (with-open-file (s path :direction :input :if-does-not-exist nil)
    (unless s
      (return-from parse-help-file nil))
    (let* ((title (or (read-line s nil "") ""))
           (body-lines '()))
      (loop for line = (read-line s nil nil)
            while line
            ;; Skip comment lines
            unless (and (plusp (length line)) (char= (char line 0) #\#))
              do (push (parse-help-line line) body-lines))
      (make-instance 'help-page
                     :title title
                     :lines (nreverse body-lines)
                     :source-path path))))

;;; Help file search

(defun find-help-file (name-string)
  "Search the current application's directories for NAME-STRING.help."
  (loop for dir in (application-screen-directories *application*)
        for path = (merge-pathnames (make-pathname :name name-string :type "help") dir)
        when (probe-file path) return path))

;;; Display rendering

(defclass link-position ()
  ((line :initarg :line :initform 0 :accessor link-position-line)
   (col-start :initarg :col-start :initform 0 :accessor link-position-col-start)
   (col-end :initarg :col-end :initform 0 :accessor link-position-col-end)
   (target :initarg :target :initform "" :accessor link-position-target))
  (:documentation "Position of a link on a rendered display line."))

(defun render-help-line (segments)
  "Render a parsed help line to display text.
Returns (values display-string link-positions) where link-positions
is a list of (col-start col-end target)."
  (let ((out (make-string-output-stream))
        (links '())
        (col 0))
    (dolist (seg segments)
      (etypecase seg
        (string
         (write-string seg out)
         (incf col (length seg)))
        (help-link
         (let* ((text (help-link-text seg))
                (start col))
           (write-string text out)
           (incf col (length text))
           (push (list start col (help-link-target seg)) links)))))
    (values (get-output-stream-string out)
            (nreverse links))))

(defun render-help-page (page)
  "Render a help-page for display.
Returns (values display-lines link-map) where display-lines is a list
of strings and link-map is a list of link-position structs with absolute
line numbers."
  (let ((display-lines '())
        (link-map '())
        (line-num 0))
    (dolist (parsed-line (help-page-lines page))
      (multiple-value-bind (text links) (render-help-line parsed-line)
        (push text display-lines)
        (dolist (link links)
          (push (make-instance 'link-position :line line-num
                                    :col-start (first link)
                                    :col-end (second link)
                                    :target (third link))
                link-map))
        (incf line-num)))
    (values (nreverse display-lines)
            (nreverse link-map))))

;;; Segment-based editing support

(defclass help-edit-segment ()
  ((kind :initarg :kind :initform :text :accessor help-edit-segment-kind)
   (text :initarg :text :initform "" :accessor help-edit-segment-text)
   (target :initarg :target :initform nil :accessor help-edit-segment-target)
   (raw :initarg :raw :initform "" :accessor help-edit-segment-raw)
   (display-start :initarg :display-start :initform 0 :accessor help-edit-segment-display-start)
   (display-end :initarg :display-end :initform 0 :accessor help-edit-segment-display-end))
  (:documentation "A segment of a help line for edit mapping."))

(defun parse-help-segments (line)
  "Parse LINE into segments with display position tracking.
Returns a list of help-edit-segment structs."
  (let ((segments '())
        (pos 0)
        (display-pos 0)
        (len (length line)))
    (loop while (< pos len)
          for open = (position #\{ line :start pos)
          do (if (null open)
                 (let ((text (subseq line pos)))
                   (push (make-instance 'help-edit-segment
                          :kind :text :text text :raw text
                          :display-start display-pos
                          :display-end (+ display-pos (length text)))
                         segments)
                   (incf display-pos (length text))
                   (setf pos len))
                 (let ((close (position #\} line :start (1+ open))))
                   (unless close
                     (let ((text (subseq line pos)))
                       (push (make-instance 'help-edit-segment
                              :kind :text :text text :raw text
                              :display-start display-pos
                              :display-end (+ display-pos (length text)))
                             segments)
                       (incf display-pos (length text))
                       (setf pos len))
                     (return))
                   ;; Text before link
                   (when (> open pos)
                     (let ((text (subseq line pos open)))
                       (push (make-instance 'help-edit-segment
                              :kind :text :text text :raw text
                              :display-start display-pos
                              :display-end (+ display-pos (length text)))
                             segments)
                       (incf display-pos (length text))))
                   ;; Link segment
                   (let* ((raw (subseq line open (1+ close)))
                          (content (subseq line (1+ open) close))
                          (colon (position #\: content))
                          (target (if colon (subseq content 0 colon) content))
                          (text (if colon (subseq content (1+ colon)) content)))
                     (push (make-instance 'help-edit-segment
                            :kind :link :text text :target target :raw raw
                            :display-start display-pos
                            :display-end (+ display-pos (length text)))
                           segments)
                     (incf display-pos (length text)))
                   (setf pos (1+ close)))))
    (nreverse segments)))

(defun reconstruct-help-line (segments new-display)
  "Reconstruct a raw help line from SEGMENTS using NEW-DISPLAY text.
Plain text segments get their display text replaced.
Link segments preserve {target:} and get their visible text replaced."
  (with-output-to-string (out)
    (let ((display-len (length new-display)))
      (dolist (seg segments)
        (let* ((ds (help-edit-segment-display-start seg))
               (de (help-edit-segment-display-end seg))
               (new-text (cond
                           ((>= ds display-len) "")
                           ((> de display-len) (subseq new-display ds))
                           (t (subseq new-display ds de)))))
          (ecase (help-edit-segment-kind seg)
            (:text (write-string new-text out))
            (:link
             (let ((trimmed (string-right-trim '(#\Space) new-text)))
               (if (string= trimmed (help-edit-segment-text seg))
                   ;; Unchanged, preserve original raw form
                   (write-string (help-edit-segment-raw seg) out)
                   ;; Changed, reconstruct with target preserved
                   (progn
                     (write-char #\{ out)
                     (write-string (help-edit-segment-target seg) out)
                     (write-char #\: out)
                     (write-string trimmed out)
                     (write-char #\} out)))))))))))

(defun apply-help-line-edit (raw-line col-offset data-width new-data)
  "Apply a display-space edit to a raw help line.
COL-OFFSET and DATA-WIDTH define the visible window in display space.
NEW-DATA is the edited content for that window.
Returns the new raw line."
  (let* ((segments (parse-help-segments raw-line))
         (stripped (strip-help-markup raw-line))
         (before (subseq stripped 0 (min col-offset (length stripped))))
         (after (if (> (length stripped) (+ col-offset data-width))
                    (subseq stripped (+ col-offset data-width))
                    ""))
         (new-display (concatenate 'string
                                   before
                                   (string-right-trim '(#\Space) new-data)
                                   after)))
    (reconstruct-help-line segments new-display)))

;;; Link editing support

(defun find-link-segment-at-col (segments display-col)
  "Find the link segment at DISPLAY-COL, or NIL."
  (dolist (seg segments)
    (when (and (eq (help-edit-segment-kind seg) :link)
               (>= display-col (help-edit-segment-display-start seg))
               (< display-col (help-edit-segment-display-end seg)))
      (return seg))))

(defun wrap-word-as-link (raw-line display-col target)
  "Wrap the word at DISPLAY-COL in RAW-LINE as a link to TARGET.
Returns the new raw line, or NIL if no word at that position."
  (let* ((stripped (strip-help-markup raw-line))
         (len (length stripped)))
    (when (or (>= display-col len)
              (char= (char stripped display-col) #\Space))
      (return-from wrap-word-as-link nil))
    ;; Find word boundaries
    (let ((start (1+ (or (position #\Space stripped :end display-col :from-end t) -1)))
          (end (or (position #\Space stripped :start display-col) len)))
      (let* ((word (subseq stripped start end))
             (segments (parse-help-segments raw-line))
             ;; Check if already a link
             (existing (find-link-segment-at-col segments display-col)))
        (when existing
          (return-from wrap-word-as-link nil))
        ;; Build new display with link markup inserted
        (let ((new-display (concatenate 'string
                                        (subseq stripped 0 start)
                                        "{" target ":" word "}"
                                        (subseq stripped end))))
          ;; Reconstruct through segments to preserve existing links
          ;; For simplicity, rebuild from scratch since we're inserting new markup
          new-display)))))

(defun remove-link-at-col (raw-line display-col)
  "Remove link markup at DISPLAY-COL, leaving plain text.
Returns the new raw line, or NIL if no link at that position."
  (let ((segments (parse-help-segments raw-line)))
    (when-let (seg (find-link-segment-at-col segments display-col))
      (with-output-to-string (out)
        (dolist (s segments)
          (if (eq s seg)
              (write-string (help-edit-segment-text s) out)
              (write-string (help-edit-segment-raw s) out)))))))

;;; Markup stripping for editor integration

(defun strip-help-markup (line)
  "Strip help link markup from LINE for WYSIWYG display.
Replaces {target:text} with text and {target} with target."
  (with-output-to-string (out)
    (let ((pos 0)
          (len (length line)))
      (loop while (< pos len)
            for open = (position #\{ line :start pos)
            do (if (null open)
                   (progn
                     (write-string line out :start pos)
                     (setf pos len))
                   (let ((close (position #\} line :start (1+ open))))
                     (unless close
                       (write-string line out :start pos)
                       (setf pos len)
                       (return))
                     (write-string line out :start pos :end open)
                     (let* ((content (subseq line (1+ open) close))
                            (colon (position #\: content)))
                       (if colon
                           (write-string content out :start (1+ colon))
                           (write-string content out)))
                     (setf pos (1+ close))))))))
