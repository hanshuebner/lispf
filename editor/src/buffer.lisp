;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; buffer.lisp - Line buffer utilities

(in-package #:lispf-editor)

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
  (setf (editor-modified session) t)
  (incf (editor-alteration-count session)))

(defun insert-lines-before (session real-index new-lines)
  "Insert NEW-LINES before REAL-INDEX."
  (insert-lines-after session (1- real-index) new-lines))

(defun delete-line-range (session start-index count)
  "Delete COUNT lines starting at START-INDEX."
  (let ((lines (editor-lines session)))
    (setf (editor-lines session)
          (append (subseq lines 0 (min start-index (length lines)))
                  (subseq lines (min (+ start-index count) (length lines))))))
  (setf (editor-modified session) t)
  (incf (editor-alteration-count session)))

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
        (setf (editor-modified session) t)
        (incf (editor-alteration-count session))))))

(defun clamp-top-line (session)
  "Clamp top-line to valid range.
Allows scrolling past the end so Bottom-of-Data can reach the top of the screen."
  (setf (editor-top-line session)
        (max 0 (min (editor-top-line session)
                     (max 0 (1- (total-virtual-lines session)))))))
