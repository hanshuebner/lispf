;;;; -*- Mode: Lisp; Coding: utf-8 -*-
;;;; Embed static frontend assets into the Lisp image

(in-package #:screen-editor)

(defvar *static-files* (make-hash-table :test #'equal)
  "Hash table mapping URL paths to (content-type . content-bytes).")

(defun mime-type-for (path)
  "Return a MIME type string for PATH based on its extension."
  (let ((ext (pathname-type (pathname path))))
    (cond
      ((string-equal ext "html") "text/html; charset=utf-8")
      ((string-equal ext "js") "application/javascript; charset=utf-8")
      ((string-equal ext "css") "text/css; charset=utf-8")
      ((string-equal ext "json") "application/json")
      ((string-equal ext "svg") "image/svg+xml")
      ((string-equal ext "png") "image/png")
      ((string-equal ext "ico") "image/x-icon")
      (t "application/octet-stream"))))

(defun load-static-files ()
  "Read all files from frontend/dist/ into *static-files*."
  (let ((dist-dir (merge-pathnames
                   "frontend/dist/"
                   (asdf:system-source-directory :lispf-editor))))
    (clrhash *static-files*)
    (labels ((walk (dir)
               (dolist (entry (uiop:directory-files dir))
                 (let* ((rel (enough-namestring entry dist-dir))
                        (url-path (concatenate 'string "/" rel))
                        (bytes (alexandria:read-file-into-byte-vector entry)))
                   (setf (gethash url-path *static-files*)
                         (cons (mime-type-for rel) bytes))))
               (dolist (subdir (uiop:subdirectories dir))
                 (walk subdir))))
      (walk dist-dir)
      (format t "Loaded ~D static files into image.~%" (hash-table-count *static-files*)))))

(defun create-static-dispatcher ()
  "Return a Hunchentoot dispatcher that serves from *static-files*."
  (lambda (request)
    (let* ((path (hunchentoot:script-name request))
           (entry (or (gethash path *static-files*)
                      ;; SPA fallback: serve index.html for non-API, non-file paths
                      (unless (or (search "/api/" path)
                                  (gethash path *static-files*))
                        (gethash "/index.html" *static-files*)))))
      (when entry
        (lambda ()
          (setf (hunchentoot:content-type*) (car entry))
          (cdr entry))))))

;; Load files when this module is compiled/loaded
(load-static-files)
