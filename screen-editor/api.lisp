;;;; -*- Mode: Lisp; Coding: utf-8 -*-

(in-package #:screen-editor)

(defun json-response (data &optional (status 200))
  "Encode DATA as JSON and return it as an HTTP response."
  (setf (hunchentoot:content-type*) "application/json")
  (setf (hunchentoot:return-code*) status)
  (with-output-to-string (s)
    (yason:encode data s)))

(defun json-error (message &optional (status 400))
  "Return a JSON error response."
  (let ((ht (make-hash-table :test #'equal)))
    (setf (gethash "error" ht) message)
    (json-response ht status)))

(defun field-alist-to-hash-table (field)
  "Convert a field alist to a hash table for JSON encoding."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair field ht)
      (setf (gethash (car pair) ht)
            (if (null (cdr pair)) :null (cdr pair))))))

(defun key-alist-to-hash-table (key-alist)
  "Convert a key spec alist to a hash table for JSON encoding."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair key-alist ht)
      (setf (gethash (car pair) ht)
            (if (null (cdr pair)) :null (cdr pair))))))

(defun screen-alist-to-hash-table (screen)
  "Convert a screen alist to a hash table for JSON encoding."
  (let ((ht (make-hash-table :test #'equal)))
    (dolist (pair screen ht)
      (let ((key (car pair))
            (val (cdr pair)))
        (setf (gethash key ht)
              (cond
                ((string= key "fields")
                 (coerce (mapcar #'field-alist-to-hash-table val) 'vector))
                ((string= key "keys")
                 (coerce (mapcar #'key-alist-to-hash-table val) 'vector))
                ((string= key "rows")
                 (coerce val 'vector))
                ((null val) :null)
                (t val)))))))

(defun parse-json-body ()
  "Parse the JSON request body into a hash table."
  (let ((body (hunchentoot:raw-post-data :force-text t)))
    (yason:parse body)))

(defun hash-table-to-field-alist (ht)
  "Convert a JSON hash table for a field to our alist format."
  (list (cons "fromRow" (gethash "fromRow" ht))
        (cons "fromCol" (gethash "fromCol" ht))
        (cons "len" (gethash "len" ht))
        (cons "name" (let ((v (gethash "name" ht))) (if (eq v :null) nil v)))
        (cons "write" (gethash "write" ht))
        (cons "intense" (gethash "intense" ht))
        (cons "hidden" (gethash "hidden" ht))
        (cons "numericOnly" (gethash "numericOnly" ht))
        (cons "autoskip" (gethash "autoskip" ht))
        (cons "color" (or (gethash "color" ht) "default"))
        (cons "highlighting" (or (gethash "highlighting" ht) "default"))
        (cons "keepspaces" (gethash "keepspaces" ht))
        (cons "positionOnly" (gethash "positionOnly" ht))
        (cons "default" (gethash "default" ht))
        (cons "transient" (gethash "transient" ht))
        (cons "repeat" (or (gethash "repeat" ht) 1))))

(defun hash-table-to-key-alist (ht)
  "Convert a JSON hash table for a key spec to our alist format."
  (let ((result (list (cons "aidKey" (gethash "aidKey" ht))
                      (cons "label" (gethash "label" ht))
                      (cons "action" (or (gethash "action" ht) "handler")))))
    (let ((hidden (gethash "hidden" ht)))
      (when (and hidden (not (eq hidden :null)) (not (eq hidden :false)))
        (setf result (append result (list (cons "hidden" t))))))
    (let ((goto (gethash "gotoScreen" ht)))
      (when (and goto (not (eq goto :null)))
        (setf result (append result (list (cons "gotoScreen" goto))))))
    result))

(defun hash-table-to-screen-alist (ht)
  "Convert a JSON hash table for a screen to our alist format."
  (let ((result (list (cons "name" (gethash "name" ht))
                      (cons "rows" (coerce (gethash "rows" ht) 'list))
                      (cons "fields" (mapcar #'hash-table-to-field-alist
                                             (coerce (gethash "fields" ht) 'list))))))
    (let ((keys (gethash "keys" ht)))
      (when (and keys (not (eq keys :null)))
        (setf result (append result
                             (list (cons "keys"
                                         (mapcar #'hash-table-to-key-alist
                                                 (coerce keys 'list))))))))
    result))

(defun screen-path (name)
  "Return the full path for screen NAME in the screen directory."
  (merge-pathnames (make-pathname :name name :type "screen") *screen-directory*))

;;; List screens endpoint

(defun handle-get-screens ()
  "Handle GET /api/screens. Returns sorted list of available screen names."
  (let* ((files (directory (merge-pathnames "*.screen" *screen-directory*)))
         (names (sort (mapcar (lambda (f) (pathname-name f)) files) #'string<))
         (ht (make-hash-table :test #'equal)))
    (setf (gethash "screens" ht) (coerce names 'vector))
    (json-response ht)))

;;; Screen endpoint: load/save by name

(defun handle-get-screen ()
  "Handle GET /api/screen?name=<screen-name>."
  (let* ((name (hunchentoot:get-parameter "name"))
         (path (when name (screen-path name))))
    (unless name
      (return-from handle-get-screen (json-error "Missing 'name' parameter")))
    (unless (probe-file path)
      (return-from handle-get-screen (json-error (format nil "Screen not found: ~A" name) 404)))
    (handler-case
        (let ((screen (parse-screen-file path)))
          (json-response (screen-alist-to-hash-table screen)))
      (error (e)
        (json-error (format nil "Parse error: ~A" e) 500)))))

(defun handle-post-screen ()
  "Handle POST /api/screen. Saves screen data to a .screen file."
  (handler-case
      (let* ((data (parse-json-body))
             (name (gethash "name" data))
             (path (when name (screen-path name)))
             (screen (hash-table-to-screen-alist data)))
        (unless name
          (return-from handle-post-screen (json-error "Missing 'name' field")))
        (write-screen-file screen (namestring path))
        (let ((ht (make-hash-table :test #'equal)))
          (setf (gethash "ok" ht) t)
          (json-response ht)))
    (error (e)
      (json-error (format nil "Save error: ~A" e) 500))))

(defun define-api-routes ()
  "Set up API route handlers."
  (hunchentoot:define-easy-handler (api-screens :uri "/api/screens")
      ()
    (case (hunchentoot:request-method*)
      (:get (handle-get-screens))
      (otherwise
       (setf (hunchentoot:return-code*) 405)
       (json-error "Method not allowed" 405))))
  (hunchentoot:define-easy-handler (api-screen :uri "/api/screen")
      ()
    (case (hunchentoot:request-method*)
      (:get (handle-get-screen))
      (:post (handle-post-screen))
      (otherwise
       (setf (hunchentoot:return-code*) 405)
       (json-error "Method not allowed" 405)))))
