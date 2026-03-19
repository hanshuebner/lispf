;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; registry.lisp
;;;;
;;;; Screen registry: lazy-loading screens from .screen files by name.
;;;; Framework manages rows 0 (title), 22 (error), and 23 (key labels).
;;;; Screen files contain 21 application rows (rows 1-21 of the display).

(in-package #:lispf)

;;; Constants

(defconstant +app-rows+ 21
  "Number of application rows in a screen (24 total minus 3 framework rows).")

;;; Registry state

(defvar *screen-registry* (make-hash-table :test 'equal)
  "Maps screen name strings to cl3270:screen objects.")

(defvar *screen-rules-registry* (make-hash-table :test 'equal)
  "Maps screen name strings to rules hash tables.")

(defvar *screen-keys-registry* (make-hash-table :test 'equal)
  "Maps screen name strings to key spec lists.")

(defvar *screen-transient-fields-registry* (make-hash-table :test 'equal)
  "Maps screen name strings to lists of transient field name strings.")

(defvar *screen-repeat-fields-registry* (make-hash-table :test 'equal)
  "Maps screen name strings to alists of (base-name-string . count).")

(defvar *screen-file-timestamps* (make-hash-table :test 'equal)
  "Maps screen name strings to file-write-date of the loaded .screen file.")

(defvar *screen-directories* '()
  "List of directories to search for .screen files.")

(defvar *application-name* ""
  "Application name displayed in the title line of every screen.")

;;; Directory management

(defun register-screen-directory (directory)
  "Add DIRECTORY to the screen search path."
  (pushnew (truename directory) *screen-directories* :test #'equal))

;;; Name resolution

(defun screen-name-string (screen-name)
  "Convert SCREEN-NAME (symbol or string) to a lowercase string."
  (etypecase screen-name
    (string screen-name)
    (symbol (string-downcase (symbol-name screen-name)))))

(defun find-screen-file (name-string)
  "Search *screen-directories* for NAME-STRING.screen. Returns pathname or nil."
  (loop for dir in *screen-directories*
        for path = (merge-pathnames (make-pathname :name name-string :type "screen") dir)
        when (probe-file path) return path))

;;; Framework row management

(defun pad-screen-string (screen-string)
  "Wrap application screen content (up to 21 rows) with framework rows.
Adds blank rows for title (row 0), error message (row 22), and key labels (row 23)."
  (let* ((parts (split-sequence:split-sequence #\Newline screen-string))
         (app-rows (rest parts))
         (app-rows (if (and app-rows (string= "" (car (last app-rows))))
                       (butlast app-rows)
                       app-rows))
         (n (length app-rows)))
    (when (> n +app-rows+)
      (error "Screen has ~D application rows, maximum is ~D" n +app-rows+))
    (with-output-to-string (s)
      (terpri s)                                ; leading \n
      (terpri s)                                ; row 0: blank (title)
      (dolist (row app-rows)
        (write-string row s) (terpri s))        ; app rows
      (dotimes (i (- +app-rows+ n))
        (terpri s))                             ; padding to 21 app rows
      (terpri s))))                             ; row 22: blank (errormsg)
                                                ; row 23 = trailing "" (keys)

(defun offset-field-rows (field-definitions)
  "Add 1 to all :from row values to account for the framework title row."
  (mapcar (lambda (field-def)
            (let* ((copy (copy-list field-def))
                   (from (getf copy :from)))
              (when from
                (setf (getf copy :from)
                      (list (1+ (first from)) (second from))))
              copy))
          field-definitions))

;;; Repeat field expansion

(defun expand-repeat-fields (field-definitions)
  "Expand fields with :repeat N into N individual fields.
For each field with :repeat N (N > 1), generates N copies with :from row
incremented, :name suffixed with .0 ... .N-1, :repeat removed.
Returns (values expanded-fields repeat-groups-alist)."
  (let ((expanded '())
        (groups '()))
    (dolist (field-def field-definitions)
      (let ((repeat (getf field-def :repeat))
            (name (getf field-def :name))
            (from (getf field-def :from)))
        (if (and repeat (> repeat 1) name from)
            (let ((base-name (string-downcase (string name))))
              (push (list base-name repeat (getf field-def :len)) groups)
              (dotimes (i repeat)
                (let ((copy (copy-list field-def)))
                  (setf (getf copy :from)
                        (list (+ (first from) i) (second from)))
                  (setf (getf copy :name)
                        (intern (format nil "~A.~D"
                                        (string-upcase (string name)) i)
                                (symbol-package name)))
                  (remf copy :repeat)
                  (push copy expanded))))
            (let ((copy (copy-list field-def)))
              (remf copy :repeat)
              (push copy expanded)))))
    (values (nreverse expanded) (nreverse groups))))

;;; Repeat field value utilities

(defun join-repeat-field-values (repeat-groups hash-table)
  "For each repeat group, join individual field values into a single value.
Reads \"name.0\" ... \"name.N-1\", right-trims each, joins with newlines,
stores as \"name\", and removes individual keys."
  (dolist (group repeat-groups)
    (destructuring-bind (base-name count &optional len) group
      (declare (ignore len))
      (let ((lines '()))
        (dotimes (i count)
          (let ((key (format nil "~A.~D" base-name i)))
            (push (string-right-trim '(#\Space) (or (gethash key hash-table) ""))
                  lines)
            (remhash key hash-table)))
        ;; Trim trailing empty lines
        (setf lines (nreverse lines))
        (loop while (and lines (string= "" (car (last lines))))
              do (setf lines (butlast lines)))
        (setf (gethash base-name hash-table)
              (format nil "~{~A~^~%~}" lines))))))

(defun split-repeat-field-value (repeat-groups field-len-fn hash-table)
  "For each repeat group, split the single value into individual field values.
FIELD-LEN-FN is a fallback function that takes a base-name and returns the field length.
Splits value on newlines, pads/truncates each line to field-len,
distributes to \"name.0\" ... \"name.N-1\", removes base key."
  (dolist (group repeat-groups)
    (destructuring-bind (base-name count &optional stored-len) group
      (let* ((value (or (gethash base-name hash-table) ""))
             (lines (split-sequence:split-sequence #\Newline value))
             (field-len (or stored-len (funcall field-len-fn base-name))))
        (remhash base-name hash-table)
        (dotimes (i count)
          (let* ((line (if (< i (length lines)) (nth i lines) ""))
                 (padded (if (> (length line) field-len)
                             (subseq line 0 field-len)
                             line)))
            (setf (gethash (format nil "~A.~D" base-name i) hash-table)
                  padded)))))))

(defun repeat-field-length (screen base-name)
  "Find the field length for a repeat group by looking up \"base-name.0\" in SCREEN."
  (let ((target (format nil "~A.0" base-name)))
    (dolist (f (cl3270:screen-fields screen))
      (when (string-equal (cl3270:field-name f) target)
        (return-from repeat-field-length (length (cl3270:field-content f))))))
  ;; Fallback
  80)

(defun make-framework-fields ()
  "Create the three framework-managed fields: title, errormsg, keys."
  (list
   (cl3270:make-field :row 0 :col 0 :name "title")
   (cl3270:make-field :row 21 :col 79 :name "errormsg" :color cl3270:+red+)
   (cl3270:make-field :row 22 :col 79 :name "keys")))

(defun format-title-line (screen-name)
  "Format the title line: SCREEN-NAME   *APPLICATION-NAME*   date time"
  (let* ((left (string-upcase (screen-name-string screen-name)))
         (right (concatenate 'string (cl3270:today-date) " " (cl3270:now-time)))
         (center *application-name*)
         (used (+ (length left) (length center) (length right)))
         (total-gap (max 2 (- 78 used)))
         (left-gap (floor total-gap 2))
         (right-gap (- total-gap left-gap)))
    (format nil "~A~V@T~A~V@T~A" left left-gap center right-gap right)))

;;; Rule extraction from .screen field definitions

(defun extract-rules-from-fields (field-definitions)
  "Extract :required, :must-change, :error-text, :transient from field definitions.
Returns (values cleaned-field-definitions rules-hash-table-or-nil transient-field-names)."
  (let ((rules (make-hash-table :test 'equal))
        (cleaned '())
        (transient-fields '()))
    (dolist (field-def field-definitions)
      (let* ((name (getf field-def :name))
             (required (getf field-def :required))
             (must-change (getf field-def :must-change))
             (error-text (getf field-def :error-text))
             (transient (getf field-def :transient))
             (clean (copy-list field-def)))
        (remf clean :required)
        (remf clean :must-change)
        (remf clean :error-text)
        (remf clean :transient)
        (remf clean :default)
        (push clean cleaned)
        (when (and name transient)
          (push (string-downcase (string name)) transient-fields))
        (when (and name (or required must-change error-text))
          (let ((field-name (string-downcase (string name))))
            (setf (gethash field-name rules)
                  (cl3270:field-rules field-name
                                      :must-change (or must-change required)
                                      :error-text (or error-text
                                                      (format nil "Please enter a valid value for ~A." field-name))
                                      :validator (if required #'cl3270:non-blank-validator (constantly t))))))))
    (values (nreverse cleaned)
            (when (plusp (hash-table-count rules)) rules)
            (nreverse transient-fields))))

;;; Screen compilation

(defun normalize-key-specs (key-specs)
  "Normalize key specs from a screen plist. Each entry is (aid-keyword label &key back goto).
Ensures aid-keyword is a keyword, label is a string, and :goto value is a keyword."
  (mapcar (lambda (spec)
            (destructuring-bind (aid-kw label &rest rest) spec
              (let ((normalized-rest (copy-list rest))
                    (goto-val (getf rest :goto)))
                (when goto-val
                  (setf (getf normalized-rest :goto)
                        (intern (string-upcase (string goto-val)) :keyword)))
                (list* (intern (string aid-kw) :keyword)
                       (string label)
                       normalized-rest))))
          key-specs))

(defun compile-screen-data (name data)
  "Compile a screen data plist into a screen object with framework fields.
Returns (values screen rules keys transient-fields repeat-groups)."
  (let* ((screen-string (pad-screen-string (getf data :screen)))
         (raw-fields (getf data :fields))
         (raw-keys (getf data :keys)))
    (multiple-value-bind (expanded-fields repeat-groups)
        (expand-repeat-fields raw-fields)
      (multiple-value-bind (clean-fields rules transient-fields)
          (extract-rules-from-fields expanded-fields)
        (let* ((offset-fields (offset-field-rows clean-fields))
               (mapped-fields (mapcar #'map-field-attributes offset-fields))
               (field-forms (parse-screen screen-string mapped-fields))
               (app-fields (mapcar (lambda (form)
                                     (let ((f (eval form)))
                                       (let ((name (cl3270:field-name f)))
                                         (declare (ignorable name))
                                         (unless (stringp name)
                                           (setf (cl3270:field-name f)
                                                 (string-downcase (string name)))))
                                       f))
                                   field-forms))
               (all-fields (append app-fields (make-framework-fields)))
               (keys (when raw-keys (normalize-key-specs raw-keys))))
          (values (apply #'cl3270:make-screen name all-fields)
                  rules
                  keys
                  transient-fields
                  repeat-groups))))))

;;; Registry operations

(defun load-and-register-screen (name-string)
  "Load a screen from disk and register it. Returns the screen object."
  (let ((path (find-screen-file name-string)))
    (unless path
      (error "Screen file ~A.screen not found in directories: ~S"
             name-string *screen-directories*))
    (let ((data (load-screen-data path)))
      (multiple-value-bind (screen rules keys transient-fields repeat-groups)
          (compile-screen-data name-string data)
        (setf (gethash name-string *screen-registry*) screen)
        (setf (gethash name-string *screen-rules-registry*) rules)
        (setf (gethash name-string *screen-keys-registry*) keys)
        (setf (gethash name-string *screen-transient-fields-registry*) transient-fields)
        (setf (gethash name-string *screen-repeat-fields-registry*) repeat-groups)
        (setf (gethash name-string *screen-file-timestamps*) (file-write-date path))
        screen))))

(defun screen-file-changed-p (name-string)
  "Check if the .screen file on disk is newer than the cached version.
Also returns true if a screen is cached but has no recorded timestamp
\(loaded before timestamp tracking was added)."
  (let ((path (find-screen-file name-string))
        (cached-time (gethash name-string *screen-file-timestamps*)))
    (when (and path (gethash name-string *screen-registry*))
      (or (null cached-time)
          (let ((disk-time (file-write-date path)))
            (and disk-time (> disk-time cached-time)))))))

(defun get-screen (screen-name)
  "Get a screen by name, loading from .screen file if not cached or changed on disk.
SCREEN-NAME may be a symbol (converted to lowercase) or a string."
  (let ((name-string (ensure-screen-loaded screen-name)))
    (gethash name-string *screen-registry*)))

(defun ensure-screen-loaded (screen-name)
  "Ensure SCREEN-NAME is loaded and up-to-date. Reloads if file changed on disk."
  (let ((name-string (screen-name-string screen-name)))
    (when (screen-file-changed-p name-string)
      (load-and-register-screen name-string))
    (unless (gethash name-string *screen-registry*)
      (load-and-register-screen name-string))
    name-string))

(defun get-screen-rules (screen-name)
  "Get the declared rules for a screen. Returns a rules hash table or nil."
  (let ((name-string (ensure-screen-loaded screen-name)))
    (gethash name-string *screen-rules-registry*)))

(defun get-screen-keys (screen-name)
  "Get the declared key specs for a screen. Returns a list of key specs or nil."
  (let ((name-string (ensure-screen-loaded screen-name)))
    (gethash name-string *screen-keys-registry*)))

(defun get-screen-transient-fields (screen-name)
  "Get the list of transient field name strings for a screen."
  (let ((name-string (ensure-screen-loaded screen-name)))
    (gethash name-string *screen-transient-fields-registry*)))

(defun get-screen-repeat-fields (screen-name)
  "Get the repeat groups alist for a screen. Each entry is (base-name-string . count)."
  (let ((name-string (ensure-screen-loaded screen-name)))
    (gethash name-string *screen-repeat-fields-registry*)))

(defun reload-screen (screen-name)
  "Force reload a screen from disk."
  (let ((name-string (screen-name-string screen-name)))
    (remhash name-string *screen-registry*)
    (remhash name-string *screen-rules-registry*)
    (remhash name-string *screen-keys-registry*)
    (remhash name-string *screen-transient-fields-registry*)
    (remhash name-string *screen-repeat-fields-registry*)
    (remhash name-string *screen-file-timestamps*)
    (load-and-register-screen name-string)))

(defun reload-all-screens ()
  "Clear the screen cache, forcing all screens to reload on next access."
  (clrhash *screen-registry*)
  (clrhash *screen-rules-registry*)
  (clrhash *screen-keys-registry*)
  (clrhash *screen-transient-fields-registry*)
  (clrhash *screen-repeat-fields-registry*)
  (clrhash *screen-file-timestamps*)
  (when (boundp '*validated-screens*)
    (clrhash *validated-screens*)))

;;; Rule merging

(defun merge-screen-rules (screen-rules app-rules)
  "Merge screen-declared rules with application-provided rules.
APP-RULES takes precedence over SCREEN-RULES. Returns nil if both are nil."
  (cond
    ((and (null screen-rules) (null app-rules)) nil)
    ((null screen-rules) app-rules)
    ((null app-rules) screen-rules)
    (t (let ((merged (make-hash-table :test 'equal)))
         (maphash (lambda (k v) (setf (gethash k merged) v)) screen-rules)
         (maphash (lambda (k v) (setf (gethash k merged) v)) app-rules)
         merged))))

;;; Field name validation

(defun check-field-names (screen &rest field-name-symbols)
  "Warn if any FIELD-NAME-SYMBOLS don't match named fields in SCREEN."
  (let ((screen-field-names
          (remove ""
                  (mapcar (lambda (f) (string (cl3270:field-name f)))
                          (cl3270:screen-fields screen))
                  :test #'string=)))
    (dolist (sym field-name-symbols)
      (let ((name (string-downcase (symbol-name sym))))
        (unless (member name screen-field-names :test #'string-equal)
          (warn "Field ~A not found in screen ~S. Known fields: ~{~A~^, ~}"
                sym (cl3270:screen-name screen)
                (mapcar #'string-upcase screen-field-names)))))))

;;; Simple screen display

(defun display-screen (screen-name conn &key devinfo codepage values cursor no-response)
  "Display a registered screen without the handle-screen validation loop.
Automatically populates the title line. Useful for informational screens."
  (let* ((screen (get-screen screen-name))
         (vals (or values (cl3270:make-dict :test #'equal))))
    (unless (nth-value 1 (gethash "title" vals))
      (setf (gethash "title" vals) (format-title-line screen-name)))
    (cl3270:show-screen-opts screen vals conn
                             (cl3270:make-screen-opts
                              :altscreen devinfo
                              :codepage codepage
                              :cursor-row (if cursor (first cursor) 0)
                              :cursor-col (if cursor (second cursor) 0)
                              :no-response no-response))))
