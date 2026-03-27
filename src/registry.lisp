;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; registry.lisp
;;;;
;;;; Screen registry: lazy-loading screens from .screen files by name.
;;;; Framework manages rows 0 (title), 21 (command), 22 (error), and 23 (key labels).
;;;; Screen files contain 20 application rows (rows 1-20 of the display).

(in-package #:lispf)

;;; Constants

(defconstant +app-rows+ 20
  "Number of application rows in a screen (24 total minus 4 framework rows).")

;;; Per-screen compiled data

(defclass screen-info ()
  ((screen :initarg :screen :accessor screen-info-screen)
   (rules :initarg :rules :accessor screen-info-rules)
   (keys :initarg :keys :accessor screen-info-keys)
   (key-layout :initarg :key-layout :accessor screen-info-key-layout)
   (transient-fields :initarg :transient-fields :accessor screen-info-transient-fields)
   (repeat-groups :initarg :repeat-groups :accessor screen-info-repeat-groups)
   (dynamic-areas :initarg :dynamic-areas :initform nil :accessor screen-info-dynamic-areas)
   (has-command :initarg :has-command :initform nil :accessor screen-info-has-command)
   (menu :initarg :menu :initform nil :accessor screen-info-menu)
   (aliases :initarg :aliases :initform nil :accessor screen-info-aliases)
   (anonymous :initarg :anonymous :initform nil :accessor screen-info-anonymous)
   (roles :initarg :roles :initform nil :accessor screen-info-roles)
   (navigable :initarg :navigable :initform nil :accessor screen-info-navigable)
   (handler-package :initarg :handler-package :initform nil :accessor screen-info-handler-package)
   (full-control :initarg :full-control :initform nil :accessor screen-info-full-control)
   (overlay :initarg :overlay :initform nil :accessor screen-info-overlay)
   (file-timestamp :initarg :file-timestamp :initform 0 :accessor screen-info-file-timestamp))
  (:documentation "All compiled data for a single screen."))

(defclass dynamic-area ()
  ((name :initarg :name :initform "" :accessor dynamic-area-name)
   (from-row :initarg :from-row :initform 0 :accessor dynamic-area-from-row)
   (from-col :initarg :from-col :initform 0 :accessor dynamic-area-from-col)
   (to-row :initarg :to-row :initform 0 :accessor dynamic-area-to-row)
   (to-col :initarg :to-col :initform 0 :accessor dynamic-area-to-col))
  (:documentation "A host-updateable screen region."))

;;; Name resolution

(defun screen-name-string (screen-name)
  "Convert SCREEN-NAME (symbol or string) to a lowercase string."
  (etypecase screen-name
    (string screen-name)
    (symbol (string-downcase (symbol-name screen-name)))))

(defun find-screen-file (name-string)
  "Search the current application's directories for NAME-STRING.screen."
  (loop for dir in (application-screen-directories *application*)
        for path = (merge-pathnames (make-pathname :name name-string :type "screen") dir)
        when (probe-file path) return path))

;;; Directory management

(defun register-screen-directory (directory)
  "Add DIRECTORY to the current application's screen search path."
  (pushnew (truename directory)
           (application-screen-directories *application*)
           :test #'equal))

;;; Framework row management

(defun pad-screen-string (screen-string &key has-command full-control start-row)
  "Wrap application screen content with framework rows.
With HAS-COMMAND: up to 20 app rows + command line (row 21).
Without: up to 21 app rows, no command line.
With FULL-CONTROL: all 24 rows (0-23) belong to the application.
START-ROW: when set, prepend blank rows so content begins at this row number.
Always adds title (row 0), error (row 22), and keys (row 23) unless FULL-CONTROL."
  (let* ((max-rows (cond (full-control 24)
                         ((not has-command) 21)
                         (t +app-rows+)))
         (parts (split-sequence:split-sequence #\Newline screen-string))
         (app-rows (rest parts))
         (app-rows (if (and app-rows (string= "" (car (last app-rows))))
                       (butlast app-rows)
                       app-rows))
         (app-rows (if start-row
                       (append (make-list start-row :initial-element "") app-rows)
                       app-rows))
         (n (length app-rows)))
    (when (> n max-rows)
      (error "Screen has ~D application rows, maximum is ~D" n max-rows))
    (with-output-to-string (s)
      (terpri s)                                ; leading \n
      (if full-control
          (progn
            ;; Full control: all 24 rows are app content
            ;; Write rows with newlines between them (not trailing)
            (dotimes (i max-rows)
              (when (plusp i) (terpri s))
              (when (< i n)
                (write-string (nth i app-rows) s))))
          (progn
            (terpri s)                          ; row 0: blank (title)
            (dolist (row app-rows)
              (write-string row s) (terpri s))  ; app rows
            (dotimes (i (- max-rows n))
              (terpri s))                       ; padding to max app rows
            (when has-command
              (terpri s))                       ; row 21: blank (command)
            (terpri s))))))

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

(defun make-framework-fields (&key has-command full-control)
  "Create framework-managed fields: title, command line (when HAS-COMMAND), errormsg, keys.
With FULL-CONTROL, no framework fields are created (app manages all rows)."
  (when full-control
    (return-from make-framework-fields '()))
  (append
   (list (cl3270:make-field :row 0 :col 0 :name "title" :position-only t)
         (cl3270:make-field :row 0 :col 79 :name ""))
   (when has-command
     (list (cl3270:make-field :row 21 :col 0 :name "cmdlabel"
                              :content "Command ==>" :color cl3270:+turquoise+)
           (cl3270:make-field :row 21 :col 13 :name "command"
                              :write t :highlighting cl3270:+underscore+)))
   (list (cl3270:make-field :row 21 :col 79 :name "errormsg" :color cl3270:+red+
                            :len 79)
         (cl3270:make-field :row 22 :col 79 :name "keys" :len 79))))

(defun now-time-hhmm ()
  "Return current time as HH:MM string."
  (multiple-value-bind (s m h) (decode-universal-time (get-universal-time))
    (declare (ignore s))
    (format nil "~2,'0D:~2,'0D" h m)))

(defun format-title-line (screen-name &optional indicators)
  "Format the title line: SCREEN APP  ...IND2 IND1  YYYY-MM-DD HH:MM.
Screen name is truncated to 20 characters, app name to 10.
Indicators are right-aligned, stacked from the date leftward, separated
from the date by two spaces and from each other by one space.
Warns at runtime if indicators do not fit the available space."
  (let* ((screen (subseq (string-upcase (screen-name-string screen-name))
                         0 (min 20 (length (screen-name-string screen-name)))))
         (app (let ((name (or (application-title *application*)
                              (application-name *application*))))
                (subseq name 0 (min 10 (length name)))))
         (right (concatenate 'string (cl3270:today-date) " " (now-time-hhmm)))
         (left (format nil "~20A ~A" screen app))
         (ind-str (if indicators
                      (format nil "~{~A~^ ~}" indicators)
                      ""))
         (right-block (if (plusp (length ind-str))
                          (concatenate 'string ind-str "  " right)
                          right))
         (available (- 79 (length left) 1))
         (line (make-string 79 :initial-element #\Space)))
    (when (and indicators (> (length right-block) available))
      (warn "Title line indicators overflow: ~S needs ~D chars, ~D available"
            ind-str (length ind-str) (- available (length right) 2)))
    (replace line left :start1 0)
    (let ((rb-start (max (1+ (length left))
                         (- 79 (length right-block)))))
      (replace line right-block
               :start1 rb-start
               :end1 (min (+ rb-start (length right-block)) 79)))
    line))

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
  "Normalize key specs from a screen plist.
Each entry is (aid-keyword label &key back goto hidden).
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

(defun compute-key-layout (key-specs)
  "Compute fixed column positions for key labels from KEY-SPECS.
Returns an alist of (aid-keyword column width) entries. Keys with non-empty
labels (including :hidden keys) get a reserved slot. The layout defines
where each key's label appears on row 23, ensuring positions don't shift
when keys are shown or hidden at runtime."
  (let ((layout nil)
        (col 1))                        ; start at column 1 (leading space)
    (dolist (spec key-specs)
      (destructuring-bind (aid-kw label &rest rest) spec
        (when (or (string/= label "") (getf rest :hidden))
          (let* ((display-label (if (string= label "") ; hidden key with no label
                                    (or (getf rest :default-label)
                                        (symbol-name aid-kw))
                                    label))
                 (prefix (format nil "~A " (symbol-name aid-kw)))
                 (width (+ (length prefix) (length display-label))))
            (push (list aid-kw col width) layout)
            (incf col (+ width 2))))))  ; 2 spaces between keys
    (nreverse layout)))

(defun compile-dynamic-areas (specs)
  "Compile dynamic area specifications from screen data into dynamic-area structs."
  (mapcar (lambda (spec)
            (make-instance 'dynamic-area
                           :name (string-downcase (string (getf spec :name)))
                           :from-row (first (getf spec :from))
                           :from-col (second (getf spec :from))
                           :to-row (first (getf spec :to))
                           :to-col (second (getf spec :to))))
          specs))

(defun compile-screen-data (name data)
  "Compile a screen data plist into a screen-info struct."
  (let* ((menu-name (let ((m (getf data :menu)))
                      (when m (string-downcase (string m)))))
         (has-command (or (getf data :command) menu-name))
         (aliases (mapcar (lambda (a) (string-downcase (string a)))
                          (getf data :aliases)))
         (anonymous (getf data :anonymous))
         (roles (mapcar (lambda (r) (intern (string-upcase (string r)) :keyword))
                        (getf data :roles)))
         (navigable (getf data :navigable))
         (handler-package (let ((hp (getf data :handler-package)))
                            (when hp (find-package (string-upcase (string hp))))))
         (full-control (getf data :full-control))
         (overlay (getf data :overlay))
         (start-row (getf data :start-row))
         (screen-string (pad-screen-string (getf data :screen)
                                            :has-command has-command
                                            :full-control full-control
                                            :start-row start-row))
         (raw-fields (getf data :fields))
         (raw-keys (getf data :keys))
         (raw-dynamic-areas (getf data :dynamic-areas)))
    (multiple-value-bind (expanded-fields repeat-groups)
        (expand-repeat-fields raw-fields)
      (multiple-value-bind (clean-fields rules transient-fields)
          (extract-rules-from-fields expanded-fields)
        (let* ((positioned-fields (if full-control
                                      clean-fields
                                      (offset-field-rows clean-fields)))
               (mapped-fields (mapcar #'map-field-attributes positioned-fields))
               (field-forms (parse-screen screen-string mapped-fields))
               (app-fields (mapcar #'eval field-forms))
               (all-fields (append app-fields
                                   (make-framework-fields :has-command has-command
                                                          :full-control full-control)))
               (keys (when raw-keys (normalize-key-specs raw-keys)))
               (key-layout (when keys (compute-key-layout keys))))
          (make-instance 'screen-info
           :screen (apply #'cl3270:make-screen name all-fields)
           :rules rules
           :keys keys
           :key-layout key-layout
           :transient-fields transient-fields
           :repeat-groups repeat-groups
           :dynamic-areas (when raw-dynamic-areas
                            (compile-dynamic-areas raw-dynamic-areas))
           :has-command has-command
           :menu menu-name
           :aliases aliases
           :anonymous anonymous
           :roles roles
           :navigable navigable
           :handler-package handler-package
           :full-control full-control
           :overlay overlay))))))

;;; Registry operations

(defun app-screens ()
  "Return the screen cache hash table for the current application."
  (application-screens *application*))

(defun load-and-register-screen (name-string)
  "Load a screen from disk and register it. Returns the screen-info."
  (let ((path (find-screen-file name-string)))
    (unless path
      (error "Screen file ~A.screen not found in directories: ~S"
             name-string (application-screen-directories *application*)))
    (let ((info (compile-screen-data name-string (load-screen-data path))))
      (setf (screen-info-file-timestamp info) (file-write-date path))
      (setf (gethash name-string (app-screens)) info)
      info)))

(defun screen-file-changed-p (name-string)
  "Check if the .screen file on disk is newer than the cached version."
  (let* ((info (gethash name-string (app-screens)))
         (path (find-screen-file name-string)))
    (when (and path info)
      (let ((disk-time (file-write-date path)))
        (and disk-time (> disk-time (screen-info-file-timestamp info)))))))

(defun generate-menu-screen-data (name-string menu-data)
  "Generate a screen data plist for a menu with no .screen file.
Creates a simple screen with repeat fields for menu items."
  (let* ((items (getf menu-data :items))
         (max-items (min (length items) 17)))
    (declare (ignore max-items))
    (let ((result (list :name name-string
                       :menu name-string
                       :screen (format nil "~%~%~%")
                       :fields (list (list :from '(2 3) :len 3 :name 'key
                                           :color 'green :intense t :repeat 17)
                                     (list :from '(2 8) :len 20 :name 'label
                                           :intense t :repeat 17)
                                     (list :from '(2 29) :len 50 :name 'description
                                           :repeat 17))
                       :navigable t
                       :roles (getf menu-data :roles)
                       :keys (multiple-value-bind (enter-label pf3-label)
                                 (menu-key-labels *application* name-string)
                               (list (list :enter enter-label)
                                     (list :pf3 pf3-label :back t))))))
      (when (getf menu-data :command)
        (setf (getf result :command) t))
      (when (getf menu-data :aliases)
        (setf (getf result :aliases) (getf menu-data :aliases)))
      result)))

(defun ensure-screen-loaded (screen-name)
  "Ensure SCREEN-NAME is loaded and up-to-date. Returns the screen-info.
For menu screens without a .screen file, generates one automatically.
Menu files are checked for changes on disk and reloaded if newer."
  (let ((name-string (screen-name-string screen-name)))
    ;; Check for menu file changes (clears cached screen if changed)
    (ensure-menu-loaded name-string)
    (when (screen-file-changed-p name-string)
      (load-and-register-screen name-string))
    (or (gethash name-string (app-screens))
        ;; Try loading from .screen file
        (let ((path (find-screen-file name-string)))
          (if path
              (load-and-register-screen name-string)
              ;; No .screen file: check if it's a menu and auto-generate
              (let ((menu-data (or (gethash name-string
                                            (application-menus *application*))
                                   ;; Discover new .menu files added after startup
                                   (let ((menu-path (find-menu-file name-string)))
                                     (when menu-path
                                       (let ((data (load-menu-file menu-path)))
                                         (setf (gethash name-string (application-menus *application*)) data
                                               (gethash name-string (application-menu-timestamps *application*))
                                               (cons menu-path (file-write-date menu-path)))
                                         data))))))
                (if menu-data
                    (let ((info (compile-screen-data
                                 name-string
                                 (generate-menu-screen-data name-string menu-data))))
                      (setf (gethash name-string (app-screens)) info)
                      info)
                    (error "Screen file ~A.screen not found in directories: ~S"
                           name-string
                           (application-screen-directories *application*)))))))))

(defun get-screen (screen-name)
  "Get a screen by name, loading from .screen file if not cached or changed on disk."
  (screen-info-screen (ensure-screen-loaded screen-name)))

(defun get-screen-rules (screen-name)
  "Get the declared rules for a screen. Returns a rules hash table or nil."
  (screen-info-rules (ensure-screen-loaded screen-name)))

(defun get-screen-keys (screen-name)
  "Get the declared key specs for a screen. Returns a list of key specs or nil."
  (screen-info-keys (ensure-screen-loaded screen-name)))

(defun get-screen-key-layout (screen-name)
  "Get the key layout for a screen. Returns an alist of (aid-keyword column width)."
  (screen-info-key-layout (ensure-screen-loaded screen-name)))

(defun get-screen-transient-fields (screen-name)
  "Get the list of transient field name strings for a screen."
  (screen-info-transient-fields (ensure-screen-loaded screen-name)))

(defun get-screen-repeat-fields (screen-name)
  "Get the repeat groups alist for a screen."
  (screen-info-repeat-groups (ensure-screen-loaded screen-name)))

(defun get-screen-dynamic-areas (screen-name)
  "Get the dynamic area list for a screen."
  (screen-info-dynamic-areas (ensure-screen-loaded screen-name)))

(defun get-screen-handler-package (screen-name)
  "Get the handler package for a screen, or nil for app default."
  (screen-info-handler-package (ensure-screen-loaded screen-name)))

(defun reload-screen (screen-name)
  "Force reload a screen from disk."
  (let ((name-string (screen-name-string screen-name)))
    (remhash name-string (app-screens))
    (load-and-register-screen name-string)))

(defun reload-all-screens ()
  "Clear the screen cache for the current application."
  (clrhash (app-screens))
  (clrhash (application-validated-screens *application*)))

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

;;; List data distribution

(defun truncate-to-field (string field-len)
  "Truncate STRING to at most FIELD-LEN characters."
  (if (> (length string) field-len)
      (subseq string 0 field-len)
      string))

(defun record-field-value (record key field-len)
  "Extract KEY from RECORD plist, convert to string, and truncate to FIELD-LEN."
  (let ((value (getf record key)))
    (if value
        (truncate-to-field (princ-to-string value) field-len)
        "")))

(defun distribute-group (group records data-count screen-name field-values)
  "Fill field slots for one repeat GROUP from RECORDS into FIELD-VALUES."
  (destructuring-bind (base-name count &optional stored-len) group
    (let ((key (intern (string-upcase base-name) :keyword))
          (field-len (or stored-len
                        (repeat-field-length (get-screen screen-name) base-name))))
      (dotimes (i count)
        (setf (gethash (format nil "~A.~D" base-name i) field-values)
              (if (< i data-count)
                  (record-field-value (nth i records) key field-len)
                  ""))))))

(defun set-page-info (field-values context offset page-size total)
  "Auto-populate the page-info field in FIELD-VALUES and CONTEXT."
  (let ((str (format nil "Page ~D of ~D"
                     (1+ (floor offset page-size))
                     (max 1 (ceiling total page-size)))))
    (setf (gethash "page-info" field-values) str
          (gethash "page-info" context) str)))

(defun distribute-list-data (screen-name repeat-groups context field-values)
  "Call get-list-data for SCREEN-NAME and distribute records into FIELD-VALUES.
Returns (values data-count total) if list data was provided, NIL otherwise."
  (let* ((page-size (reduce #'max repeat-groups :key #'second :initial-value 0))
         (offset (list-offset *session* screen-name)))
    (multiple-value-bind (records total)
        (get-list-data screen-name offset (+ offset page-size))
      (when total
        (let ((data-count (length records)))
          (dolist (group repeat-groups)
            (distribute-group group records data-count screen-name field-values))
          (set-page-info field-values context offset page-size total)
          ;; Store data count for selected-list-index
          (setf (list-state-value *session* screen-name :data-count)
                data-count)
          (values data-count total))))))

(defun list-data-row (screen repeat-groups)
  "Return the screen row where the first repeat field instance (.0) starts."
  (let ((target (format nil "~A.0" (first (first repeat-groups)))))
    (dolist (f (cl3270:screen-fields screen))
      (when (string-equal (cl3270:field-name f) target)
        (return (cl3270:field-row f))))))

(defun selected-list-index ()
  "Return the absolute index of the list row under the cursor, or NIL.
Call from a key handler on a screen with a list-data-getter."
  (let* ((screen-sym (session-current-screen *session*))
         (repeat-groups (get-screen-repeat-fields screen-sym))
         (screen (get-screen screen-sym)))
    (when repeat-groups
      (let* ((first-row (list-data-row screen repeat-groups))
             (page-size (reduce #'max repeat-groups :key #'second :initial-value 0))
             (row-index (- (cursor-row) first-row))
             (data-count (or (getf (list-state *session* screen-sym) :data-count) 0)))
        (when (and first-row
                   (<= 0 row-index (1- page-size))
                   (< row-index data-count))
          (+ (list-offset *session* screen-sym) row-index))))))

(defun repeat-field-index (field-name base-names)
  "Parse FIELD-NAME as \"base.N\". If base is in BASE-NAMES, return N; otherwise NIL."
  (let ((dot-pos (position #\. field-name :from-end t)))
    (when dot-pos
      (let ((base (subseq field-name 0 dot-pos))
            (idx-str (subseq field-name (1+ dot-pos))))
        (when (and (plusp (length idx-str))
                   (every #'digit-char-p idx-str)
                   (member base base-names :test #'string-equal))
          (parse-integer idx-str))))))

(defun filter-screen-fields (screen repeat-groups data-count)
  "Return a screen copy excluding repeat field instances beyond DATA-COUNT.
Fields belonging to a repeat group with index >= DATA-COUNT are removed."
  (let ((max-count (reduce #'max repeat-groups :key #'second :initial-value 0)))
    (if (>= data-count max-count)
        screen
        (let ((base-names (mapcar #'first repeat-groups)))
          (flet ((beyond-data-p (field)
                   (let ((idx (repeat-field-index (cl3270:field-name field) base-names)))
                     (and idx (>= idx data-count)))))
            (apply #'cl3270:make-screen
                   (cl3270:screen-name screen)
                   (remove-if #'beyond-data-p (cl3270:screen-fields screen))))))))

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
