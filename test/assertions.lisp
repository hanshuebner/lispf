;;;; -*- Mode: Lisp; Coding: utf-8 -*-

;;;; assertions.lisp
;;;;
;;;; Assertion functions and minimal test runner for LISPF tests.

(in-package #:lispf-test)

;;; Test failure condition

(define-condition test-failure (error)
  ((description :initarg :description :reader test-failure-description :initform nil)
   (expected :initarg :expected :reader test-failure-expected :initform nil)
   (actual :initarg :actual :reader test-failure-actual :initform nil))
  (:report (lambda (c stream)
             (format stream "Test failure~@[: ~A~]~@[~%  Expected: ~S~]~@[~%  Actual:   ~S~]"
                     (test-failure-description c)
                     (test-failure-expected c)
                     (test-failure-actual c)))))

(defun fail (description expected actual)
  "Signal a test-failure."
  (error 'test-failure :description description :expected expected :actual actual))

;;; Basic assertions

(defun assert-screen-contains (session text &key description)
  "Assert that the full screen text contains TEXT somewhere."
  (let* ((rows (screen-text session))
         (full (format nil "~{~A~^~%~}" rows)))
    (unless (search text full)
      (fail (or description (format nil "Screen should contain ~S" text))
            text full))))

(defun assert-screen-match (session row expected &key trim description)
  "Assert that screen ROW matches EXPECTED. If TRIM, right-trim both."
  (let* ((actual (screen-row session row))
         (cmp-actual (if trim (string-right-trim '(#\Space) actual) actual))
         (cmp-expected (if trim (string-right-trim '(#\Space) expected) expected)))
    (unless (string= cmp-actual cmp-expected)
      (fail (or description (format nil "Row ~D mismatch" row))
            cmp-expected cmp-actual))))

(defun assert-text-at (session row col length expected &key trim description)
  "Assert that the text at ROW,COL for LENGTH matches EXPECTED."
  (let* ((actual (screen-text-at session row col length))
         (cmp-actual (if trim (string-right-trim '(#\Space) actual) actual))
         (cmp-expected (if trim (string-right-trim '(#\Space) expected) expected)))
    (unless (string= cmp-actual cmp-expected)
      (fail (or description (format nil "Text at (~D,~D) len ~D mismatch" row col length))
            cmp-expected cmp-actual))))

(defun assert-cursor-at (session row col &key description)
  "Assert that the cursor is at ROW,COL."
  (multiple-value-bind (actual-row actual-col) (cursor-position session)
    (unless (and (= actual-row row) (= actual-col col))
      (fail (or description "Cursor position mismatch")
            (format nil "(~D, ~D)" row col)
            (format nil "(~D, ~D)" actual-row actual-col)))))

;;; Screen-data-aware operations

(defun load-test-screen-data (screen-path)
  "Load a .screen file and return its plist data for use in field-aware assertions."
  (lispf:load-screen-data screen-path))

(defun field-position (screen-data field-name)
  "Look up a field's content position from screen data.
Returns (values row col length) where row has +1 offset for the framework title
row and col has +1 offset to skip the attribute byte.
FIELD-NAME is a string (case-insensitive)."
  (let ((fields (getf screen-data :fields)))
    (dolist (field fields)
      (let ((name (getf field :name)))
        (when (and name (string-equal (string name) field-name))
          (let ((from (getf field :from))
                (len (getf field :len)))
            (return-from field-position
              (values (1+ (first from))   ; +1 for framework title row
                      (1+ (second from))  ; +1 to skip attribute byte
                      len)))))))
  (error "Field ~S not found in screen data" field-name))

(defun read-field (session screen-data field-name)
  "Read a field's current value from the screen by name."
  (multiple-value-bind (row col len) (field-position screen-data field-name)
    (screen-text-at session row col len)))

(defun assert-field (session screen-data field-name expected &key trim description)
  "Assert that a named field's value matches EXPECTED."
  (let* ((actual (read-field session screen-data field-name))
         (cmp-actual (if trim (string-right-trim '(#\Space) actual) actual))
         (cmp-expected (if trim (string-right-trim '(#\Space) expected) expected)))
    (unless (string= cmp-actual cmp-expected)
      (fail (or description (format nil "Field ~A mismatch" field-name))
            cmp-expected cmp-actual))))

(defun type-in-field (session screen-data field-name text)
  "Move to a named field's position and type TEXT."
  (multiple-value-bind (row col len) (field-position screen-data field-name)
    (declare (ignore len))
    (type-at session row col text)))

(defun assert-on-screen (session screen-name)
  "Assert that the current screen is SCREEN-NAME by checking the title row.
Matches the format-title-line convention: title starts with uppercase screen name.
Accounts for the 3270 field attribute byte at column 0 by checking from column 1."
  (let* ((title-row (screen-row session 0))
         (trimmed (string-left-trim '(#\Space) title-row))
         (expected-prefix (string-upcase screen-name)))
    (unless (and (>= (length trimmed) (length expected-prefix))
                 (string= expected-prefix
                          (subseq trimmed 0 (length expected-prefix))))
      (fail (format nil "Expected to be on screen ~A" screen-name)
            expected-prefix
            (string-right-trim '(#\Space) title-row)))))

;;; Test runner

(defvar *test-registry* (make-hash-table :test 'eq)
  "Per-package test registry: package -> (tests-hash . test-order-list).")

(defvar *suite-order* nil
  "Packages in the order their first test was registered.")

(defvar *suite-fixtures* (make-hash-table :test 'eq)
  "Per-package fixture declarations: package -> list of fixture plists.
Each plist has keys :name, :scope (:suite or :test), :setup, :teardown.")

(defun package-tests (&optional (pkg *package*))
  "Return (tests-hash . test-order) for PKG, creating if needed."
  (or (gethash pkg *test-registry*)
      (progn
        (pushnew pkg *suite-order*)
        (setf (gethash pkg *test-registry*)
              (cons (make-hash-table :test 'equal) nil)))))

(defmacro define-test (name () &body body)
  "Define a named test, registered in the current package's test suite.
Tests are automatically available to run-tests."
  `(let ((entry (package-tests)))
     (setf (gethash ',name (car entry))
           (lambda ()
             ,@body))
     (pushnew ',name (cdr entry))
     ',name))

(defmacro define-suite-fixtures (&body fixture-specs)
  "Declare fixtures for the current package's test suite.
Each FIXTURE-SPEC is a plist with keys :name, :scope, :setup, and optionally :teardown.
SCOPE is :suite (run once before/after all tests) or :test (run around each test).
SETUP and TEARDOWN are forms to evaluate."
  (let ((specs (loop for spec in fixture-specs
                     collect `(list :name ,(getf spec :name)
                                    :scope ,(getf spec :scope)
                                    :setup (lambda () ,(getf spec :setup))
                                    :teardown ,(if (getf spec :teardown)
                                                   `(lambda () ,(getf spec :teardown))
                                                   '(lambda ()))))))
    `(setf (gethash *package* *suite-fixtures*)
           (list ,@specs))))

(defun run-single-test (name fn test-fixtures)
  "Run a single test FN, wrapping it with TEST-FIXTURES setup/teardown.
Returns :pass, :fail, or :error, and the condition on failure."
  (labels ((wrap (fixtures)
             (if (null fixtures)
                 (funcall fn)
                 (let ((f (first fixtures))
                       (setup-done nil))
                   (unwind-protect
                        (progn
                          (funcall (getf f :setup))
                          (setf setup-done t)
                          (wrap (rest fixtures)))
                     (when setup-done
                       (ignore-errors (funcall (getf f :teardown)))))))))
    (handler-case
        (progn
          (wrap test-fixtures)
          (format t "~&  PASS ~A~%" name)
          (values :pass nil))
      (test-failure (c)
        (format t "~&  FAIL ~A: ~A~%" name c)
        (values :fail c))
      (error (c)
        (format t "~&  ERROR ~A: ~A~%" name c)
        (values :error c)))))

(defun resolve-test-name (name pkg)
  "Resolve NAME to a symbol in PKG's test registry.
NAME can be a symbol from any package; lookup is by symbol-name string."
  (let ((entry (gethash pkg *test-registry*)))
    (when entry
      (find-symbol (symbol-name name) pkg))))

(defun find-test-package (name)
  "Find the unique package containing a test named NAME.
Returns the package, or NIL if not found. Signals an error if ambiguous."
  (let ((matches nil))
    (maphash (lambda (pkg entry)
               (let ((sym (find-symbol (symbol-name name) pkg)))
                 (when (and sym (gethash sym (car entry)))
                   (push pkg matches))))
             *test-registry*)
    (cond ((null matches) nil)
          ((null (cdr matches)) (first matches))
          (t (error "Ambiguous test name ~A — found in packages: ~{~A~^, ~}"
                    name (mapcar #'package-name matches))))))

(defun run-tests (&rest args)
  "Run the named tests, or all tests in definition order.
When :PACKAGE is given (a package designator), run tests from that package
instead of the calling package.  When :PACKAGE is omitted and specific test
names are given, the package is discovered automatically from the test
registry.  Example: (run-tests 'my-test) or (run-tests :package :my-tests)
Automatically sets up suite fixtures declared via define-suite-fixtures."
  (let* ((package-pos (position :package args))
         (names (if package-pos
                    (append (subseq args 0 package-pos)
                            (subseq args (+ package-pos 2)))
                    args))
         (pkg (cond (package-pos
                     (find-package (nth (1+ package-pos) args)))
                    (names
                     (or (find-test-package (first names))
                         *package*))
                    (t *package*)))
         (entry (package-tests pkg))
         (tests (car entry))
         (test-names (or (mapcar (lambda (name) (or (resolve-test-name name pkg) name))
                                 names)
                         (reverse (cdr entry))))
         (all-fixtures (gethash pkg *suite-fixtures*))
         (suite-fixtures (remove :test all-fixtures :key (lambda (f) (getf f :scope))))
         (test-fixtures (remove :suite all-fixtures :key (lambda (f) (getf f :scope))))
         (pass 0)
         (fail-count 0)
         (errors '())
         (setup-fixtures nil))
    (unwind-protect
         (progn
           (dolist (f suite-fixtures)
             (funcall (getf f :setup))
             (push f setup-fixtures))
           (dolist (name test-names)
             (let ((fn (gethash name tests)))
               (if (not fn)
                   (progn
                     (format t "~&  SKIP ~A (not defined)~%" name)
                     (incf fail-count)
                     (push (cons name "not defined") errors))
                   (multiple-value-bind (result condition)
                       (run-single-test name fn test-fixtures)
                     (case result
                       (:pass (incf pass))
                       (t (incf fail-count)
                          (push (cons name condition) errors))))))))
      (dolist (f setup-fixtures)
        (ignore-errors (funcall (getf f :teardown)))))
    (format t "~&~%Results: ~D passed, ~D failed out of ~D~%"
            pass fail-count (+ pass fail-count))
    (values (zerop fail-count) pass fail-count errors)))

(defun run-all-suites ()
  "Run all registered test suites.  Returns T if all passed.
Calls run-tests for each suite, which automatically sets up any declared
fixtures.  Always binds *package* to the suite's package so that
run-tests can find the test registry."
  (let ((all-passed t))
    (dolist (pkg (reverse *suite-order*))
      (format t "~&~%=== ~A ===~%" (package-name pkg))
      (let ((*package* pkg))
        (unless (run-tests :package pkg)
          (setf all-passed nil))))
    all-passed))
