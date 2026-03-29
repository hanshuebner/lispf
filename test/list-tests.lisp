;;; -*- Mode: Lisp -*-

;;; E2E tests for list screen pagination.
;;; Verifies that partial pages correctly clear unused rows,
;;; including write (selection) fields.

(defpackage #:lispf-list-tests
  (:use #:cl #:lispf-test)
  (:export #:run-all))

(in-package #:lispf-list-tests)

;;; Minimal test application with a list screen.
;;; The list has 7 items, page size 5, so page 2 has only 2 items.

(lispf:define-application *list-test-app*
  :title "LIST-TEST"
  :entry-screen list-test
  :screen-directory (merge-pathnames
                     #P"test/screens/"
                     (asdf:system-source-directory :lispf))
  :session-class 'lispf:session)

(defvar *test-items*
  '((:item-name "Alpha")
    (:item-name "Bravo")
    (:item-name "Charlie")
    (:item-name "Delta")
    (:item-name "Echo")
    (:item-name "Foxtrot")
    (:item-name "Golf")))

(lispf:define-list-data-getter list-test (start end)
  (let* ((total (length *test-items*))
         (page (subseq *test-items* start (min end total))))
    (values page total)))

(lispf:define-key-handler list-test :pf3 ()
  :logoff)

;;; Tests

(define-test e2e-list-partial-page-cleared ()
  "Verify that unused rows on a partial last page are blank."
  (with-test-app (s *list-test-app*)
    (assert-on-screen s "LIST-TEST")
    ;; Page 1: 5 items (Alpha through Echo)
    (assert-screen-contains s "Alpha")
    (assert-screen-contains s "Echo")
    ;; Page forward to page 2 (2 items: Foxtrot, Golf)
    (press-key s :pf8)
    (assert-screen-contains s "Foxtrot")
    (assert-screen-contains s "Golf")
    ;; Row 5 (3rd list slot, physical row 5) should be blank — no stale data
    ;; Physical rows: 0=title, 1=blank, 2=header, 3=Foxtrot, 4=Golf, 5-7=empty
    (let ((row5 (string-trim '(#\Space) (screen-row s 5))))
      (assert (string= "" row5) ()
              "Row 5 should be blank on partial page, got: ~S" row5))
    ;; Row 3 (1st data row) should NOT have stale "Charlie" from page 1
    (let ((row3-text (screen-text-at s 3 5 30)))
      (assert (search "Foxtrot" row3-text) ()
              "Row 3 should show Foxtrot, got: ~S" row3-text))
    ;; Empty rows must not have any field attributes (no visible markers)
    (assert (not (row-has-field-attribute-p s 5)) ()
            "Row 5 should have no field attributes on partial page")
    (assert (not (row-has-field-attribute-p s 6)) ()
            "Row 6 should have no field attributes on partial page")
    (assert (not (row-has-field-attribute-p s 7)) ()
            "Row 7 should have no field attributes on partial page")
    ;; Exit
    (press-key s :pf3)))

(defun run-all ()
  (run-tests :lispf-list-tests))
