#!/usr/bin/env emacs --script
;;; run-tests.el --- Test runner script for flymake-golangci-lint -*- lexical-binding: t; -*-

;; This script can be run from command line:
;; emacs --batch -l run-tests.el

;;; Code:

(add-to-list 'load-path (file-name-directory (or load-file-name buffer-file-name)))
(add-to-list 'load-path default-directory)

;; Load dependencies
(require 'ert)

;; Mock go-mode if not available
(unless (fboundp 'go-mode)
  (defun go-mode ()
    "Mock go-mode for testing."
    (setq major-mode 'go-mode)))

;; Load the module and tests
(load "flymake-golangci-lint.el")
(load "test-flymake-golangci-lint-clean.el")

;; Run all tests
(princ "Running flymake-golangci-lint test suite...\n")
(princ "==========================================\n\n")

(let ((test-results (ert-run-tests-batch "test-")))
  (princ "\n\nTest Summary:\n")
  (princ "=============\n")
  
  ;; Display summary
  (princ "Test suite completed.\n")
  
  (if (zerop (ert-stats-completed-unexpected test-results))
      (progn
        (princ "\nALL TESTS PASSED!\n")
        (princ "All functionality verified.\n"))
    (progn
      (princ "\nSOME TESTS FAILED!\n")
      (princ "Please check the output above for details.\n"))))

;;; run-tests.el ends here