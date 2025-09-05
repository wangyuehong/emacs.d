;;; test-flymake-golangci-lint-clean.el --- Tests for flymake-golangci-lint -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Wang Yuehong

;;; Commentary:
;; Comprehensive unit tests for flymake-golangci-lint.

;;; Code:

(require 'ert)
(require 'flymake)
(require 'flymake-golangci-lint)
(require 'cl-lib)

;; Mock go-mode for testing
(defun go-mode ()
  "Mock `go-mode'."
  (setq major-mode 'go-mode))

(defmacro with-temp-go-buffer (content &rest body)
  "Execute BODY with a temp buffer containing Go CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (setq buffer-file-name "/tmp/test.go")
     (go-mode)
     ,@body))

;;; Basic Functionality Tests

(ert-deftest test-command-building ()
  "Test basic command building works."
  (let ((command (flymake-golangci-lint-command "/tmp")))
    (should (listp command))
    (should (string= (car command) "golangci-lint"))
    (should (member "run" command))
    (should (member "/tmp" command))))

(ert-deftest test-configuration-options ()
  "Test configuration options work."
  (let ((flymake-golangci-lint-options
         '(:fast t 
           :timeout "30s" 
           :disable-linters ("typecheck"))))
    (let ((command (flymake-golangci-lint-command "/tmp")))
      (should (member "--fast-only" command))
      (should (member "--timeout" command))
      (should (member "30s" command))
      (should (member "-D" command))
      (should (member "typecheck" command)))))

(ert-deftest test-legacy-config-compatibility ()
  "Test legacy configuration still works."
  (let ((flymake-golangci-lint-fast t)
        (flymake-golangci-lint-timeout "30s")
        (flymake-golangci-lint-options '()))
    (should (eq (flymake-golangci-lint--get-option :fast) t))
    (should (string= (flymake-golangci-lint--get-option :timeout) "30s"))))

(ert-deftest test-output-parsing ()
  "Test golangci-lint output parsing."
  (with-temp-go-buffer "package main\n\nfunc main() {\n\tvar unused int\n}\n"
    (let ((output "/tmp/test.go:4:6: declared and not used: unused"))
      (let ((diagnostics (flymake-golangci-lint--make-diagnostics (current-buffer) output)))
        (should (= (length diagnostics) 1))
        (should (car diagnostics))))))

(ert-deftest test-file-path-matching ()
  "Test file path matching logic."
  (with-temp-go-buffer "package main\n\nfunc main() {}\n"
    (let ((buffer-file (file-truename buffer-file-name)))
      (should (flymake-golangci-lint--file-matches-p "/tmp/test.go" buffer-file))
      (should-not (flymake-golangci-lint--file-matches-p "/other/file.go" buffer-file)))))

;;; Error Handling Tests

(ert-deftest test-environment-validation ()
  "Test environment validation."
  (should (flymake-golangci-lint--validate-environment nil))
  (should-not (flymake-golangci-lint--validate-environment "/tmp/test.go")))

(ert-deftest test-backend-error-handling ()
  "Test backend handles missing file correctly."
  (with-temp-buffer
    (let ((report-called nil)
          (report-type nil))
      (flymake-golangci-lint-backend 
       (lambda (_diagnostics &optional type &rest _args)
         (setq report-called t report-type type)))
      (should report-called)
      (should (eq report-type :panic)))))

;;; Integration Tests

(ert-deftest test-hook-management ()
  "Test hook management prevents duplicates."
  (with-temp-go-buffer "package main\n\nfunc main() {}\n"
    (let ((initial-count (length flymake-diagnostic-functions)))
      (flymake-golangci-lint-load)
      (let ((after-first (length flymake-diagnostic-functions)))
        (flymake-golangci-lint-load) ; Add again
        (let ((after-second (length flymake-diagnostic-functions)))
          (should (> after-first initial-count))
          (should (= after-first after-second))
          (should (= (cl-count #'flymake-golangci-lint-backend 
                              flymake-diagnostic-functions) 1)))))))

(ert-deftest test-manual-run-missing-executable ()
  "Test manual run error when executable missing."
  (with-temp-go-buffer "package main\n\nfunc main() {}\n"
    (let ((flymake-golangci-lint-executable "nonexistent-golangci-lint"))
      (should-error (flymake-golangci-lint-run) :type 'user-error))))

;;; Performance and Quality Tests

(ert-deftest test-parse-line-performance ()
  "Test single line parsing is efficient."
  (let ((buffer-file "/tmp/test.go"))
    (should (flymake-golangci-lint--parse-line 
             "/tmp/test.go:10:5: some error message" buffer-file))
    (should-not (flymake-golangci-lint--parse-line 
                 "/other/file.go:10:5: other error" buffer-file))
    (should-not (flymake-golangci-lint--parse-line 
                 "invalid line format" buffer-file))))

(ert-deftest test-diagnostics-creation ()
  "Test diagnostic objects are created correctly."
  (with-temp-go-buffer "package main\n\nfunc main() {\n\tvar unused int\n}\n"
    (let ((issue '(:line 4 :message "declared and not used: unused")))
      (let ((diag (flymake-golangci-lint--make-single-diagnostic (current-buffer) issue)))
        (should diag)))))

;;; Test Runner

(defun test-flymake-golangci-lint-run-all ()
  "Run all flymake-golangci-lint test cases."
  (interactive)
  (ert "test-.*flymake-golangci-lint.*"))

(provide 'test-flymake-golangci-lint-clean)
;;; test-flymake-golangci-lint-clean.el ends here