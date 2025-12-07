;;; code-ref-test.el --- Tests for code-ref -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;;
;;; Commentary:
;; Unit tests for code-ref package.
;;
;;; Code:

(require 'ert)
(require 'code-ref-core)
(require 'code-ref)

;;; Mock xclip for testing
(defvar cref-test--clipboard-content nil
  "Content captured by mock xclip-set-selection.")

(defmacro cref-test-with-xclip-mock (&rest body)
  "Execute BODY with xclip-set-selection mocked."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'xclip-set-selection)
              (lambda (_type data)
                (setq cref-test--clipboard-content data))))
     ,@body))

(defmacro cref-test-without-xclip (&rest body)
  "Execute BODY with xclip-set-selection undefined."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'xclip-set-selection) nil))
     (fmakunbound 'xclip-set-selection)
     (unwind-protect
         (progn ,@body)
       (fset 'xclip-set-selection #'ignore))))

;;; Test Utilities

(defmacro cref-test-with-temp-buffer (content &rest body)
  "Execute BODY with a temp buffer containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (insert ,content)
     (goto-char (point-min))
     ,@body))

(defmacro cref-test-with-temp-file (content &rest body)
  "Execute BODY with a temp file containing CONTENT."
  (declare (indent 1))
  `(let ((temp-file (make-temp-file "code-ref-test-")))
     (unwind-protect
         (progn
           (with-temp-file temp-file
             (insert ,content))
           (with-current-buffer (find-file-noselect temp-file)
             (unwind-protect
                 (progn ,@body)
               (kill-buffer))))
       (delete-file temp-file))))

;;; Path Functions Tests

(ert-deftest cref-test-format-file-path-absolute ()
  "Test absolute path formatting."
  (cref-test-with-temp-file "test content"
    (let ((path (cref--format-file-path 'absolute)))
      (should (stringp path))
      (should (file-name-absolute-p path)))))

(ert-deftest cref-test-format-file-path-filename ()
  "Test filename-only formatting."
  (cref-test-with-temp-file "test content"
    (let ((path (cref--format-file-path 'filename)))
      (should (stringp path))
      (should-not (string-match-p "/" path)))))

(ert-deftest cref-test-format-file-path-no-file ()
  "Test error when buffer has no file."
  (cref-test-with-temp-buffer "test content"
    (should-error (cref--format-file-path 'absolute))))

(ert-deftest cref-test-format-file-path-invalid-style ()
  "Test error with invalid style."
  (cref-test-with-temp-file "test content"
    (should-error (cref--format-file-path 'invalid))))

(ert-deftest cref-test-get-buffer-display-path-no-file ()
  "Test display path for buffer without file."
  (cref-test-with-temp-buffer "test content"
    (let ((path (cref--get-buffer-display-path)))
      (should (stringp path))
      (should (string= path (buffer-name))))))

(ert-deftest cref-test-get-path-by-style-fallback ()
  "Test fallback to display path on error."
  (cref-test-with-temp-buffer "test content"
    (let ((path (cref--get-path-by-style 'absolute)))
      (should (stringp path))
      (should (string= path (buffer-name))))))

;;; Region Functions Tests

(ert-deftest cref-test-get-region-or-line-no-region ()
  "Test getting current line when no region."
  (cref-test-with-temp-buffer "line 1\nline 2\nline 3"
    (goto-char (point-min))
    (forward-line 1)
    (let ((bounds (cref--get-region-or-line)))
      (should (plist-get bounds :start))
      (should (plist-get bounds :end))
      (should-not (plist-get bounds :is-region)))))

(ert-deftest cref-test-get-region-or-line-with-region ()
  "Test getting region boundaries."
  (cref-test-with-temp-buffer "line 1\nline 2\nline 3"
    (transient-mark-mode 1)
    (goto-char (point-min))
    (push-mark (point) t t)  ; Activate mark
    (forward-line 2)
    (let ((bounds (cref--get-region-or-line)))
      (should (plist-get bounds :start))
      (should (plist-get bounds :end))
      (should (plist-get bounds :is-region)))))

(ert-deftest cref-test-get-region-or-line-bolp-adjustment ()
  "Test region end adjustment when at beginning of line."
  (cref-test-with-temp-buffer "line 1\nline 2\nline 3\n"
    (transient-mark-mode 1)
    (goto-char (point-min))
    (push-mark (point) t t)  ; Activate mark
    (forward-line 2)
    (should (bolp))
    (let* ((bounds (cref--get-region-or-line))
           (end (plist-get bounds :end)))
      (save-excursion
        (goto-char end)
        (should-not (bolp))))))

;;; Markdown Formatting Tests

(ert-deftest cref-test-find-max-backtick-sequence-none ()
  "Test with no backticks."
  (should (= (cref--find-max-backtick-sequence "hello world") 0)))

(ert-deftest cref-test-find-max-backtick-sequence-single ()
  "Test with single backtick."
  (should (= (cref--find-max-backtick-sequence "hello `code` world") 1)))

(ert-deftest cref-test-find-max-backtick-sequence-triple ()
  "Test with triple backticks."
  (should (= (cref--find-max-backtick-sequence "```code```") 3)))

(ert-deftest cref-test-find-max-backtick-sequence-mixed ()
  "Test with mixed backticks."
  (should (= (cref--find-max-backtick-sequence "`single` and ```triple```") 3)))

(ert-deftest cref-test-make-code-fence-default ()
  "Test default fence length."
  (let ((fence (cref--make-code-fence "no backticks")))
    (should (string= fence "```"))))

(ert-deftest cref-test-make-code-fence-adjusted ()
  "Test fence length adjustment for content with backticks."
  (let ((fence (cref--make-code-fence "```code```")))
    (should (string= fence "````"))))

(ert-deftest cref-test-make-code-fence-long ()
  "Test fence length for content with many backticks."
  (let ((fence (cref--make-code-fence "`````very long`````")))
    (should (= (length fence) 6))))

;;; Location String Tests

(ert-deftest cref-test-get-region-location-string-single-line ()
  "Test location string for single line."
  (cref-test-with-temp-buffer "line 1\nline 2\nline 3"
    (goto-char (point-min))
    (forward-line 1)
    (let* ((bounds (list :start (line-beginning-position)
                         :end (line-end-position)))
           (location (cref--get-region-location-string "test.el" bounds)))
      (should (string-match-p "@test\\.el#L2$" location)))))

(ert-deftest cref-test-get-region-location-string-multi-line ()
  "Test location string for multiple lines."
  (cref-test-with-temp-buffer "line 1\nline 2\nline 3\nline 4"
    (goto-char (point-min))
    (let* ((start (line-beginning-position))
           (_ (forward-line 2))
           (end (line-end-position))
           (bounds (list :start start :end end))
           (location (cref--get-region-location-string "test.el" bounds)))
      (should (string-match-p "@test\\.el#L1-L3$" location)))))

(ert-deftest cref-test-get-region-location-string-custom-prefix ()
  "Test location string with custom prefix."
  (let ((cref-location-prefix "file:"))
    (cref-test-with-temp-buffer "line 1"
      (let* ((bounds (list :start (point-min) :end (point-max)))
             (location (cref--get-region-location-string "test.el" bounds)))
        (should (string-prefix-p "file:" location))))))

;;; Content With Fence Tests

(ert-deftest cref-test-get-region-content-with-fence ()
  "Test content wrapped with fence."
  (cref-test-with-temp-buffer "hello world"
    (let* ((bounds (list :start (point-min) :end (point-max)))
           (content (cref--get-region-content-with-fence bounds)))
      (should (string-prefix-p "```" content))
      (should (string-suffix-p "```" content))
      (should (string-match-p "hello world" content)))))

(ert-deftest cref-test-get-region-content-with-fence-backticks ()
  "Test content with backticks wrapped correctly."
  (cref-test-with-temp-buffer "```code```"
    (let* ((bounds (list :start (point-min) :end (point-max)))
           (content (cref--get-region-content-with-fence bounds)))
      (should (string-prefix-p "````" content))
      (should (string-suffix-p "````" content)))))

;;; Clipboard Tests

(ert-deftest cref-test-copy-to-clipboard-with-xclip ()
  "Test copying to clipboard when xclip is available."
  (let* ((test-text "test clipboard content")
         (xclip-called nil)
         (xclip-args nil)
         (kill-ring (list "sentinel")))
    (cl-letf (((symbol-function 'xclip-set-selection)
               (lambda (type text)
                 (setq xclip-called t)
                 (setq xclip-args (list type text)))))
      (let ((result (cref--copy-to-clipboard test-text)))
        (should result)
        (should xclip-called)
        (should (eq (car xclip-args) 'clipboard))
        (should (string= (cadr xclip-args) test-text))
        (should (equal kill-ring '("sentinel")))))))

(ert-deftest cref-test-copy-to-clipboard-without-xclip ()
  "Test copying to kill-ring when xclip is not available."
  (let ((test-text "test kill-ring content")
        (kill-ring nil))
    (cref-test-without-xclip
      (let ((result (cref--copy-to-clipboard test-text)))
        (should-not result)
        (should (string= (car kill-ring) test-text))))))

;;; Integration Tests

(ert-deftest cref-test-copy-buffer-path ()
  "Test copying buffer path."
  (cref-test-with-temp-file "test content"
    (setq cref-test--clipboard-content nil)
    (cref-test-with-xclip-mock
      (cref-copy-buffer-path)
      (should (stringp cref-test--clipboard-content)))))

(ert-deftest cref-test-copy-region-location ()
  "Test copying region location."
  (cref-test-with-temp-file "line 1\nline 2\nline 3"
    (setq cref-test--clipboard-content nil)
    (goto-char (point-min))
    (forward-line 1)
    (cref-test-with-xclip-mock
      (cref-copy-region-location)
      (should (string-match-p "#L2$" cref-test--clipboard-content)))))

(ert-deftest cref-test-copy-region-with-location ()
  "Test copying region with content."
  (cref-test-with-temp-file "line 1\nline 2\nline 3"
    (setq cref-test--clipboard-content nil)
    (goto-char (point-min))
    (forward-line 1)
    (cref-test-with-xclip-mock
      (cref-copy-region-with-location)
      (let ((result cref-test--clipboard-content))
        (should (string-match-p "#L2" result))
        (should (string-match-p "```" result))
        (should (string-match-p "line 2" result))))))

;;; Test Runner

(defun cref-test-run-all ()
  "Run all code-ref test cases."
  (interactive)
  (ert "cref-test-.*"))

(provide 'code-ref-test)
;;; code-ref-test.el ends here
