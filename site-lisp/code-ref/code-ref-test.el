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
(require 'dired)
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
  "Execute BODY with `xclip-set-selection' appearing unbound.
`cl-letf' on the `symbol-function' place saves the original binding
\(including the unbound state), sets the function slot to nil for the
duration of BODY so that `fboundp' returns nil, and restores the
original binding on exit."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'xclip-set-selection) nil))
     ,@body))

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

(defmacro cref-test-with-dired (filename &rest body)
  "Execute BODY inside a dired buffer containing a file named FILENAME.
Inside BODY, `temp-dir' is bound to the dired directory and `temp-file'
to the absolute path of the sample file."
  (declare (indent 1))
  `(let* ((temp-dir (file-name-as-directory
                     (make-temp-file "code-ref-dired-" t)))
          (temp-file (expand-file-name ,filename temp-dir)))
     (unwind-protect
         (progn
           (with-temp-file temp-file (insert "sample\n"))
           (with-current-buffer (dired-noselect temp-dir)
             (unwind-protect
                 (progn ,@body)
               (kill-buffer))))
       (delete-directory temp-dir t))))

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
  "Test `user-error' when buffer has no file and is not dired."
  (cref-test-with-temp-buffer "test content"
    (should-error (cref--format-file-path 'absolute) :type 'user-error)))

(ert-deftest cref-test-format-file-path-invalid-style ()
  "Test error with invalid style."
  (cref-test-with-temp-file "test content"
    (should-error (cref--format-file-path 'invalid))))

(ert-deftest cref-test-get-buffer-display-path-no-file ()
  "Display path signals `user-error' when the buffer has no source file."
  (cref-test-with-temp-buffer "test content"
    (should-error (cref--get-buffer-display-path) :type 'user-error)))

(ert-deftest cref-test-get-path-by-style-no-file ()
  "Path resolution signals `user-error' when the buffer has no source file."
  (cref-test-with-temp-buffer "test content"
    (should-error (cref--get-path-by-style 'absolute) :type 'user-error)
    (should-error (cref--get-path-by-style 'display) :type 'user-error)
    (should-error (cref--get-path-by-style 'filename) :type 'user-error)))

(ert-deftest cref-test-copy-buffer-commands-no-file-are-silent ()
  "AC-0010-0060: on non-file non-dired buffers, every copy-buffer-* command
errors with `user-error' and leaves both clipboard and kill-ring untouched."
  (cref-test-with-temp-buffer "test content"
    (dolist (cmd '(cref-copy-buffer-path
                   cref-copy-buffer-absolute-path
                   cref-copy-buffer-file-name
                   cref-copy-buffer-git-path))
      (ert-info ((format "command=%s" cmd))
        (let ((cref-test--clipboard-content 'sentinel)
              (kill-ring (list "kr-sentinel")))
          (cref-test-with-xclip-mock
            (should-error (funcall cmd) :type 'user-error))
          (should (eq cref-test--clipboard-content 'sentinel))
          (should (equal kill-ring '("kr-sentinel"))))))))

;;; Project Path Tests

(defmacro cref-test-with-mock-project (root &rest body)
  "Execute BODY with `project-current' and `project-root' mocked for ROOT."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'project-current)
              (lambda (&optional _maybe-prompt _dir)
                (list 'mock-project ,root)))
             ((symbol-function 'project-root)
              (lambda (proj) (cadr proj))))
     ,@body))

(ert-deftest cref-test-get-project-root-with-project ()
  "Test getting project root when in a project."
  (cref-test-with-mock-project "/tmp/myproject/"
    (let ((root (cref--get-project-root)))
      (should (stringp root))
      (should (string= root "/tmp/myproject/")))))

(ert-deftest cref-test-get-project-root-no-project ()
  "Test getting project root when not in a project."
  (cl-letf (((symbol-function 'project-current)
             (lambda (&optional _maybe-prompt _dir) nil)))
    (should-not (cref--get-project-root))))

(ert-deftest cref-test-format-file-path-project ()
  "Test project-relative path formatting."
  (cref-test-with-temp-file "test content"
    (let* ((file-dir (file-name-directory buffer-file-name))
           (project-root (expand-file-name (file-name-directory file-dir))))
      (cref-test-with-mock-project project-root
        (let ((path (cref--format-file-path 'project)))
          (should (stringp path))
          (should-not (file-name-absolute-p path)))))))

(ert-deftest cref-test-format-file-path-project-no-project ()
  "Test error when not in a project."
  (cref-test-with-temp-file "test content"
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt _dir) nil)))
      (should-error (cref--format-file-path 'project)))))

(ert-deftest cref-test-get-path-by-style-project-error ()
  "Project style signals `user-error' when not in a project."
  (cref-test-with-temp-file "test content"
    (cl-letf (((symbol-function 'project-current)
               (lambda (&optional _maybe-prompt _dir) nil)))
      (should-error (cref--get-path-by-style 'project) :type 'user-error))))

(ert-deftest cref-test-format-file-path-git-no-repo ()
  "Git style signals `user-error' when the source file is not in a repo."
  (cref-test-with-temp-file "test content"
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (&rest _) nil)))
      (should-not (cref--get-git-root))
      (should-error (cref--format-file-path 'git) :type 'user-error))))

(ert-deftest cref-test-copy-buffer-git-path-no-repo-is-silent ()
  "AC-0010-0070: git style outside a repo neither writes clipboard nor kill-ring."
  (cref-test-with-temp-file "test content"
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (&rest _) nil)))
      (let ((cref-test--clipboard-content 'sentinel)
            (kill-ring (list "kr-sentinel")))
        (cref-test-with-xclip-mock
          (should-error (cref-copy-buffer-git-path) :type 'user-error))
        (should (eq cref-test--clipboard-content 'sentinel))
        (should (equal kill-ring '("kr-sentinel")))))))

(ert-deftest cref-test-get-path-by-style-display-no-repo ()
  "Display style outside a git repo returns the absolute source path."
  (cref-test-with-temp-file "test content"
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (&rest _) nil)))
      (let ((path (cref--get-path-by-style 'display)))
        (should (stringp path))
        (should (file-name-absolute-p path))
        (should (string= path (file-truename buffer-file-name)))))))

(ert-deftest cref-test-copy-buffer-path-no-repo-copies-absolute ()
  "AC-0010-0080: display style outside a repo puts absolute path on clipboard."
  (cref-test-with-temp-file "test content"
    (cl-letf (((symbol-function 'locate-dominating-file)
               (lambda (&rest _) nil)))
      (let ((cref-test--clipboard-content nil)
            (expected (file-truename buffer-file-name)))
        (cref-test-with-xclip-mock
          (cref-copy-buffer-path))
        (should (string= cref-test--clipboard-content expected))
        (should-not (string= cref-test--clipboard-content (buffer-name)))))))

;;; Dired Tests

(ert-deftest cref-test-current-source-file-dired-on-entry ()
  "In dired, point on a file entry resolves to that file."
  (cref-test-with-dired "sample.txt"
    (goto-char (point-min))
    (dired-goto-file temp-file)
    (let ((source (cref--current-source-file)))
      (should (stringp source))
      (should (string= source (file-truename temp-file))))))

(ert-deftest cref-test-current-source-file-dired-off-entry ()
  "In dired, point off any entry resolves to the dired directory itself."
  (cref-test-with-dired "sample.txt"
    (goto-char (point-min))
    (let ((source (cref--current-source-file))
          (expected (file-truename
                     (directory-file-name temp-dir))))
      (should (stringp source))
      (should (string= source expected)))))

(ert-deftest cref-test-format-file-path-dired-absolute ()
  "Absolute style works inside dired."
  (cref-test-with-dired "sample.txt"
    (dired-goto-file temp-file)
    (let ((path (cref--format-file-path 'absolute)))
      (should (string= path (file-truename temp-file))))))

(ert-deftest cref-test-format-file-path-dired-filename ()
  "Filename style returns just the entry's file name inside dired."
  (cref-test-with-dired "sample.txt"
    (dired-goto-file temp-file)
    (let ((path (cref--format-file-path 'filename)))
      (should (string= path "sample.txt")))))

(ert-deftest cref-test-get-path-by-style-dired-display ()
  "Display style inside dired returns the entry path, not the buffer name."
  (cref-test-with-dired "sample.txt"
    (dired-goto-file temp-file)
    (let ((path (cref--get-path-by-style 'display)))
      (should (stringp path))
      (should-not (string= path (buffer-name)))
      (should (string-match-p "sample\\.txt\\'" path)))))

(ert-deftest cref-test-copy-buffer-absolute-path-dired ()
  "End-to-end: `cref-copy-buffer-absolute-path' in dired copies the entry path."
  (cref-test-with-dired "sample.txt"
    (dired-goto-file temp-file)
    (setq cref-test--clipboard-content nil)
    (cref-test-with-xclip-mock
      (cref-copy-buffer-absolute-path)
      (should (string= cref-test--clipboard-content
                       (file-truename temp-file))))))

(defconst cref-test--region-commands
  '(cref-copy-region-location
    cref-copy-region-location-absolute
    cref-copy-region-location-git
    cref-copy-region-location-filename
    cref-copy-region-with-location
    cref-copy-region-with-absolute-location
    cref-copy-region-with-git-location
    cref-copy-region-with-filename-location)
  "All public region copy commands subject to AC-0020-0060.")

(defun cref-test--assert-region-commands-silent ()
  "Invoke each region command and assert silence semantics.
Each command must signal `user-error' and leave both
`cref-test--clipboard-content' and `kill-ring' untouched from their
pre-call sentinel values.
Caller is responsible for establishing the target buffer context
\(e.g. dired buffer or non-file-visiting temp buffer); this helper does
not create one."
  (dolist (cmd cref-test--region-commands)
    (ert-info ((format "command=%s" cmd))
      (let ((cref-test--clipboard-content 'sentinel)
            (kill-ring (list "kr-sentinel")))
        (cref-test-with-xclip-mock
          (should-error (funcall cmd) :type 'user-error))
        (should (eq cref-test--clipboard-content 'sentinel))
        (should (equal kill-ring '("kr-sentinel")))))))

(ert-deftest cref-test-copy-region-location-dired-is-silent ()
  "AC-0020-0060 (dired branch): region copy commands inside a dired buffer
error with `user-error' and leave clipboard and `kill-ring' untouched."
  (cref-test-with-dired "sample.txt"
    (dired-goto-file temp-file)
    (cref-test--assert-region-commands-silent)))

(ert-deftest cref-test-copy-region-location-temp-buffer-is-silent ()
  "AC-0020-0060 (non-file non-dired branch): region copy commands in a
buffer not visiting any file error with `user-error' and leave both
clipboard and `kill-ring' untouched."
  (cref-test-with-temp-buffer "line 1\nline 2\nline 3"
    (cref-test--assert-region-commands-silent)))

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
