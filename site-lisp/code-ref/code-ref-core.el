;;; code-ref-core.el --- Core functions for code-ref -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; Core definitions and helper functions for code-ref.
;; Provides path formatting, region handling, and clipboard operations.
;;
;;; Code:

(declare-function dired-get-filename "dired" (&optional localp no-error-if-not-filep))
(declare-function dired-current-directory "dired" (&optional localp))

(defgroup code-ref nil
  "Copy code references and snippets to clipboard."
  :group 'convenience
  :prefix "cref-")

(defcustom cref-location-prefix "@"
  "Prefix for location strings."
  :type 'string
  :safe #'stringp)

(defcustom cref-save-before-copy t
  "Whether to save buffer before copying region with location."
  :type 'boolean
  :safe #'booleanp)

;;; Path Functions

(defun cref--current-source-file ()
  "Return the absolute source file path associated with the current buffer.
Handles file-visiting buffers and Dired buffers:
- In a file-visiting buffer, returns variable `buffer-file-name' (truename).
- In a Dired buffer, returns the entry at point, falling back to the
  current Dired subdirectory when point is not on a file entry.
Returns nil when no source file can be determined."
  (cond
   (buffer-file-name
    (file-truename buffer-file-name))
   ((derived-mode-p 'dired-mode)
    (file-truename
     (or (dired-get-filename nil t)
         (directory-file-name (dired-current-directory)))))))

(defun cref--require-source-file ()
  "Return `cref--current-source-file' or signal a `user-error'."
  (or (cref--current-source-file)
      (user-error "Current buffer is not visiting a file")))

(defun cref--get-git-root ()
  "Get the Git repository root directory for the current source file.
Returns nil if the source file is not inside a Git repository."
  (when-let* ((source (cref--current-source-file))
              (git-root (locate-dominating-file source ".git")))
    (expand-file-name git-root)))

(defun cref--get-project-root ()
  "Get the Emacs project root directory for the current buffer.
Returns nil if not in a project."
  (when-let* ((proj (project-current)))
    (project-root proj)))

(defun cref--format-file-path (style)
  "Format the current source file path according to STYLE.
STYLE can be:
  \\='absolute - Full absolute path
  \\='git - Relative to Git root
  \\='project - Relative to Emacs project root
  \\='filename - Just the filename
Signals `user-error' when no source file is available, or when the
requested style cannot be satisfied (e.g. Git style outside a repo)."
  (let ((absolute-path (cref--require-source-file)))
    (pcase style
      ('absolute absolute-path)
      ('filename (file-name-nondirectory absolute-path))
      ('git
       (if-let* ((git-root (cref--get-git-root)))
           (file-relative-name absolute-path git-root)
         (user-error "Not in a Git repository")))
      ('project
       (if-let* ((proj-root (cref--get-project-root)))
           (file-relative-name absolute-path proj-root)
         (user-error "Not in a project")))
      (_ (error "Invalid style: %s" style)))))

(defun cref--get-buffer-display-path ()
  "Return the buffer's display path.
Git-relative when the source file is inside a Git repository,
otherwise the absolute source path.
Signals `user-error' when no source file can be determined."
  (let* ((file-path (cref--require-source-file))
         (git-root (cref--get-git-root)))
    (if git-root
        (file-relative-name file-path git-root)
      file-path)))

(defun cref--get-path-by-style (style)
  "Get buffer path by STYLE.
STYLE: \\='display, \\='absolute, \\='git, \\='project, or \\='filename.
Errors propagate; callers are expected to handle `user-error' raised by
the underlying path resolution (e.g. buffers without an associated file)."
  (if (eq style 'display)
      (cref--get-buffer-display-path)
    (cref--format-file-path style)))

;;; Region Functions

(defun cref--get-region-or-line ()
  "Get region boundaries or current line.
Returns plist (:start START :end END :is-region BOOL)."
  (let ((is-region (use-region-p)))
    (if (and is-region (> (region-end) (region-beginning)))
        (let ((start (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (when (bolp)
              (setq end (line-end-position 0))))
          (list :start start :end end :is-region t))
      (list :start (line-beginning-position)
            :end (line-end-position)
            :is-region nil))))

(defun cref--save-buffer-if-modified ()
  "Save buffer if modified. Returns t if saved, nil otherwise."
  (when (and cref-save-before-copy buffer-file-name (buffer-modified-p))
    (save-buffer)
    t))

;;; Markdown Formatting

(defun cref--find-max-backtick-sequence (text)
  "Return the length of the longest sequence of consecutive backticks in TEXT."
  (let ((max-len 0)
        (current-len 0)
        (len (length text)))
    (dotimes (i len)
      (if (eq (aref text i) ?\`)
          (setq current-len (1+ current-len))
        (setq max-len (max max-len current-len)
              current-len 0)))
    (max max-len current-len)))

(defun cref--make-code-fence (text)
  "Make Markdown code fence string for TEXT.
Returns fence with length adjusted to avoid conflicts with backticks in TEXT."
  (let* ((max-inner-ticks (cref--find-max-backtick-sequence text))
         (fence-len (max 3 (1+ max-inner-ticks))))
    (make-string fence-len ?\`)))

;;; Location String

(defun cref--get-region-location-string (location-path bounds)
  "Get region location string with LOCATION-PATH and BOUNDS plist.
Returns format like @file#L10 or @file#L10-L20."
  (let* ((start (plist-get bounds :start))
         (end (plist-get bounds :end))
         (start-line (line-number-at-pos start t))
         (end-line (line-number-at-pos end t)))
    (if (= start-line end-line)
        (format "%s%s#L%d" cref-location-prefix location-path start-line)
      (format "%s%s#L%d-L%d" cref-location-prefix location-path start-line end-line))))

(defun cref--get-region-content-with-fence (bounds)
  "Get region content wrapped with Markdown code fence.
BOUNDS is a plist with :start and :end."
  (let* ((start (plist-get bounds :start))
         (end (plist-get bounds :end))
         (selected-text (buffer-substring-no-properties start end))
         (fence (cref--make-code-fence selected-text)))
    (format "%s\n%s\n%s" fence selected-text fence)))

;;; Clipboard

(defun cref--copy-to-clipboard (text)
  "Copy TEXT to system clipboard or kill-ring.
Returns t if copied to system clipboard, nil if only to kill-ring."
  (if (fboundp 'xclip-set-selection)
      (progn
        (xclip-set-selection 'clipboard text)
        t)
    (kill-new text)
    nil))

(provide 'code-ref-core)
;;; code-ref-core.el ends here
