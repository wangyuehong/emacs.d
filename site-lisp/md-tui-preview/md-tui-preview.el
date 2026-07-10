;;; md-tui-preview.el --- Toggle a theme-colored Glow preview of Markdown -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; Toggles the current Markdown buffer between `markdown-mode' and a
;; read-only Glow-rendered preview, colored to match the active theme.
;; Never switches to another buffer or window; the toggle rewrites the
;; current buffer in place.
;;
;; Bound as `C-c C-c g' in `markdown-mode' (added to
;; `markdown-mode-command-map') and as `C-c C-c' inside the preview
;; itself.  Requires the `glow' executable; see `init-edit.el' for the
;; `:if' gate that only loads this package in a terminal frame with
;; `glow' installed.
;;
;;; Code:

(require 'md-tui-preview-core)

(declare-function markdown-mode "markdown-mode")
(defvar markdown-mode-command-map)

(defvar-local md-tui-preview--source nil
  "Markdown source text captured before switching into `md-tui-preview-mode'.")

(defvar-local md-tui-preview--source-modified-p nil
  "Value of `buffer-modified-p' captured before entering `md-tui-preview-mode'.")

(defvar-local md-tui-preview--source-point nil
  "Value of `point' captured before switching into `md-tui-preview-mode'.")

(defvar-local md-tui-preview--source-file-name nil
  "Value of variable `buffer-file-name' captured before entering the mode.
The preview clears variable `buffer-file-name' so that `save-buffer'
cannot silently overwrite the real file with the rendered
\(non-Markdown) preview text; `special-mode' makes the buffer read-only
against editing commands, but not against saving.")

(defvar-local md-tui-preview--source-auto-save-file-name nil
  "Value of variable `buffer-auto-save-file-name' captured before entry.
Cleared alongside variable `buffer-file-name' so an idle auto-save
cannot write the rendered preview text into the source file's
auto-save file either.")

(defvar-local md-tui-preview--source-major-mode nil
  "Major mode captured before entering `md-tui-preview-mode'.
Restored on exit instead of hardcoding `markdown-mode', so a derived
mode such as `gfm-mode' (used for README files) is not silently
downgraded by a preview/edit round trip.")

(defvar md-tui-preview--pending-source-major-mode nil
  "Carries the buffer's original major mode across a mode switch.
Let-bound by `md-tui-preview-toggle' around the call to
`md-tui-preview-mode', surviving the `kill-all-local-variables' that
entering a new major mode triggers.  The mode body reads this to set
the buffer-local `md-tui-preview--source-major-mode' at the same point
as the other `--source-*' state -- before `md-tui-preview--finish-setup'
runs and can fail, unlike setting it only after `md-tui-preview-mode'
returns.")

;;;###autoload
(define-derived-mode md-tui-preview-mode special-mode "MD-Preview"
  "Read-only Glow-rendered preview of the current Markdown buffer.
Call `md-tui-preview-toggle' to return to editing.  Rendering itself
happens in `md-tui-preview--finish-setup', once other mode-hook
functions (e.g. `display-line-numbers-mode') have run; see its
docstring for why."
  (setq md-tui-preview--source (buffer-string)
        md-tui-preview--source-modified-p (buffer-modified-p)
        md-tui-preview--source-point (point)
        md-tui-preview--source-file-name buffer-file-name
        md-tui-preview--source-auto-save-file-name buffer-auto-save-file-name
        md-tui-preview--source-major-mode
        (or md-tui-preview--pending-source-major-mode #'markdown-mode))
  (setq buffer-file-name nil
        buffer-auto-save-file-name nil)
  (setq buffer-read-only t))

(defun md-tui-preview--finish-setup ()
  "Render the captured Markdown source into the current buffer.
Registered on `md-tui-preview-mode-hook' at depth 90 so it runs after
other hook functions at the default depth -- notably
`display-line-numbers-mode', added by this config's
`init-highlight.el' -- have already reserved their gutter, so
`md-tui-preview--effective-width' measures the window correctly."
  (let ((source md-tui-preview--source)
        (was-modified md-tui-preview--source-modified-p)
        (source-file-name md-tui-preview--source-file-name)
        (inhibit-read-only t))
    (erase-buffer)
    (md-tui-preview--with-theme-ansi-colors
     (lambda ()
       (insert (md-tui-preview--render-string source (md-tui-preview--effective-width)))
       (ansi-color-apply-on-region (point-min) (point-max))
       (md-tui-preview--strip-margin)
       (md-tui-preview--attach-link-properties
        (md-tui-preview--parse-links source) source-file-name)))
    (set-buffer-modified-p was-modified))
  (goto-char (point-min)))

(add-hook 'md-tui-preview-mode-hook #'md-tui-preview--finish-setup 90)

;;; Link Navigation

(defun md-tui-preview--resolve-link-value (link source-file-name)
  "Return the (KIND . VALUE) pair to store for LINK.
LINK is a plist as returned by `md-tui-preview--parse-links'.  A `file'
target has any \"file://\" prefix stripped and, when relative, is
expanded against SOURCE-FILE-NAME's directory."
  (let ((target (plist-get link :target)))
    (pcase (plist-get link :kind)
      ('url (cons 'url target))
      ('file
       (let ((path (if (string-prefix-p "file://" target)
                       (substring target (length "file://"))
                     target)))
         (cons 'file
               (expand-file-name
                path (and source-file-name (file-name-directory source-file-name))))))
      (kind (error "Unknown link kind: %s" kind)))))

(defun md-tui-preview--attach-link-properties (links source-file-name)
  "Attach `md-tui-preview-link-target' properties in the current buffer.
LINKS is an ordered list as returned by `md-tui-preview--parse-links',
located in the current buffer's rendered text by literal label and
target substring search, advancing through the buffer in order --
duplicate labels or targets do not get confused with each other, since
each search resumes from where the previous one ended.
SOURCE-FILE-NAME resolves relative file targets.
When a link's label or target text cannot be found (e.g. glow wrapped
it across lines in a way the search does not tolerate), that link is
silently left unclickable; this does not affect any other link."
  (goto-char (point-min))
  (dolist (link links)
    (let* ((label (plist-get link :label))
           (target (plist-get link :target))
           (value (md-tui-preview--resolve-link-value link source-file-name))
           (label-found
            (if (not label)
                t
              (when (re-search-forward (md-tui-preview--search-regexp label t) nil t)
                (put-text-property (match-beginning 0) (match-end 0)
                                    'md-tui-preview-link-target value)
                t))))
      (when (and label-found
                 (re-search-forward (md-tui-preview--search-regexp target) nil t))
        (put-text-property (match-beginning 0) (match-end 0)
                            'md-tui-preview-link-target value)))))

;;;###autoload
(defun md-tui-preview-follow-link-at-point ()
  "Open the link at point: a URL in the browser, a file in Emacs.
Signals `user-error' when point is not on a link recognized by
`md-tui-preview--parse-links', or when a local file target does not
exist."
  (interactive)
  (let ((target (get-text-property (point) 'md-tui-preview-link-target)))
    (unless target
      (user-error "Not on a link"))
    (pcase (car target)
      ('url (browse-url (cdr target)))
      ('file
       (unless (file-exists-p (cdr target))
         (user-error "Link target does not exist: %s" (cdr target)))
       (find-file (cdr target))))))

;;;###autoload
(defun md-tui-preview-toggle ()
  "Toggle between editing this Markdown buffer and previewing it with Glow.
Operates entirely on the current buffer and window; never switches to
another buffer or window."
  (interactive)
  (cond
   ((derived-mode-p 'md-tui-preview-mode)
    (let ((source md-tui-preview--source)
          (was-modified md-tui-preview--source-modified-p)
          (pos md-tui-preview--source-point)
          (file-name md-tui-preview--source-file-name)
          (auto-save-name md-tui-preview--source-auto-save-file-name)
          (saved-mode md-tui-preview--source-major-mode)
          (inhibit-read-only t))
      (erase-buffer)
      (insert source)
      (funcall saved-mode)
      (setq buffer-file-name file-name
            buffer-auto-save-file-name auto-save-name)
      (setq buffer-read-only nil)
      (set-buffer-modified-p was-modified)
      (goto-char (min pos (point-max)))))
   ((derived-mode-p 'markdown-mode)
    (let ((md-tui-preview--pending-source-major-mode major-mode))
      (md-tui-preview-mode)))
   (t (user-error "Not a Markdown buffer"))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-command-map (kbd "g") #'md-tui-preview-toggle))

(define-key md-tui-preview-mode-map (kbd "C-c C-c") #'md-tui-preview-toggle)
(define-key md-tui-preview-mode-map (kbd "RET") #'md-tui-preview-follow-link-at-point)

(provide 'md-tui-preview)
;;; md-tui-preview.el ends here
