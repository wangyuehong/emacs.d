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
        (inhibit-read-only t))
    (erase-buffer)
    (md-tui-preview--with-theme-ansi-colors
     (lambda ()
       (insert (md-tui-preview--render-string source (md-tui-preview--effective-width)))
       (ansi-color-apply-on-region (point-min) (point-max))))
    (set-buffer-modified-p was-modified))
  (goto-char (point-min)))

(add-hook 'md-tui-preview-mode-hook #'md-tui-preview--finish-setup 90)

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

(provide 'md-tui-preview)
;;; md-tui-preview.el ends here
