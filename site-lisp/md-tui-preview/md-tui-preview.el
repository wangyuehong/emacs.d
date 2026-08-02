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
;; `markdown-mode-command-map') and as `C-c C-c' or `q' inside the
;; preview itself.  Requires the `glow' executable; see `init-edit.el'
;; for the `:if' gate that only loads this package in a terminal frame
;; with `glow' installed.
;;
;;; Code:

(require 'md-tui-preview-core)

(declare-function markdown-mode "markdown-mode")
(defvar markdown-mode-command-map)

(defvar-local md-tui-preview--source-text nil
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
docstring for why.

Not a command: reaching this mode any way other than
`md-tui-preview-toggle' would capture and blank out the state of a buffer
that was never Markdown to begin with."
  :interactive nil
  (setq md-tui-preview--source-text (buffer-substring-no-properties
                                     (point-min) (point-max))
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
  (pcase-let* ((`(,render-source . ,blocks)
                (md-tui-preview--mask-code-blocks md-tui-preview--source-text))
               (was-modified md-tui-preview--source-modified-p)
               (source-file-name md-tui-preview--source-file-name)
               ;; Measure while the buffer still holds the Markdown source:
               ;; the line-number gutter then reserves room for the
               ;; document's own line count, not for an emptied buffer.
               (width (md-tui-preview--effective-width))
               ;; Render before erasing, so a failing glow leaves the source
               ;; on screen instead of an empty buffer.
               (rendered (md-tui-preview--render-string render-source width))
               (inhibit-read-only t))
    (erase-buffer)
    (md-tui-preview--with-theme-ansi-colors
     (lambda () (insert rendered)
       (md-tui-preview--colorize-ansi (point-min) (point-max))))
    (md-tui-preview--trim-leading-blank-line)
    (md-tui-preview--attach-link-properties
     (md-tui-preview--parse-links render-source) source-file-name)
    (md-tui-preview--restore-code-blocks blocks)
    (set-buffer-modified-p was-modified))
  (goto-char (point-min)))

(defun md-tui-preview--trim-leading-blank-line ()
  "Delete the rendered content's leading blank line, if glow emitted one.
A document opening with a list or a block quote comes back with a blank
first line: glamour puts it there itself, and unlike the document-level
prefixes the bundled style zeroes, no style field reaches it (AC-0060-0020)."
  (goto-char (point-min))
  (when (and (looking-at-p "^[ \t]*$") (< (line-end-position) (point-max)))
    (delete-region (point) (line-beginning-position 2))))

(add-hook 'md-tui-preview-mode-hook #'md-tui-preview--finish-setup 90)

;;; Link Navigation

(defun md-tui-preview--link-destination (target source-file-name)
  "Return what to open for link TARGET, resolved for navigation.
A URL is returned unchanged.  A local path has any \"file://\" prefix
stripped and, when relative, is expanded against SOURCE-FILE-NAME's
directory -- the document's own location, which is what a relative
Markdown link is relative to."
  (if (eq (md-tui-preview--link-kind target) 'url)
      target
    (let ((path (if (string-prefix-p "file://" target)
                    (substring target (length "file://"))
                  target)))
      (expand-file-name
       path (and source-file-name (file-name-directory source-file-name))))))

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
  (pcase-dolist (`(,label . ,target) links)
    (let* ((value (md-tui-preview--link-destination target source-file-name))
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
  (let ((destination (get-text-property (point) 'md-tui-preview-link-target)))
    (unless destination
      (user-error "Not on a link"))
    (if (eq (md-tui-preview--link-kind destination) 'url)
        (browse-url destination)
      (unless (file-exists-p destination)
        (user-error "Link target does not exist: %s" destination))
      (find-file destination))))

;;; Code Blocks

(defun md-tui-preview--insert-code-block (text)
  "Replace the placeholder line at point's line with a code block's TEXT.
TEXT is the block's own source, both fence lines included, as
`md-tui-preview--mask-code-blocks' collected it.  Point must be somewhere
on the rendered placeholder's line, which is deleted whole -- glow pads it
out to the render width, and none of that padding belongs to the block.
The inserted text is then given markdown-mode's own faces: identical text
fontified by markdown-mode means the spans transfer by offset, with no
searching or alignment in between."
  (delete-region (line-beginning-position) (line-beginning-position 2))
  (let ((begin (point)))
    (insert text)
    (unless (bolp) (insert "\n"))
    (pcase-dolist (`(,span-begin ,span-end ,face)
                   (md-tui-preview--fontify-markdown-faces text))
      (put-text-property (+ begin span-begin) (+ begin span-end) 'face face))))

(defun md-tui-preview--restore-code-blocks (blocks)
  "Put every code block in BLOCKS back where its placeholder was rendered.
BLOCKS is the source-order list `md-tui-preview--mask-code-blocks' returns,
numbered the same way it numbered the placeholders.  Each is located by
searching for its own token, so a placeholder glow moved or indented is
still found; one that cannot be found leaves that block out of the preview
rather than affecting any other."
  (let ((index 0))
    (dolist (text blocks)
      (goto-char (point-min))
      (when (search-forward (md-tui-preview--placeholder index) nil t)
        (md-tui-preview--insert-code-block text))
      (setq index (1+ index)))))

;;;###autoload
(defun md-tui-preview-toggle ()
  "Toggle between editing this Markdown buffer and previewing it with Glow.
Operates entirely on the current buffer and window; never switches to
another buffer or window."
  (interactive)
  (cond
   ((derived-mode-p 'md-tui-preview-mode)
    (let ((source md-tui-preview--source-text)
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
(define-key md-tui-preview-mode-map (kbd "q") #'md-tui-preview-toggle)
(define-key md-tui-preview-mode-map (kbd "RET") #'md-tui-preview-follow-link-at-point)

(provide 'md-tui-preview)
;;; md-tui-preview.el ends here
