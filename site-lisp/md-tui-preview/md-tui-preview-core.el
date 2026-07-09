;;; md-tui-preview-core.el --- Core functions for md-tui-preview -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; Core definitions and helper functions for md-tui-preview.
;; Renders Markdown text through the `glow' CLI and recolors its ANSI
;; output to match the current Emacs theme.
;;
;;; Code:

(require 'ansi-color)
(require 'cl-lib)

(defgroup md-tui-preview nil
  "Terminal Markdown preview rendered by Glow, colored to match the theme."
  :group 'convenience
  :prefix "md-tui-preview-")

(defcustom md-tui-preview-glow-args '("--style" "dark" "--pager=false")
  "Command-line arguments passed to the `glow' executable.
Does not include the trailing \"-\" stdin marker, which is always
appended automatically."
  :type '(repeat string)
  :group 'md-tui-preview)

;;; Theme Color Mapping
;;
;; In Emacs 28+, `ansi-color-apply-on-region' no longer reads the
;; obsolete `ansi-color-names-vector'.  It resolves SGR 30-37/40-47 codes
;; through `ansi-color-normal-colors-vector' and 90-97/100-107 through
;; `ansi-color-bright-colors-vector', both of which hold FACE SYMBOLS
;; (`ansi-color-red', `ansi-color-bright-blue', ...); the actual color
;; comes from `face-foreground'/`face-background' on those faces.
;; `face-remap-add-relative' does not affect what those functions report,
;; so the only way to make rendered colors follow the theme is to
;; temporarily `set-face-attribute' the faces themselves.
;;
;; Foregrounds for red/green/yellow/blue/magenta/cyan come from
;; `ansi-color-names-vector' itself, not from unrelated semantic faces
;; (`error', `font-lock-keyword-face', ...).  Many themes -- including
;; this config's srcery -- still customize that vector with colors that
;; match each ANSI slot's conventional hue (red really is red, blue
;; really is blue), even though `ansi-color.el' no longer reads it for
;; rendering.  Mapping ANSI "blue" to an arbitrary semantic face instead
;; (e.g. a theme's red-hued keyword face) breaks the hue Glow's own style
;; assumes for that slot.  When the active theme leaves the vector at
;; its stock default, colors just fall back to Emacs's hue-sane built-in
;; palette.
;;
;; Black and white are sourced from the `default' face instead: many
;; themes reserve their ANSI "white" slot for a muted/dim shade (srcery's
;; is a plain gray), which looks like a comment face when Glow uses it
;; for ordinary paragraph text -- the buffer's actual body-text color is
;; `default's foreground, not whatever the theme calls "ANSI white".
;;
;; Backgrounds are always the buffer's own background, regardless of
;; slot: Glow's "dark" style paints background bars/boxes for a few
;; elements (the H1 banner, inline code spans) using ANSI slots that are
;; otherwise meant for foreground text (blue, black).  Making every slot's
;; background match the buffer means text still gets colored, but no
;; bars or boxes appear -- a plain-text look consistent with the rest of
;; the buffer, and robust to any other background a style happens to
;; paint with a basic-16 color.

(defconst md-tui-preview--ansi-vector-faces
  '(ansi-color-red ansi-color-green ansi-color-yellow
    ansi-color-blue ansi-color-magenta ansi-color-cyan)
  "The 6 base `ansi-color-*' faces sourced from `ansi-color-names-vector'.
Listed in the same slot order as that vector's slots 1-6 (SGR 31-36).")

(defconst md-tui-preview--ansi-bright-face-alist
  '((ansi-color-black   . ansi-color-bright-black)
    (ansi-color-red     . ansi-color-bright-red)
    (ansi-color-green   . ansi-color-bright-green)
    (ansi-color-yellow  . ansi-color-bright-yellow)
    (ansi-color-blue    . ansi-color-bright-blue)
    (ansi-color-magenta . ansi-color-bright-magenta)
    (ansi-color-cyan    . ansi-color-bright-cyan)
    (ansi-color-white   . ansi-color-bright-white))
  "Alist mapping a base `ansi-color-*' face to its bright counterpart.
The current theme has no 16 distinct semantic slots to draw from, so the
bright variant always reuses the base variant's color.")

(defun md-tui-preview--theme-ansi-foregrounds ()
  "Return an alist of (ANSI-FACE . COLOR) foregrounds for the 16 base faces.
See the commentary above this section for how each slot's color is
sourced."
  (let* ((default-bg (face-attribute 'default :background nil t))
         (default-fg (face-attribute 'default :foreground nil t))
         result)
    (dolist (pair `((ansi-color-black . ,default-bg)
                    (ansi-color-white . ,default-fg)))
      (push pair result)
      (push (cons (alist-get (car pair) md-tui-preview--ansi-bright-face-alist) (cdr pair))
            result))
    (cl-loop for base-face in md-tui-preview--ansi-vector-faces
             for color across (seq-subseq ansi-color-names-vector 1 7)
             do (push (cons base-face color) result)
                (push (cons (alist-get base-face md-tui-preview--ansi-bright-face-alist) color)
                      result))
    result))

(defun md-tui-preview--with-theme-ansi-colors (thunk)
  "Call THUNK with the 16 base ansi-color faces recolored to match the theme.
Every face's foreground follows `md-tui-preview--theme-ansi-foregrounds';
every face's background is set to the buffer's own background, so no
background bars or boxes appear.  Restores the original colors
afterward.  Safe because Emacs is single-threaded and THUNK runs
synchronously, so no other code observes the faces mid-override."
  (let ((default-bg (face-attribute 'default :background nil t))
        originals)
    (unwind-protect
        (progn
          (pcase-dolist (`(,ansi-face . ,fg) (md-tui-preview--theme-ansi-foregrounds))
            (push (list ansi-face
                        (face-attribute ansi-face :foreground nil t)
                        (face-attribute ansi-face :background nil t))
                  originals)
            (set-face-attribute ansi-face nil :foreground fg :background default-bg))
          (funcall thunk))
      (dolist (spec originals)
        (set-face-attribute (nth 0 spec) nil
                             :foreground (nth 1 spec)
                             :background (nth 2 spec))))))

;;; Width

(defun md-tui-preview--effective-width ()
  "Return the usable rendering width of the selected window, in columns.
`window-body-width' does not subtract the `display-line-numbers-mode'
gutter -- it is not a margin, fringe, or scroll bar, so Emacs does not
count it as part of \"the text area\" that function documents.  This
subtracts that gutter's width explicitly (plus its 2 padding columns,
per `line-number-display-width''s docstring) when the mode is active in
the current buffer."
  (- (window-body-width)
     (if (bound-and-true-p display-line-numbers-mode)
         (+ 2 (ceiling (line-number-display-width 'columns)))
       0)))

;;; Rendering

(defun md-tui-preview--render-string (markdown-text &optional width)
  "Return MARKDOWN-TEXT rendered by glow as raw ANSI-escaped text.
WIDTH, when non-nil, is passed to glow as `--width' so it wraps to that
many columns instead of its own tty-less default guess.
Signals `user-error' if the glow process exits with a non-zero status."
  (with-temp-buffer
    (insert markdown-text)
    (let* ((process-environment
            (append '("CLICOLOR_FORCE=1" "FORCE_COLOR=1" "TERM=xterm-256color")
                    process-environment))
           (args (append md-tui-preview-glow-args
                         (when width (list "--width" (number-to-string width)))
                         '("-")))
           (status (apply #'call-process-region
                           (point-min) (point-max) "glow" t t nil args)))
      (unless (and (integerp status) (zerop status))
        (user-error "Glow failed to render (%s): %s" status (buffer-string)))
      (buffer-string))))

(provide 'md-tui-preview-core)
;;; md-tui-preview-core.el ends here
