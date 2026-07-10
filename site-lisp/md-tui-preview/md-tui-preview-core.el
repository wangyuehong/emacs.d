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

;;; Margin

(defconst md-tui-preview--glow-margin-width 2
  "Left-margin columns glow's \"dark\" style pads every content line with.
Glow has no CLI flag to disable this (only `--style', a name or a full
custom style JSON path), so `md-tui-preview--strip-margin' removes it
from the buffer instead.")

(defun md-tui-preview--strip-margin ()
  "Remove glow's left margin from every line in the current buffer.
Only removes `md-tui-preview--glow-margin-width' literal leading space
characters from a line that actually starts with them, leaving blank
lines and any line without that margin untouched.  Must run after the
buffer's ANSI escape sequences have already been stripped (e.g. by
`ansi-color-apply-on-region'), since a line can start with escape bytes
rather than the margin spaces before that point."
  (let ((prefix (make-string md-tui-preview--glow-margin-width ?\s)))
    (goto-char (point-min))
    (while (not (eobp))
      (when (looking-at-p (regexp-quote prefix))
        (delete-char md-tui-preview--glow-margin-width))
      (forward-line 1))))

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

;;; Link Parsing
;;
;; Modeled after markdown-mode's own link recognition (its bracket/
;; parenthesis shape for inline and reference links, its bracket shape
;; for autolinks, and its use of `scan-sexps' in `markdown-link-at-pos'
;; to find an inline target's true closing parenthesis even when the
;; target itself contains balanced parentheses, e.g. a Wikipedia-style
;; URL) but reimplemented here rather than depending on markdown-mode at
;; runtime: entering the preview requires the buffer's real major mode
;; to already be `markdown-mode', but this package's test suite
;; deliberately stays independent of whichever markdown-mode version
;; (if any) happens to be installed -- see run-tests.el's own stub.

(defconst md-tui-preview--link-regexp
  "\\(!?\\)\\[\\(\\(?:\\\\\\]\\|[^]\n]\\)*\\)\\]\\(?:(\\|\\[\\([^]\n]*\\)\\]\\)"
  "Regexp matching the start of an inline or reference link/image.
Group 1: optional \"!\" image marker.  Group 2: label text (an escaped
\"\\]\" does not end it early).  For the inline form, matches only
through the opening \"(\" -- `md-tui-preview--inline-link-target' takes
over from there via `scan-sexps'.  Group 3: reference id, present only
for the reference form.")

(defconst md-tui-preview--angle-uri-regexp
  "<\\(\\(?:https?\\|mailto\\):[^<>\n ]+\\)>"
  "Regexp matching an autolink.
Restricted to http/https/mailto per SPEC.md US-0050.  Group 1: the
target.")

(defconst md-tui-preview--reference-def-regexp
  "^[ \t]\\{0,3\\}\\[\\([^]\n]+\\)\\]:[ \t]*\\(<[^>\n]*>\\|\\S-+\\)"
  "Regexp matching a Markdown reference definition line.
Group 1: reference id.  Group 2: target, still wrapped in angle
brackets when written that way (a trailing title, if any, is not part
of this group and is ignored).")

(defun md-tui-preview--collect-reference-defs ()
  "Return a hash table mapping downcased reference ids to targets.
Scans the current buffer for lines of the form \"[id]: target\" or
\"[id]: <target>\", unwrapping the latter's angle brackets."
  (let ((defs (make-hash-table :test #'equal)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward md-tui-preview--reference-def-regexp nil t)
        (let ((target (match-string 2)))
          (puthash (downcase (match-string 1))
                   (if (string-match "\\`<\\(.*\\)>\\'" target)
                       (match-string 1 target)
                     target)
                   defs))))
    defs))

(defun md-tui-preview--link-kind (target)
  "Return the navigation kind for TARGET, or nil if unsupported.
Returns `url' for http/https/mailto targets, and `file' for file://
URIs, absolute paths, and relative paths.  Returns nil for any other
scheme (e.g. \"javascript:\", \"ftp:\") and for a target containing a
\"#\" heading fragment (\"file.md#heading\" or a bare \"#heading\"),
per SPEC.md US-0050's declared out-of-scope forms."
  (cond
   ((string-match-p "\\`\\(https?\\|mailto\\):" target) 'url)
   ((string-match-p "#" target) nil)
   ((string-match-p "\\`file://" target) 'file)
   ((string-match-p "\\`[a-zA-Z][a-zA-Z0-9+.-]*:" target) nil)
   (t 'file)))

(defun md-tui-preview--inline-link-target ()
  "Return (TARGET . END) for the inline link parenthesis group at point.
Returns nil when the parenthesis is unbalanced (no matching close paren
before the end of the buffer) -- that single malformed construct is
then simply not treated as a link, per SPEC.md US-0050's silent-skip
handling of an unmatched candidate; scanning continues, rather than a
`scan-sexps' failure aborting the rest of the document.
Point must sit right after its opening \"(\", as
`md-tui-preview--link-regexp' leaves it.  Finds the true closing
parenthesis via `scan-sexps' even when TARGET itself contains balanced
parentheses, splits off and discards an optional trailing title in
double quotes, and unwraps a target wrapped in angle brackets to permit
whitespace -- the same handling markdown-mode's own
`markdown-link-at-pos' applies to inline links."
  (let ((end (condition-case nil (scan-sexps (1- (point)) 1) (scan-error nil))))
    (when end
      (let ((inside (string-trim
                     (buffer-substring-no-properties (point) (max (point) (1- end))))))
        (cons
         (cond
          ((string-match "\\`<\\(.+\\)>\\'" inside) (match-string 1 inside))
          ((string-match "\\`\\([^ \t\n]+\\)[ \t\n]" inside) (match-string 1 inside))
          (t inside))
         end)))))

(defun md-tui-preview--next-link-match (limit)
  "Move point to the start of the next link-like construct before LIMIT.
Tries `md-tui-preview--link-regexp' and `md-tui-preview--angle-uri-regexp'
independently from point and returns the symbol `bracket' or `angle'
for whichever matches earliest, with match data set for that regexp.
Returns nil, and does not move point, when neither matches."
  (let ((bracket-pos (save-excursion
                       (save-match-data
                         (and (re-search-forward md-tui-preview--link-regexp limit t)
                              (match-beginning 0)))))
        (angle-pos (save-excursion
                    (save-match-data
                      (and (re-search-forward md-tui-preview--angle-uri-regexp limit t)
                           (match-beginning 0))))))
    (cond
     ((and bracket-pos (or (not angle-pos) (<= bracket-pos angle-pos)))
      (goto-char bracket-pos)
      (re-search-forward md-tui-preview--link-regexp limit t)
      'bracket)
     (angle-pos
      (goto-char angle-pos)
      (re-search-forward md-tui-preview--angle-uri-regexp limit t)
      'angle))))

(defun md-tui-preview--parse-links (markdown-text)
  "Return an ordered list of navigable links found in MARKDOWN-TEXT.
Each element is a plist (:label LABEL-OR-NIL :target TARGET :kind KIND),
in the order the links appear in MARKDOWN-TEXT.  Recognizes inline
links, reference-style links (together with their definitions), and
http/https/mailto autolinks.  Image syntax, dangling references (a
reference id with no matching definition), the collapsed reference form
\"[text][]\", and unsupported schemes are excluded from the result."
  (with-temp-buffer
    (insert markdown-text)
    (goto-char (point-min))
    (let ((defs (md-tui-preview--collect-reference-defs))
          links match-kind)
      (while (setq match-kind (md-tui-preview--next-link-match (point-max)))
        (if (eq match-kind 'angle)
            (let* ((target (match-string 1))
                   (kind (md-tui-preview--link-kind target))
                   (on-def-line (save-excursion
                                  (goto-char (line-beginning-position))
                                  (looking-at-p md-tui-preview--reference-def-regexp))))
              ;; An angle-bracket target on a reference definition line
              ;; (e.g. "[id]: <url>") is that definition's target, already
              ;; captured by `md-tui-preview--collect-reference-defs' --
              ;; not a second, standalone autolink at the same position.
              (when (and kind (not on-def-line))
                (push (list :label nil :target target :kind kind) links)))
          (let ((bang (match-string 1))
                (label (match-string 2))
                (ref-id (match-string 3)))
            (cond
             ;; Image ("!" marker): matched only to consume it, never a link.
             ((and bang (not (string-empty-p bang))))
             ((not ref-id)
              (let ((result (md-tui-preview--inline-link-target)))
                (when result
                  (pcase-let ((`(,target . ,end) result))
                    (let ((kind (and (not (string-empty-p target))
                                      (md-tui-preview--link-kind target))))
                      (when kind
                        (push (list :label label :target target :kind kind) links)))
                    (goto-char (max end (point)))))))
             ((not (string-empty-p ref-id))
              (let* ((target (gethash (downcase ref-id) defs))
                     (kind (and target (md-tui-preview--link-kind target))))
                (when kind
                  (push (list :label label :target target :kind kind) links))))))))
      (nreverse links))))

;;; Rendered-Text Search

(defun md-tui-preview--strip-inline-markup (word)
  "Strip leading/trailing Markdown emphasis/code markup from WORD.
Removes any run of `*', `_', `~', or backtick characters immediately at
the start or end of WORD.  Approximates how glow renders emphasis,
strong emphasis, strikethrough, and inline code: the markup characters
are dropped, and only the enclosed text remains visible."
  (replace-regexp-in-string "\\`[*_~`]+\\|[*_~`]+\\'" "" word))

(defun md-tui-preview--search-regexp (text &optional strip-markup)
  "Return a regexp that finds TEXT in glow's rendered output.
Splits TEXT on whitespace and joins the tokens with a pattern matching
one or more whitespace characters of any kind, including newlines, so
the result still matches after glow re-wraps TEXT onto different
lines.  When STRIP-MARKUP is non-nil (for a link label, never for a
target), also strips Markdown emphasis/code markup from each token's
edges via `md-tui-preview--strip-inline-markup', since glow renders
that markup as ANSI styling rather than literal characters."
  (mapconcat
   #'regexp-quote
   (seq-remove
    #'string-empty-p
    (mapcar (lambda (word)
              (if strip-markup (md-tui-preview--strip-inline-markup word) word))
            (split-string text "[ \t\n]+" t)))
   "[ \t\n]+"))

(provide 'md-tui-preview-core)
;;; md-tui-preview-core.el ends here
