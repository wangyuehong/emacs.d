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
(require 'color)
(require 'subr-x)

(defgroup md-tui-preview nil
  "Terminal Markdown preview rendered by Glow, colored to match the theme."
  :group 'convenience
  :prefix "md-tui-preview-")

(defconst md-tui-preview--directory
  (file-name-directory (or load-file-name buffer-file-name
                           (locate-library "md-tui-preview-core")))
  "Directory this package's files live in, including the bundled style.")

(defconst md-tui-preview--style-file
  (expand-file-name "md-tui-preview-style.json" md-tui-preview--directory)
  "Absolute path to the bundled glow style JSON.
Generated from glamour's upstream \"dark\" style plus the layout
overrides in md-tui-preview-style.jq (see the package Makefile's
`style'/`style-check' targets).  Bundled rather than using glow's
built-in \"dark\" so the ANSI color slots it emits stay fixed for
`md-tui-preview--with-theme-ansi-colors' to remap; see the Theme Color
Mapping commentary below.")

(defcustom md-tui-preview-glow-args
  (list "--style" md-tui-preview--style-file "--pager=false")
  "Command-line arguments passed to the `glow' executable.
The default points `--style' at the bundled style file (see
`md-tui-preview--style-file').  Does not include the trailing \"-\"
stdin marker, which is always appended automatically."
  :type '(repeat string)
  :group 'md-tui-preview)

(defcustom md-tui-preview-code-block-background 'auto
  "Background color for fenced code blocks in the preview.
glow does not emit a code-block background of its own; this package
overlays one so the code region stands out from surrounding prose.
`auto' derives a shade from the `default' face's background that
stays distinct under both dark and light themes.  A color string is
used literally.  nil disables the code-block background entirely."
  :type '(choice (const :tag "Off" nil)
                 (const :tag "Auto (follow theme)" auto)
                 (color :tag "Explicit color"))
  :group 'md-tui-preview)

(defcustom md-tui-preview-mermaid-ascii-args nil
  "Command-line arguments passed to the `mermaid-ascii' executable.
Used when rendering fenced Mermaid code blocks into diagrams; see
`md-tui-preview--substitute-mermaid-blocks'.  The default (empty)
leaves the tool's own defaults in effect, including Unicode
box-drawing characters; pass \"--ascii\" here to render with plain
ASCII characters instead."
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
;; A subtlety worth stating so it is not mistaken for a bug: the bundled
;; style declares its colors as 256-palette indices (H1 background "63",
;; inline-code background "236", ...), and `ansi-color' resolves a
;; 256-color SGR (38;5;N / 48;5;N, N>=16) to a fixed hex, bypassing these
;; faces.  But glow/termenv downsamples those style colors to basic-16
;; aixterm codes in its actual escape output -- H1 background emits 104
;; (bright blue), inline-code background emits 40 (black), foregrounds
;; emit 30-37/90-97 -- exactly the range resolved through the faces
;; below, so the remap does apply to what glow really writes.  The lone
;; exception is fenced code-block syntax highlighting, whose chroma
;; colors glow does emit as fixed 256-color/truecolor and which
;; therefore do not follow the theme (see US-0070 in SPEC.md).
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
;; slot: the bundled style (glamour's "dark" base) still colors a few
;; elements' backgrounds (the H1 title, inline code spans) using ANSI
;; slots that are otherwise meant for foreground text (blue, black).
;; Making every slot's background match the buffer means text still gets
;; colored, but no bars or boxes appear -- a plain-text look consistent
;; with the rest of the buffer, and robust to any other background a
;; style happens to paint with a basic-16 color.

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
         (base-pairs (append `((ansi-color-black . ,default-bg)
                                (ansi-color-white . ,default-fg))
                              (cl-loop for base-face in md-tui-preview--ansi-vector-faces
                                       for color across (seq-subseq ansi-color-names-vector 1 7)
                                       collect (cons base-face color))))
         result)
    (dolist (pair base-pairs result)
      (push pair result)
      (push (cons (alist-get (car pair) md-tui-preview--ansi-bright-face-alist) (cdr pair))
            result))))

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

(defun md-tui-preview--call-process-piping (program args text)
  "Run PROGRAM with ARGS, piping TEXT to its stdin, and return (STATUS . OUTPUT).
STATUS is the process's exit status; OUTPUT is everything it wrote to
stdout (stderr merged in, per `call-process-region''s single-buffer
target).  Never signals -- callers decide what a failing STATUS means,
the shared subprocess mechanics behind both `md-tui-preview--render-string'
and `md-tui-preview--call-mermaid-ascii'."
  (with-temp-buffer
    (insert text)
    (let ((status (apply #'call-process-region (point-min) (point-max) program t t nil args)))
      (cons status (buffer-string)))))

(defun md-tui-preview--render-string (markdown-text &optional width)
  "Return MARKDOWN-TEXT rendered by glow as raw ANSI-escaped text.
WIDTH, when non-nil, is passed to glow as `--width' so it wraps to that
many columns instead of its own tty-less default guess.
Signals `user-error' if the glow process exits with a non-zero status."
  (let* ((process-environment
          (append '("CLICOLOR_FORCE=1" "FORCE_COLOR=1" "TERM=xterm-256color")
                  process-environment))
         (args (append md-tui-preview-glow-args
                       (when width (list "--width" (number-to-string width)))
                       '("-")))
         (result (md-tui-preview--call-process-piping "glow" args markdown-text))
         (status (car result)))
    (unless (and (integerp status) (zerop status))
      (user-error "Glow failed to render (%s): %s" status (cdr result)))
    (cdr result)))

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

;;; Code Block Background
;;
;; glow does not emit a background for fenced code blocks (its chroma
;; formatter paints only foreground colors when rendering to captured,
;; non-tty output), so the block does not visually stand out from the
;; surrounding prose.  This package supplies the background itself: it
;; parses fenced code blocks out of the Markdown source, locates each
;; block's rendered text, and overlays a background on the span -- the
;; same source-to-rendered search the link locator uses.

(defun md-tui-preview--color-dark-p (color)
  "Return non-nil when COLOR reads as dark, per the built-in `color-dark-p'.
COLOR is any name `color-name-to-rgb' accepts; returns nil when it
cannot be parsed."
  (when-let* ((rgb (color-name-to-rgb color)))
    (color-dark-p rgb)))

(defun md-tui-preview--code-block-background ()
  "Return the code-block background color, or nil when disabled.
Resolves `md-tui-preview-code-block-background': `auto' nudges the
`default' face's background lighter on a dark theme and darker on a
light one; a color string is returned as-is; nil disables it."
  (pcase md-tui-preview-code-block-background
    ('nil nil)
    ('auto (let ((bg (face-attribute 'default :background nil t)))
             ;; `default's background can be unparseable (e.g. the
             ;; symbolic `unspecified-bg' in a display-less batch
             ;; session); derive only from a real color, else disable.
             (when (color-name-to-rgb bg)
               (if (md-tui-preview--color-dark-p bg)
                   (color-lighten-name bg 10)
                 (color-darken-name bg 10)))))
    ((and color (pred stringp)) color)))

(defconst md-tui-preview--code-fence-open-regexp
  "\\`[ \t]\\{0,3\\}\\(`\\{3,\\}\\|~\\{3,\\}\\)"
  "Regexp matching a fenced code block's opening line.
Group 1 is the fence marker run (backticks or tildes); an info string
after it is ignored.")

(defun md-tui-preview--code-fence-close-regexp (marker)
  "Return a regexp matching a closing fence for MARKER.
MARKER is the opening fence's marker run; the close must use the same
character and be at least as long, with nothing but whitespace after."
  (format "\\`[ \t]\\{0,3\\}%c\\{%d,\\}[ \t]*\\'"
          (aref marker 0) (length marker)))

(defun md-tui-preview--scan-fenced-blocks (markdown-text)
  "Return an ordered list of every fenced code block in MARKDOWN-TEXT.
Each element is a plist (:begin BEGIN :end END :info INFO :closed CLOSED
:body-begin BODY-BEGIN :body-end BODY-END).  BEGIN/END are 0-based
character offsets into MARKDOWN-TEXT spanning the whole block,
including both fence lines; BODY-BEGIN/BODY-END bound the block's raw
content between the two fence lines, blank lines and indentation
included verbatim.  When CLOSED is nil (an unclosed block trailing off
at end of text), END and BODY-END both equal the end of MARKDOWN-TEXT.
INFO is the opening fence's info string exactly as written, untrimmed.

This is the shared primitive behind `md-tui-preview--parse-code-blocks'
and `md-tui-preview--find-mermaid-blocks': each projects this list down
to whatever slice of MARKDOWN-TEXT (via `substring') and whichever
subset of blocks (open/closed, tagged/untagged) its own contract needs,
rather than re-walking the buffer independently."
  (with-temp-buffer
    (insert markdown-text)
    (goto-char (point-min))
    (let (blocks close-regexp info block-begin body-begin)
      (while (not (eobp))
        (let ((line-start (point))
              (line (buffer-substring-no-properties (point) (line-end-position))))
          (cond
           ;; Inside a block: a closing fence ends it.  Tracking
           ;; close-regexp for every fence (not just ones a caller cares
           ;; about) keeps a nested/shorter fence run inside an unrelated
           ;; block from being misread as a new fence opener.
           (close-regexp
            (forward-line 1)
            (when (string-match-p close-regexp line)
              (push (list :begin (1- block-begin) :end (1- (point))
                          :info info :closed t
                          :body-begin (1- body-begin) :body-end (1- line-start))
                    blocks)
              (setq close-regexp nil)))
           ;; Outside a block: an opening fence starts one.
           ((string-match md-tui-preview--code-fence-open-regexp line)
            (setq close-regexp (md-tui-preview--code-fence-close-regexp (match-string 1 line))
                  info (substring line (match-end 0))
                  block-begin line-start)
            (forward-line 1)
            (setq body-begin (point)))
           (t (forward-line 1)))))
      ;; A block left unclosed at end of text is still reported, trailing
      ;; off at end of text -- matching how glow renders it.
      (when close-regexp
        (push (list :begin (1- block-begin) :end (1- (point-max))
                    :info info :closed nil
                    :body-begin (1- body-begin) :body-end (1- (point-max)))
              blocks))
      (nreverse blocks))))

(defun md-tui-preview--parse-code-blocks (markdown-text)
  "Return an ordered list of fenced code blocks in MARKDOWN-TEXT.
Each element is a plist (:lines LINES): LINES is the block's non-blank
content lines, in source order.  Blank lines inside a block are dropped
-- the rendered background spans continuously from the first matched
line to the last, so it already covers any blank rows between them.
Blocks with no non-blank content line are omitted.  A block left
unclosed at end of text is still reported, matching how glow renders it."
  (let (blocks)
    (dolist (block (md-tui-preview--scan-fenced-blocks markdown-text))
      (let* ((body (substring markdown-text
                               (plist-get block :body-begin) (plist-get block :body-end)))
             (lines (seq-remove #'string-blank-p (split-string body "\n"))))
        (when lines
          (push (list :lines lines) blocks))))
    (nreverse blocks)))

;;; Mermaid Diagrams
;;
;; glow has no notion of Mermaid diagram syntax; a fenced ```mermaid
;; block renders as inert source text.  This package pipes that block's
;; content through the external `mermaid-ascii' tool before handing the
;; Markdown to glow, so the block renders as a diagram instead.  See
;; SPEC.md US-0080.

(defconst md-tui-preview--mermaid-info-token-regexp
  "\\`[ \t]*\\([^ \t\n]+\\)"
  "Regexp matching a fenced code block's first info-string token.
Group 1 is the token itself, compared case-insensitively against
\"mermaid\" by `md-tui-preview--mermaid-fence-info-p'.")

(defun md-tui-preview--mermaid-fence-info-p (info)
  "Return non-nil when INFO's first whitespace-delimited token is \"mermaid\".
Comparison is case-insensitive.  INFO is the text following a fenced
code block's opening marker run, e.g. \"mermaid\" or
\"mermaid title=\\='x\\='\" -- a trailing token does not exclude the
block, only the first one is compared."
  (and (string-match md-tui-preview--mermaid-info-token-regexp info)
       (string-equal (downcase (match-string 1 info)) "mermaid")))

(defun md-tui-preview--find-mermaid-blocks (markdown-text)
  "Return an ordered list of closed Mermaid-tagged fenced blocks in MARKDOWN-TEXT.
Each element is a plist (:begin BEGIN :end END :body BODY): BEGIN/END
and BODY are projected from `md-tui-preview--scan-fenced-blocks' --
BODY is that scan's verbatim body span (blank lines and indentation
preserved), unlike `md-tui-preview--parse-code-blocks', whose
non-blank-only, language-agnostic contract only serves rendered-text
location and is insufficient here.  A block is included only when it
is closed and its opening fence's info string tags it as Mermaid, per
`md-tui-preview--mermaid-fence-info-p' -- a Mermaid-tagged fence left
unclosed at end of text is omitted, per SPEC.md US-0080's boundary
note, left for glow to render as ordinary text rather than inventing
behavior for a truncated fragment."
  (let (blocks)
    (dolist (block (md-tui-preview--scan-fenced-blocks markdown-text))
      (when (and (plist-get block :closed)
                 (md-tui-preview--mermaid-fence-info-p (plist-get block :info)))
        (push (list :begin (plist-get block :begin) :end (plist-get block :end)
                    :body (substring markdown-text
                                     (plist-get block :body-begin) (plist-get block :body-end)))
              blocks)))
    (nreverse blocks)))

(defun md-tui-preview--call-mermaid-ascii (mermaid-text)
  "Run mermaid-ascii on MERMAID-TEXT and return (STATUS . OUTPUT).
Never signals: a non-zero STATUS is returned to the caller rather than
raised, so `md-tui-preview--substitute-mermaid-blocks' can branch on it
with an explicit `if' instead of `condition-case' (SPEC.md US-0080
AC-0080-0030 requires the distinction to be made this way, not by
catching a signaled error)."
  (md-tui-preview--call-process-piping
   "mermaid-ascii" md-tui-preview-mermaid-ascii-args mermaid-text))

(defun md-tui-preview--clean-mermaid-ascii-output (output)
  "Return OUTPUT (a successful mermaid-ascii run's stdout) ready to splice in.
Strips any ANSI escapes -- untrusted subprocess output should not be
allowed to inject raw control sequences into a Markdown source that is
about to be spliced into a document already managed by
`ansi-color-apply-on-region' -- and trims trailing whitespace to a
single newline, so the block substituted into the Markdown source has
a predictable shape."
  (concat (string-trim-right (ansi-color-filter-apply output)) "\n"))

(defun md-tui-preview--mermaid-render-failure-text (status output mermaid-source)
  "Return fallback text for a Mermaid block whose render failed.
STATUS and OUTPUT are mermaid-ascii's exit status and captured
output (as returned by `md-tui-preview--call-mermaid-ascii').  The
result leads with a visible failure notice quoting OUTPUT verbatim,
followed by MERMAID-SOURCE unchanged, so the failure is never silent
\(SPEC.md US-0080 AC-0080-0030) and the user can see and fix the
diagram source directly in the preview."
  (format "[mermaid-ascii failed to render (%s): %s]\n\n%s"
          status (string-trim output) mermaid-source))

(defconst md-tui-preview--mermaid-fence-marker "````"
  "Fence marker wrapping a Mermaid block's replacement in the text handed to glow.
Four backticks rather than three, so a diagram (or a failure notice
quoting the tool's raw output) whose content happens to render a run
of exactly three backtick characters cannot prematurely close the
wrapper fence.  The opening fence additionally carries an explicit
\"text\" info string (`md-tui-preview--mermaid-fence-open'); the
closing fence must stay a bare marker per CommonMark, which is why
this constant does not include that suffix itself.")

(defconst md-tui-preview--mermaid-fence-open
  (concat md-tui-preview--mermaid-fence-marker "text")
  "Opening fence line for a Mermaid block's replacement.
An explicit \"text\" info string, rather than no info string at all,
is required: glow's syntax highlighter (chroma) guesses a language for
an *unlabeled* fenced block once it has enough lines to score a
confident guess, and a wrong guess can tokenize substrings it fails to
parse (commonly hit by CJK characters or full-width punctuation mixed
into the block) as an error token, painted with a jarring background
color by the bundled style -- reproduced directly against real `glow'
and `mermaid-ascii' binaries with both mermaid-ascii's own diagram
output and this package's failure-notice fallback text.  An explicit
\"text\" tag makes chroma skip that guess entirely and treat the block
as plain, unstyled text, which is what the unlabeled fence was always
meant to achieve.")

(defun md-tui-preview--mermaid-block-replacement (body)
  "Return the fenced replacement text for a Mermaid block whose raw content is BODY.
On success that is mermaid-ascii's diagram; on failure it is a visible
failure notice followed by BODY unchanged (SPEC.md US-0080
AC-0080-0030).  Either way the result is wrapped in the `text'-tagged
fence `md-tui-preview--mermaid-fence-open' produces, so glow treats it
as pre-formatted, unstyled text rather than reflowing or
syntax-highlighting it, and it still qualifies for the code-block
background overlay in `md-tui-preview--attach-code-block-backgrounds'."
  (let* ((result (md-tui-preview--call-mermaid-ascii body))
         (status (car result))
         (output (cdr result)))
    (concat md-tui-preview--mermaid-fence-open "\n"
            (if (and (integerp status) (zerop status))
                (md-tui-preview--clean-mermaid-ascii-output output)
              (md-tui-preview--mermaid-render-failure-text status output body))
            md-tui-preview--mermaid-fence-marker "\n")))

(defun md-tui-preview--substitute-mermaid-blocks (markdown-text)
  "Return MARKDOWN-TEXT with each Mermaid-tagged fenced block replaced.
Each block's replacement comes from
`md-tui-preview--mermaid-block-replacement'; a block's failure does
not affect any other block.  Returns MARKDOWN-TEXT unchanged, without
attempting to parse it, when `executable-find' cannot locate
`mermaid-ascii' -- the sole legal state branch decided before any
block is located, per SPEC.md US-0080 AC-0080-0020."
  (if (not (executable-find "mermaid-ascii"))
      markdown-text
    (let ((blocks (md-tui-preview--find-mermaid-blocks markdown-text))
          (cursor 0) pieces)
      (dolist (block blocks)
        (let ((begin (plist-get block :begin))
              (end (plist-get block :end)))
          (push (substring markdown-text cursor begin) pieces)
          (push (md-tui-preview--mermaid-block-replacement (plist-get block :body)) pieces)
          (setq cursor end)))
      (push (substring markdown-text cursor) pieces)
      (apply #'concat (nreverse pieces)))))

(provide 'md-tui-preview-core)
;;; md-tui-preview-core.el ends here
