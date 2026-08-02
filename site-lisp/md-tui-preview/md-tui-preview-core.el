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
(require 'subr-x)

(declare-function markdown-mode "markdown-mode")

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
What it contains and why is documented in md-tui-preview-style.jq, which
generates it.  Bundled rather than using glow's built-in \"dark\" so the
ANSI color slots it emits stay fixed for
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
;; below, so the remap does apply to what glow really writes.  Fenced
;; code blocks carry no glow-emitted syntax colors at all: the bundled
;; style drops the whole chroma section, and their highlighting is
;; applied on the Emacs side from the language's own major mode instead
;; (see US-0070 in SPEC.md).
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

(defconst md-tui-preview--width-margin 2
  "Columns left unused at the right edge of the render width.
glow pads every line out to the width it is given, so a line exactly as
wide as the text area touches the right edge -- where a text terminal
needs a column for the continuation glyph, and Emacs soft-wraps every
single line to make room.  The second column absorbs the line-number
gutter growing by a digit between measuring and displaying, which is the
boundary AC-0025-0030 describes.")

(defun md-tui-preview--effective-width ()
  "Return the usable rendering width of the selected window, in columns.
`window-body-width' does not subtract the `display-line-numbers-mode'
gutter -- it is not a margin, fringe, or scroll bar, so Emacs does not
count it as part of \"the text area\" that function documents.  This
subtracts that gutter's width explicitly when the mode is active in the
current buffer, and `md-tui-preview--width-margin' always."
  (- (window-body-width)
     md-tui-preview--width-margin
     (if (bound-and-true-p display-line-numbers-mode)
         (ceiling (line-number-display-width 'columns))
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

(defun md-tui-preview--colorize-ansi (begin end)
  "Turn the ANSI escapes between BEGIN and END into `face' text properties.
`ansi-color-apply-on-region' applies its colors through
`ansi-color-apply-face-function', whose default puts each run in an
*overlay*.  An overlay's face wins over a text property's, so the colors
glow emitted for a code block would hide the syntax faces markdown-mode
puts on that same text (SPEC.md US-0070) -- the highlighting would be
computed and then covered up.  Applying the colors as `face' text
properties keeps everything in one layer, which markdown-mode's own
fontification legitimately replaces inside a code block."
  (let ((ansi-color-apply-face-function
         (lambda (beg end face)
           (when face (put-text-property beg end 'face face)))))
    (ansi-color-apply-on-region begin end)))

;;; Link Parsing
;;
;; Modeled after markdown-mode's own link recognition (its bracket/
;; parenthesis shape for inline and reference links, its bracket shape for
;; autolinks, and its use of `scan-sexps' in `markdown-link-at-pos' to
;; find an inline target's true closing parenthesis even when the target
;; itself contains balanced parentheses, e.g. a Wikipedia-style URL), but
;; recognizing deliberately less than markdown-mode does: what may be
;; navigated is a closed set (http/https/mailto and local paths), and
;; images, dangling references, shortcut references and heading fragments
;; are all excluded on purpose.  Borrowing markdown-mode's own parser
;; would mean taking its full notion of a link and then narrowing it back
;; down; the narrow set is small enough to state directly.

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
Each element is a cons (LABEL-OR-NIL . TARGET) in the order the links
appear in MARKDOWN-TEXT.  What kind of target it is stays derivable from
TARGET itself (`md-tui-preview--link-kind'), so it is not stored.
Recognizes inline links, reference-style links (with their definitions), and
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
                (push (cons nil target) links)))
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
                    (when (and (not (string-empty-p target))
                               (md-tui-preview--link-kind target))
                      (push (cons label target) links))
                    (goto-char (max end (point)))))))
             ((not (string-empty-p ref-id))
              (let ((target (gethash (downcase ref-id) defs)))
                (when (and target (md-tui-preview--link-kind target))
                  (push (cons label target) links))))))))
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

;;; Code Blocks
;;
;; A fenced code block never reaches glow.  `md-tui-preview--mask-code-blocks'
;; swaps each block out for a one-token placeholder paragraph before
;; rendering; md-tui-preview.el puts the block's own source text back where
;; that placeholder was rendered and has markdown-mode fontify it.  A code
;; block in the preview therefore is what it is in the edit buffer -- same
;; opening ```lang line, same content, same closing fence, same faces --
;; with markdown-mode as the single implementation behind both.  Routing
;; the block around glow is also what keeps its lines verbatim: glow fills
;; and re-wraps what it renders, breaking even a long unbroken token.
;;
;; A placeholder is one token of ASCII letters and digits.  Being unbreakable
;; is the point: glow re-wraps paragraphs, and only a token with nothing to
;; break at is guaranteed to survive on a line of its own, which is what
;; lets that line be found again afterwards.

(defun md-tui-preview--placeholder (index)
  "Return the placeholder token standing in for code block INDEX.
Mixed case and a trailing letter keep it from reading as anything Markdown
styles, and it holds no character glow could wrap at."
  (format "MdTuiPreviewCodeBlock%dZ" index))

(defconst md-tui-preview--code-fence-open-regexp
  "\\`[ \t]*\\(`\\{3,\\}\\|~\\{3,\\}\\)\\([^`\n]*\\)\\'"
  "Regexp matching a fenced code block's opening line.
Group 1 is the fence marker run (backticks or tildes), group 2 the info
string.  Leading whitespace is unrestricted: a fence inside a list item
carries that item's indentation, and markdown-mode fontifies those blocks
too.  A backtick fence's info string may not contain a backtick, per
CommonMark -- without that restriction a line opening with an inline code
span (\"```code``` at line start\") reads as a fence and swallows the rest
of the document.")

(defun md-tui-preview--code-fence-close-regexp (marker)
  "Return a regexp matching a closing fence for MARKER.
MARKER is the opening fence's marker run; the close must use the same
character and be at least as long, with nothing but whitespace after."
  (format "\\`[ \t]*%c\\{%d,\\}[ \t]*\\'"
          (aref marker 0) (length marker)))

(defun md-tui-preview--mask-code-blocks (markdown-text)
  "Return (MASKED . BLOCKS) for MARKDOWN-TEXT.
MASKED is MARKDOWN-TEXT with every fenced code block replaced by a
placeholder paragraph -- the token on a line of its own with blank lines
around it, so glow renders it standalone instead of folding it into
neighboring text.  BLOCKS is the list of those blocks' source texts, both
fence lines included, in the same order the placeholders are numbered.  A
block left unclosed at end of text is masked too, ending there."
  (with-temp-buffer
    (insert markdown-text)
    (goto-char (point-min))
    (let ((cursor (point-min))
          pieces blocks close-regexp block-begin)
      (cl-flet ((mask (end)
                  (push (buffer-substring-no-properties cursor block-begin) pieces)
                  (push (format "\n%s\n\n"
                                (md-tui-preview--placeholder (length blocks)))
                        pieces)
                  (push (buffer-substring-no-properties block-begin end) blocks)
                  (setq cursor end)))
        (while (not (eobp))
          (let ((line-start (point))
                (line (buffer-substring-no-properties (point) (line-end-position))))
            (forward-line 1)
            (cond
             ;; Inside a block: a closing fence ends it, and the block runs
             ;; through that fence line.
             (close-regexp
              (when (string-match-p close-regexp line)
                (mask (point))
                (setq close-regexp nil)))
             ;; Outside a block: an opening fence starts one.  Tracking the
             ;; close regexp for every fence keeps a shorter fence run inside
             ;; an unrelated block from reading as a new opener.
             ((string-match md-tui-preview--code-fence-open-regexp line)
              (setq close-regexp (md-tui-preview--code-fence-close-regexp
                                  (match-string 1 line))
                    block-begin line-start)))))
        (when close-regexp (mask (point-max)))
        (push (buffer-substring-no-properties cursor (point-max)) pieces)
        (cons (apply #'concat (nreverse pieces)) (nreverse blocks))))))

;;; Code Block Fontification
;;
;; The block's source text is fontified by running markdown-mode itself
;; over it in a hidden buffer and copying the faces out.  markdown-mode
;; is what colors a fenced block in the edit buffer -- the fence markup,
;; the language label, and (through
;; `markdown-fontify-code-blocks-natively') the content in the language's
;; own mode -- so going through it is what makes the two states agree,
;; rather than reimplementing any part of it here.  The text in the
;; preview is byte-identical to the text fontified, so faces transfer by
;; offset with no matching or alignment involved.

(defconst md-tui-preview--fontify-buffer-name " *md-tui-preview-fontify*"
  "Name of the hidden buffer a code block's source is fontified in.
Reused across blocks and previews: entering a major mode is expensive
enough to matter on a document full of blocks.  The leading space keeps
the buffer out of the buffer list.")

(defun md-tui-preview--fontify-markdown-faces (text)
  "Return the `face' spans of TEXT as markdown-mode fontifies it.
Each element is (BEGIN END FACE) with 0-based offsets into TEXT.

Both `face' and `font-lock-face' are read: the hidden buffer's name starts
with a space, which keeps `font-lock-mode' off, and font-lock's remapping
between those two properties then never happens -- `org-src.el' carries a
comment about the same trap.  Mode hooks are delayed so entering
markdown-mode here cannot start machinery meant for real editing buffers,
and messages it emits stay out of the echo area."
  (with-current-buffer (get-buffer-create md-tui-preview--fontify-buffer-name)
    (erase-buffer)
    (insert text)
    (let ((inhibit-message t))
      (unless (derived-mode-p 'markdown-mode)
        (delay-mode-hooks (markdown-mode)))
      (font-lock-ensure))
    (let ((pos (point-min))
          spans)
      (while (< pos (point-max))
        (let ((face (or (get-text-property pos 'face)
                        (get-text-property pos 'font-lock-face)))
              (next (min (next-single-property-change pos 'face nil (point-max))
                         (next-single-property-change pos 'font-lock-face nil
                                                      (point-max)))))
          (when face
            (push (list (1- pos) (1- next) face) spans))
          (setq pos next)))
      (nreverse spans))))

(provide 'md-tui-preview-core)
;;; md-tui-preview-core.el ends here
