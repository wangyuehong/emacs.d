;;; md-tui-preview-test.el --- Tests for md-tui-preview -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;;
;;; Commentary:
;; Unit tests for md-tui-preview.  The `glow' subprocess is mocked at the
;; `call-process-region' boundary; `markdown-mode' is stubbed by
;; run-tests.el, independent of any installed markdown-mode version.
;;
;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'display-line-numbers)
(require 'md-tui-preview-core)
(require 'md-tui-preview)

;;; Test Utilities

(defmacro md-tui-preview-test-with-mock-glow (&rest body)
  "Execute BODY with `call-process-region' mocked to echo the piped text
back wrapped in a red SGR escape, simulating glow's ANSI output."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'call-process-region)
              (lambda (start end _program _delete _buffer _display &rest _args)
                (let ((text (buffer-substring-no-properties start end)))
                  (delete-region start end)
                  (insert (format "\e[31m%s\e[0m" text)))
                0)))
     ,@body))

(defmacro md-tui-preview-test-with-failing-glow (&rest body)
  "Execute BODY with `call-process-region' mocked to simulate a failing
glow run: deletes the piped input, inserts \"boom\", returns exit
status 1."
  (declare (indent 0))
  `(cl-letf (((symbol-function 'call-process-region)
              (lambda (start end _program _delete _buffer _display &rest _args)
                (delete-region start end)
                (insert "boom")
                1)))
     ,@body))

(defmacro md-tui-preview-test-with-ansi-color-names (colors &rest body)
  "Execute BODY with `ansi-color-names-vector' bound to COLORS."
  (declare (indent 1))
  `(let ((ansi-color-names-vector ,colors))
     ,@body))

(defmacro md-tui-preview-test-with-markdown-buffer (content &rest body)
  "Execute BODY in a temp buffer in `markdown-mode' containing CONTENT."
  (declare (indent 1))
  `(with-temp-buffer
     (markdown-mode)
     (insert ,content)
     (set-buffer-modified-p nil)
     (goto-char (point-min))
     ,@body))

(defmacro md-tui-preview-test-with-mode-buffer (mode content &rest body)
  "Execute BODY in a temp buffer in MODE containing CONTENT."
  (declare (indent 2))
  `(with-temp-buffer
     (funcall ,mode)
     (insert ,content)
     (set-buffer-modified-p nil)
     (goto-char (point-min))
     ,@body))

;;; Rendering Tests

(ert-deftest md-tui-preview-test-render-string-success ()
  "A successful glow run returns its rendered stdout."
  (cl-letf (((symbol-function 'call-process-region)
             (lambda (start end _program _delete _buffer _display &rest _args)
               (delete-region start end)
               (insert "\e[93mHi\e[0m")
               0)))
    (should (string= (md-tui-preview--render-string "# Hi") "\e[93mHi\e[0m"))))

(ert-deftest md-tui-preview-test-render-string-failure ()
  "A non-zero glow exit signals `user-error' with the process output."
  (md-tui-preview-test-with-failing-glow
    (should-error (md-tui-preview--render-string "# Hi") :type 'user-error)))

(ert-deftest md-tui-preview-test-render-string-uses-configured-args ()
  "The configured `md-tui-preview-glow-args' are forwarded to glow, with
the stdin marker appended."
  (let ((md-tui-preview-glow-args '("--style" "light"))
        (captured-args nil))
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (start end _program _delete _buffer _display &rest args)
                 (setq captured-args args)
                 (delete-region start end)
                 0)))
      (md-tui-preview--render-string "# Hi")
      (should (equal captured-args '("--style" "light" "-"))))))

(ert-deftest md-tui-preview-test-render-string-passes-width ()
  "A non-nil WIDTH is forwarded to glow as `--width', before the stdin
marker."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (start end _program _delete _buffer _display &rest args)
                 (setq captured-args args)
                 (delete-region start end)
                 0)))
      (md-tui-preview--render-string "# Hi" 100)
      (should (equal captured-args
                     (append md-tui-preview-glow-args '("--width" "100" "-")))))))

(ert-deftest md-tui-preview-test-render-string-omits-width-when-nil ()
  "A nil WIDTH does not add a `--width' flag."
  (let ((captured-args nil))
    (cl-letf (((symbol-function 'call-process-region)
               (lambda (start end _program _delete _buffer _display &rest args)
                 (setq captured-args args)
                 (delete-region start end)
                 0)))
      (md-tui-preview--render-string "# Hi")
      (should-not (member "--width" captured-args)))))

;;; Bundled Style Tests

(ert-deftest md-tui-preview-test-style-file-exists ()
  "The bundled glow style file ships with the package and is readable."
  (should (file-readable-p md-tui-preview--style-file)))

(ert-deftest md-tui-preview-test-default-glow-args-reference-style-file ()
  "The default `md-tui-preview-glow-args' point `--style' at the bundled
style file."
  (let ((tail (member "--style" md-tui-preview-glow-args)))
    (should tail)
    (should (string= (cadr tail) md-tui-preview--style-file))))

(ert-deftest md-tui-preview-test-style-file-strips-layout-artifacts ()
  "AC-0060: the bundled style regularizes glow's decorative layout at
the source -- no left margin, no document top/bottom blank line, and
the H1 gets a `# ' marker prefix (consistent with h2-h6) with no
banner padding suffix.  This guards the override mechanism (a
regeneration that dropped the overrides would fail here); the rendered
visual outcome depends on the real glow binary and is not batch
observable, mirroring the theme-color tests above."
  (let* ((style (json-parse-string
                 (with-temp-buffer
                   (insert-file-contents md-tui-preview--style-file)
                   (buffer-string))
                 :object-type 'alist))
         (document (alist-get 'document style))
         (h1 (alist-get 'h1 style)))
    (should (= (alist-get 'margin document) 0))
    (should (string= (alist-get 'block_prefix document) ""))
    (should (string= (alist-get 'block_suffix document) ""))
    (should (string= (alist-get 'prefix h1) "# "))
    (should (string= (alist-get 'suffix h1) ""))))

;;; Width Tests

(ert-deftest md-tui-preview-test-effective-width-without-line-numbers ()
  "Without `display-line-numbers-mode', the effective width is just the
window's body width."
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80)))
    (let ((display-line-numbers-mode nil))
      (should (= (md-tui-preview--effective-width) 80)))))

(ert-deftest md-tui-preview-test-effective-width-subtracts-line-number-gutter ()
  "With `display-line-numbers-mode' on, the gutter width (plus its 2
padding columns) is subtracted from the window's body width, since
`window-body-width' does not do this itself."
  (cl-letf (((symbol-function 'window-body-width) (lambda (&rest _) 80))
            ((symbol-function 'line-number-display-width) (lambda (&rest _) 4.0)))
    (let ((display-line-numbers-mode t))
      (should (= (md-tui-preview--effective-width) 74)))))

(ert-deftest md-tui-preview-test-width-measured-before-erase ()
  "AC-0025-0030: the render width is measured while the buffer still
holds the Markdown source (so a line-number gutter is sized for the
document), not after `erase-buffer' when the buffer is empty.  A stub
records the buffer size at measurement time; a non-zero size means the
source was still present."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "line1\nline2\nline3\nline4\nline5"
      (let (size-at-measure)
        (cl-letf (((symbol-function 'md-tui-preview--effective-width)
                   (lambda () (setq size-at-measure (buffer-size)) 80)))
          (md-tui-preview-toggle))
        (should (> size-at-measure 0))))))

;;; Theme Color Mapping Tests

(ert-deftest md-tui-preview-test-theme-ansi-foregrounds-vector-source ()
  "Red/green/yellow/blue/magenta/cyan foregrounds come from the matching
slot in `ansi-color-names-vector'."
  (md-tui-preview-test-with-ansi-color-names
      ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#ff00ff" "#00ffff" "#ffffff"]
    (let ((colors (md-tui-preview--theme-ansi-foregrounds)))
      (should (equal (alist-get 'ansi-color-red colors) "#ff0000"))
      (should (equal (alist-get 'ansi-color-green colors) "#00ff00"))
      (should (equal (alist-get 'ansi-color-yellow colors) "#ffff00"))
      (should (equal (alist-get 'ansi-color-blue colors) "#0000ff"))
      (should (equal (alist-get 'ansi-color-magenta colors) "#ff00ff"))
      (should (equal (alist-get 'ansi-color-cyan colors) "#00ffff")))))

(ert-deftest md-tui-preview-test-theme-ansi-foregrounds-black-white-from-default ()
  "Black and white foregrounds come from the `default' face, not from
`ansi-color-names-vector' (whose \"white\" slot many themes reserve for
a dim/muted shade)."
  (md-tui-preview-test-with-ansi-color-names
      ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#ff00ff" "#00ffff" "#585858"]
    (let ((colors (md-tui-preview--theme-ansi-foregrounds)))
      (should (equal (alist-get 'ansi-color-black colors)
                     (face-attribute 'default :background nil t)))
      (should (equal (alist-get 'ansi-color-white colors)
                     (face-attribute 'default :foreground nil t)))
      (should-not (equal (alist-get 'ansi-color-white colors) "#585858")))))

(ert-deftest md-tui-preview-test-theme-ansi-foregrounds-bright-matches-base ()
  "Bright variants reuse the same color as their base counterpart."
  (md-tui-preview-test-with-ansi-color-names
      ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#ff00ff" "#00ffff" "#ffffff"]
    (let ((colors (md-tui-preview--theme-ansi-foregrounds)))
      (should (equal (alist-get 'ansi-color-bright-red colors)
                     (alist-get 'ansi-color-red colors))))))

(ert-deftest md-tui-preview-test-with-theme-ansi-colors-overrides-and-restores ()
  "Foregrounds follow the theme and backgrounds follow the buffer's own
background for the duration of the thunk; both are restored after."
  (md-tui-preview-test-with-ansi-color-names
      ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#ff00ff" "#00ffff" "#ffffff"]
    (let ((original-fg (face-attribute 'ansi-color-red :foreground nil t))
          (original-bg (face-attribute 'ansi-color-red :background nil t))
          (seen-fg nil)
          (seen-bg nil))
      (md-tui-preview--with-theme-ansi-colors
       (lambda ()
         (setq seen-fg (face-attribute 'ansi-color-red :foreground nil t))
         (setq seen-bg (face-attribute 'ansi-color-red :background nil t))))
      (should (equal seen-fg "#ff0000"))
      (should (equal seen-bg (face-attribute 'default :background nil t)))
      (should (equal (face-attribute 'ansi-color-red :foreground nil t) original-fg))
      (should (equal (face-attribute 'ansi-color-red :background nil t) original-bg)))))

(ert-deftest md-tui-preview-test-with-theme-ansi-colors-neutralizes-heading-background ()
  "The slot Glow's H1 background uses (bright blue) is overridden to the
buffer's own background, not a theme color.  This checks the face
override mechanism, not the rendered visual outcome (no banner bar),
which batch Emacs cannot observe."
  (md-tui-preview-test-with-ansi-color-names
      ["#000000" "#ff0000" "#00ff00" "#ffff00" "#0000ff" "#ff00ff" "#00ffff" "#ffffff"]
    (let (seen-bg)
      (md-tui-preview--with-theme-ansi-colors
       (lambda ()
         (setq seen-bg (face-attribute 'ansi-color-bright-blue :background nil t))))
      (should (equal seen-bg (face-attribute 'default :background nil t)))
      (should-not (equal seen-bg "#0000ff")))))

(ert-deftest md-tui-preview-test-with-theme-ansi-colors-restores-on-error ()
  "Faces are restored even when the thunk signals an error."
  (let ((original-fg (face-attribute 'ansi-color-red :foreground nil t)))
    (should-error (md-tui-preview--with-theme-ansi-colors (lambda () (error "boom"))))
    (should (equal (face-attribute 'ansi-color-red :foreground nil t) original-fg))))

;;; Toggle Tests

(ert-deftest md-tui-preview-test-toggle-enters-read-only-preview ()
  "Toggling from markdown-mode enters `md-tui-preview-mode', read-only,
with the rendered (ANSI-stripped) content."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "# Hello"
      (md-tui-preview-toggle)
      (should (derived-mode-p 'md-tui-preview-mode))
      (should buffer-read-only)
      (should (string= (buffer-string) "# Hello"))
      (should-not (string-match-p "\e" (buffer-string))))))

(ert-deftest md-tui-preview-test-toggle-roundtrip-restores-content ()
  "Toggling back from the preview restores the exact original text,
major mode, and modified flag."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "# Hello\n\nWorld"
      (md-tui-preview-toggle)
      (md-tui-preview-toggle)
      (should (derived-mode-p 'markdown-mode))
      (should-not buffer-read-only)
      (should (string= (buffer-string) "# Hello\n\nWorld"))
      (should-not (buffer-modified-p)))))

(ert-deftest md-tui-preview-test-toggle-roundtrip-preserves-unsaved-edits ()
  "Unsaved edits made before entering the preview survive the round
trip, and the buffer is still marked modified afterward."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "one\ntwo"
      (goto-char (point-max))
      (insert "\nthree")
      (should (buffer-modified-p))
      (md-tui-preview-toggle)
      (md-tui-preview-toggle)
      (should (string= (buffer-string) "one\ntwo\nthree"))
      (should (buffer-modified-p)))))

(ert-deftest md-tui-preview-test-toggle-does-not-mark-clean-buffer-modified ()
  "Entering and leaving the preview on an unmodified buffer leaves it
unmodified."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "clean"
      (should-not (buffer-modified-p))
      (md-tui-preview-toggle)
      (should-not (buffer-modified-p))
      (md-tui-preview-toggle)
      (should-not (buffer-modified-p)))))

(ert-deftest md-tui-preview-test-toggle-restores-point-position ()
  "After returning from the preview, point returns to where it was
before entering."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "one\ntwo\nthree"
      (goto-char (point-min))
      (forward-line 1)
      (let ((pos (point)))
        (md-tui-preview-toggle)
        (md-tui-preview-toggle)
        (should (= (point) pos))))))

(ert-deftest md-tui-preview-test-toggle-clears-and-restores-file-name ()
  "Entering the preview clears variable `buffer-file-name' (so
`save-buffer' cannot silently overwrite the real file with rendered
text); leaving it restores the original value."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "# Hello"
      (setq buffer-file-name "/tmp/fake.md")
      (md-tui-preview-toggle)
      (should-not buffer-file-name)
      (md-tui-preview-toggle)
      (should (string= buffer-file-name "/tmp/fake.md")))))

(ert-deftest md-tui-preview-test-toggle-save-buffer-does-not-overwrite-source ()
  "AC-0010-0070: `save-buffer' in the preview cannot silently overwrite
the real source file; with `buffer-file-name' cleared, it is redirected
to whatever `read-file-name' returns instead.
The buffer must actually be modified for `save-buffer' to attempt a
write at all (see `basic-save-buffer'), so this leaves an unsaved edit
in place before entering the preview, matching AC-0010-0040."
  (md-tui-preview-test-with-mock-glow
    (let* ((source-file (make-temp-file "md-tui-preview-test-" nil ".md"))
           (original-content "# Hello\n")
           (redirect-file (make-temp-file "md-tui-preview-test-redirect-" nil ".md")))
      (unwind-protect
          (progn
            (with-temp-file source-file (insert original-content))
            (with-current-buffer (find-file-noselect source-file)
              (unwind-protect
                  (progn
                    (markdown-mode)
                    (goto-char (point-max))
                    (insert "unsaved edit\n")
                    (should (buffer-modified-p))
                    (md-tui-preview-toggle)
                    (should (buffer-modified-p))
                    (cl-letf (((symbol-function 'read-file-name)
                               (lambda (&rest _) redirect-file))
                              ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
                      (call-interactively 'save-buffer))
                    (should (string= (with-temp-buffer
                                       (insert-file-contents source-file)
                                       (buffer-string))
                                     original-content)))
                (kill-buffer))))
        (delete-file source-file)
        (delete-file redirect-file)))))

(ert-deftest md-tui-preview-test-toggle-clears-and-restores-auto-save-file-name ()
  "Entering the preview also clears variable `buffer-auto-save-file-name'
\(an idle auto-save would otherwise write the rendered text into the
source file's auto-save file); leaving it restores the original value."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-markdown-buffer "# Hello"
      (setq buffer-auto-save-file-name "/tmp/#fake.md#")
      (md-tui-preview-toggle)
      (should-not buffer-auto-save-file-name)
      (md-tui-preview-toggle)
      (should (string= buffer-auto-save-file-name "/tmp/#fake.md#")))))

(ert-deftest md-tui-preview-test-toggle-preserves-gfm-mode ()
  "AC-0010-0030: a `gfm-mode' buffer (used for README files) round-trips
back to `gfm-mode', not a downgraded plain `markdown-mode'."
  (md-tui-preview-test-with-mock-glow
    (md-tui-preview-test-with-mode-buffer #'gfm-mode "# Hello"
      (md-tui-preview-toggle)
      (md-tui-preview-toggle)
      (should (eq major-mode 'gfm-mode)))))

(ert-deftest md-tui-preview-test-toggle-recovers-after-render-failure ()
  "If glow fails while entering the preview, `md-tui-preview-mode's body
has already run (it captures `md-tui-preview--source-major-mode' before
`md-tui-preview--finish-setup' can fail), leaving the buffer stuck in
`md-tui-preview-mode' but with valid `--source-*' state.  Toggling
again must recover to `markdown-mode', not crash."
  (md-tui-preview-test-with-markdown-buffer "# Hello"
    (md-tui-preview-test-with-failing-glow
      (should-error (md-tui-preview-toggle) :type 'user-error))
    (should (derived-mode-p 'md-tui-preview-mode))
    (md-tui-preview-toggle)
    (should (derived-mode-p 'markdown-mode))
    (should-not buffer-read-only)
    (should (string= (buffer-string) "# Hello"))))

(ert-deftest md-tui-preview-test-toggle-recovers-gfm-mode-after-render-failure ()
  "The same recovery path preserves `gfm-mode' rather than downgrading
to plain `markdown-mode' -- `md-tui-preview--source-major-mode' is
captured from the real original mode before the failure, not defaulted
after the fact."
  (md-tui-preview-test-with-mode-buffer #'gfm-mode "# Hello"
    (md-tui-preview-test-with-failing-glow
      (should-error (md-tui-preview-toggle) :type 'user-error))
    (should (derived-mode-p 'md-tui-preview-mode))
    (md-tui-preview-toggle)
    (should (eq major-mode 'gfm-mode))))

(ert-deftest md-tui-preview-test-toggle-rejects-non-markdown-buffer ()
  "Calling the toggle outside `markdown-mode' and outside the preview
signals `user-error' instead of rendering an unrelated buffer, and
leaves the buffer's mode and content untouched."
  (with-temp-buffer
    (fundamental-mode)
    (insert "not markdown")
    (should-error (md-tui-preview-toggle) :type 'user-error)
    (should (eq major-mode 'fundamental-mode))
    (should (string= (buffer-string) "not markdown"))))

;;; Link Parsing Tests

(ert-deftest md-tui-preview-test-parse-links-inline ()
  "An inline link yields its label and target, classified as `url'."
  (should (equal (md-tui-preview--parse-links "[text](https://example.com)")
                 '((:label "text" :target "https://example.com" :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-autolink ()
  "An autolink yields a nil label and its target, classified as `url'."
  (should (equal (md-tui-preview--parse-links "<https://example.com>")
                 '((:label nil :target "https://example.com" :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-reference-resolves-definition ()
  "A reference-style link resolves to its definition's target."
  (should (equal (md-tui-preview--parse-links
                  "[text][1]\n\n[1]: https://example.com")
                 '((:label "text" :target "https://example.com" :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-excludes-images ()
  "Image syntax (leading \"!\"), inline or reference-style, is not a
navigable link."
  (should-not (md-tui-preview--parse-links "![alt](https://example.com/img.png)"))
  (should-not (md-tui-preview--parse-links
               "![alt][1]\n\n[1]: https://example.com/img.png")))

(ert-deftest md-tui-preview-test-parse-links-excludes-dangling-reference ()
  "A reference id with no matching definition is excluded."
  (should-not (md-tui-preview--parse-links "[text][missing]")))

(ert-deftest md-tui-preview-test-parse-links-excludes-collapsed-reference ()
  "The collapsed reference form \"[text][]\" is excluded."
  (should-not (md-tui-preview--parse-links "[text][]\n\n[text]: https://example.com")))

(ert-deftest md-tui-preview-test-parse-links-excludes-nested-bracket-label ()
  "A label containing a nested, unescaped \"[\" is a known limitation
\(SPEC.md US-0050): the malformed construct matches nothing, rather
than being parsed as some other link."
  (should-not (md-tui-preview--parse-links "[a[b]c](https://example.com)")))

(ert-deftest md-tui-preview-test-parse-links-excludes-shortcut-reference ()
  "The shortcut reference form \"[text]\" (no explicit \"[ref]\") is
excluded, per SPEC.md US-0050's declared out-of-scope forms."
  (should-not (md-tui-preview--parse-links "[text]\n\n[text]: https://example.com")))

(ert-deftest md-tui-preview-test-parse-links-excludes-unsupported-scheme ()
  "An unsupported scheme (e.g. \"javascript:\") is excluded."
  (should-not (md-tui-preview--parse-links "[text](javascript:alert(1))")))

(ert-deftest md-tui-preview-test-parse-links-duplicate-labels-preserve-order ()
  "Two links sharing the same label are both returned, each with its
own target, in source order."
  (should (equal (md-tui-preview--parse-links
                  "[same](https://example.com/a) [same](https://example.com/b)")
                 '((:label "same" :target "https://example.com/a" :kind url)
                   (:label "same" :target "https://example.com/b" :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-mailto-kind ()
  "A mailto target is classified as `url', not `file'."
  (should (equal (md-tui-preview--parse-links "[mail](mailto:a@b.com)")
                 '((:label "mail" :target "mailto:a@b.com" :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-relative-and-absolute-path-kind ()
  "Relative and absolute paths are both classified as `file'."
  (should (equal (md-tui-preview--parse-links "[rel](a/b.md) [abs](/a/b.md)")
                 '((:label "rel" :target "a/b.md" :kind file)
                   (:label "abs" :target "/a/b.md" :kind file)))))

(ert-deftest md-tui-preview-test-parse-links-reference-style-file-kind ()
  "A reference-style link resolving to a relative path is classified as
`file', not `url' -- AC-0050-0020's reference-style example."
  (should (equal (md-tui-preview--parse-links
                  "[text][1]\n\n[1]: relative/file.md")
                 '((:label "text" :target "relative/file.md" :kind file)))))

(ert-deftest md-tui-preview-test-parse-links-inline-target-nested-parens ()
  "An inline target containing balanced parentheses (e.g. a Wikipedia-
style URL) is captured whole, not truncated at the first \")\"."
  (should (equal (md-tui-preview--parse-links
                  "[wiki](https://en.wikipedia.org/wiki/Foo_(bar))")
                 '((:label "wiki"
                    :target "https://en.wikipedia.org/wiki/Foo_(bar)"
                    :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-inline-target-strips-title ()
  "An inline link's optional trailing title is not part of the target."
  (should (equal (md-tui-preview--parse-links "[t](https://example.com \"a title\")")
                 '((:label "t" :target "https://example.com" :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-excludes-heading-fragment ()
  "A bare \"#heading\", a \"file.md#heading\", and a \"file://\"-prefixed
target with a heading fragment are all excluded, per SPEC.md US-0050's
declared out-of-scope forms -- the fragment check applies regardless of
whether the target also happens to use the file:// scheme."
  (should-not (md-tui-preview--parse-links "[text](#heading)"))
  (should-not (md-tui-preview--parse-links "[text](file.md#heading)"))
  (should-not (md-tui-preview--parse-links "[text](file:///a/file.md#heading)")))

(ert-deftest md-tui-preview-test-parse-links-skips-unbalanced-paren ()
  "An inline link with an unbalanced opening parenthesis in its target
is silently skipped -- not treated as a link -- without aborting the
rest of the document: a later, well-formed link is still found."
  (should (equal (md-tui-preview--parse-links
                  "[bad](https://example.com/( [good](https://example.com/b)")
                 '((:label "good" :target "https://example.com/b" :kind url)))))

(ert-deftest md-tui-preview-test-parse-links-reference-definition-strips-angle-brackets ()
  "A reference definition whose target is wrapped in angle brackets
(\"[id]: <url>\", permitted by CommonMark to allow whitespace) resolves
to the unwrapped target."
  (should (equal (md-tui-preview--parse-links "[text][1]\n\n[1]: <https://example.com>")
                 '((:label "text" :target "https://example.com" :kind url)))))

;;; Attach Link Properties Tests

(ert-deftest md-tui-preview-test-attach-marks-label-and-target ()
  "Both the label text and the target text get the property, so RET
works whether point is on the label or on the visible target."
  (with-temp-buffer
    (insert "click here https://example.com/a")
    (md-tui-preview--attach-link-properties
     '((:label "click here" :target "https://example.com/a" :kind url)) nil)
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'md-tui-preview-link-target)
                   '(url . "https://example.com/a")))
    (goto-char (point-max))
    (should (equal (get-text-property (1- (point)) 'md-tui-preview-link-target)
                   '(url . "https://example.com/a")))))

(ert-deftest md-tui-preview-test-attach-duplicate-labels-do-not-cross-match ()
  "Two links sharing the same label each get their own target, matched
positionally in order, not confused with each other."
  (with-temp-buffer
    (insert "click here https://example.com/a and click here https://example.com/b")
    (md-tui-preview--attach-link-properties
     '((:label "click here" :target "https://example.com/a" :kind url)
       (:label "click here" :target "https://example.com/b" :kind url))
     nil)
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'md-tui-preview-link-target)
                   '(url . "https://example.com/a")))
    (goto-char (point-max))
    (should (equal (get-text-property (1- (point)) 'md-tui-preview-link-target)
                   '(url . "https://example.com/b")))))

(ert-deftest md-tui-preview-test-attach-skips-unmatched-link-without-affecting-others ()
  "A link whose label cannot be found in the buffer is silently left
unclickable, and does not prevent a later link from being attached."
  (with-temp-buffer
    (insert "click here https://example.com/b")
    (md-tui-preview--attach-link-properties
     '((:label "missing label" :target "https://example.com/a" :kind url)
       (:label "click here" :target "https://example.com/b" :kind url))
     nil)
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'md-tui-preview-link-target)
                   '(url . "https://example.com/b")))))

(ert-deftest md-tui-preview-test-attach-tolerates-wrapped-label ()
  "A label split across a line wrap by re-wrapping is still found."
  (with-temp-buffer
    (insert "this is a\nwrapped label https://example.com/c")
    (md-tui-preview--attach-link-properties
     '((:label "this is a wrapped label" :target "https://example.com/c" :kind url))
     nil)
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'md-tui-preview-link-target)
                   '(url . "https://example.com/c")))))

(ert-deftest md-tui-preview-test-attach-autolink-has-no-label-to-match ()
  "An autolink (nil label) attaches the property straight to its target
text, without requiring a separate label match first."
  (with-temp-buffer
    (insert "https://example.com/i")
    (md-tui-preview--attach-link-properties
     '((:label nil :target "https://example.com/i" :kind url)) nil)
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'md-tui-preview-link-target)
                   '(url . "https://example.com/i")))))

(ert-deftest md-tui-preview-test-attach-resolves-relative-file-against-source-dir ()
  "A `file' target's relative path is resolved against SOURCE-FILE-NAME's
directory, and any \"file://\" prefix is stripped."
  (with-temp-buffer
    (insert "relfile relative/file.md")
    (md-tui-preview--attach-link-properties
     '((:label "relfile" :target "relative/file.md" :kind file))
     "/tmp/fake/source.md")
    (goto-char (point-min))
    (should (equal (get-text-property (point) 'md-tui-preview-link-target)
                   '(file . "/tmp/fake/relative/file.md")))))

;;; Follow Link At Point Tests

(ert-deftest md-tui-preview-test-follow-link-opens-url ()
  "Point on a `url' link calls `browse-url' with its target."
  (with-temp-buffer
    (insert "x")
    (put-text-property (point-min) (point-max) 'md-tui-preview-link-target
                        '(url . "https://example.com"))
    (goto-char (point-min))
    (let (called-with)
      (cl-letf (((symbol-function 'browse-url)
                 (lambda (url) (setq called-with url))))
        (md-tui-preview-follow-link-at-point))
      (should (string= called-with "https://example.com")))))

(ert-deftest md-tui-preview-test-follow-link-opens-existing-file ()
  "Point on a `file' link whose target exists calls `find-file' with it."
  (let ((file (make-temp-file "md-tui-preview-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert "x")
          (put-text-property (point-min) (point-max) 'md-tui-preview-link-target
                              (cons 'file file))
          (goto-char (point-min))
          (let (called-with)
            (cl-letf (((symbol-function 'find-file)
                       (lambda (f) (setq called-with f))))
              (md-tui-preview-follow-link-at-point))
            (should (string= called-with file))))
      (delete-file file))))

(ert-deftest md-tui-preview-test-follow-link-rejects-point-off-link ()
  "Point not on any link signals `user-error' and calls neither
`browse-url' nor `find-file'."
  (with-temp-buffer
    (insert "plain text")
    (goto-char (point-min))
    (let (browse-url-called find-file-called)
      (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) (setq browse-url-called t)))
                ((symbol-function 'find-file) (lambda (&rest _) (setq find-file-called t))))
        (should-error (md-tui-preview-follow-link-at-point) :type 'user-error))
      (should-not browse-url-called)
      (should-not find-file-called))))

(ert-deftest md-tui-preview-test-follow-link-rejects-missing-file-target ()
  "A `file' link whose target does not exist signals `user-error' and
does not call `find-file' (which would otherwise create an empty
file)."
  (with-temp-buffer
    (insert "x")
    (put-text-property (point-min) (point-max) 'md-tui-preview-link-target
                        (cons 'file "/nonexistent/md-tui-preview-test-file.md"))
    (goto-char (point-min))
    (let (find-file-called)
      (cl-letf (((symbol-function 'find-file) (lambda (&rest _) (setq find-file-called t))))
        (should-error (md-tui-preview-follow-link-at-point) :type 'user-error))
      (should-not find-file-called))))

(ert-deftest md-tui-preview-test-follow-link-rejects-image-target ()
  "AC-0050-0050: point on an image's rendered target text signals
`user-error' through the actual RET entry point, not merely as an
inference from `md-tui-preview--parse-links' excluding images.  The
buffer text mimics glow's rendering of \"![alt](url)\" (label, then the
target rendered as plain visible text) to show the target text being
present in the buffer is not enough to make it navigable."
  (with-temp-buffer
    (insert "Image: alt icon https://example.com/img.png")
    (md-tui-preview--attach-link-properties
     (md-tui-preview--parse-links "![alt](https://example.com/img.png)") nil)
    (goto-char (point-max))
    (backward-char 3)
    (let (browse-url-called find-file-called)
      (cl-letf (((symbol-function 'browse-url) (lambda (&rest _) (setq browse-url-called t)))
                ((symbol-function 'find-file) (lambda (&rest _) (setq find-file-called t))))
        (should-error (md-tui-preview-follow-link-at-point) :type 'user-error))
      (should-not browse-url-called)
      (should-not find-file-called))))

;;; Code Block Tests

(ert-deftest md-tui-preview-test-parse-code-blocks-basic ()
  "A fenced block yields its non-blank content lines in order."
  (should (equal (md-tui-preview--parse-code-blocks "```python\ndef f():\n    return 1\n```")
                 '((:lines ("def f():" "    return 1"))))))

(ert-deftest md-tui-preview-test-parse-code-blocks-single-line ()
  "A block with a single non-blank content line yields a one-element list."
  (should (equal (md-tui-preview--parse-code-blocks "```\nsolo\n```")
                 '((:lines ("solo"))))))

(ert-deftest md-tui-preview-test-parse-code-blocks-skips-blank-edges ()
  "Blank content lines are dropped, keeping only the non-blank ones."
  (should (equal (md-tui-preview--parse-code-blocks "```\n\na\n\nb\n\n```")
                 '((:lines ("a" "b"))))))

(ert-deftest md-tui-preview-test-parse-code-blocks-tilde-fence ()
  "Tilde fences are recognized like backtick fences."
  (should (equal (md-tui-preview--parse-code-blocks "~~~\ncode\n~~~")
                 '((:lines ("code"))))))

(ert-deftest md-tui-preview-test-parse-code-blocks-ignores-inner-shorter-fence ()
  "A shorter run of the fence character inside the block does not close it."
  (should (equal (md-tui-preview--parse-code-blocks "````\n```\ninner\n````")
                 '((:lines ("```" "inner"))))))

(ert-deftest md-tui-preview-test-parse-code-blocks-unclosed-at-eof ()
  "A block left unclosed at end of text is still reported."
  (should (equal (md-tui-preview--parse-code-blocks "```\nx\ny")
                 '((:lines ("x" "y"))))))

(ert-deftest md-tui-preview-test-parse-code-blocks-empty-block-omitted ()
  "A block with no non-blank content line is omitted."
  (should-not (md-tui-preview--parse-code-blocks "```\n\n```")))

(ert-deftest md-tui-preview-test-color-dark-p ()
  "Luminance below the midpoint is dark, above is light."
  (should (md-tui-preview--color-dark-p "#000000"))
  (should-not (md-tui-preview--color-dark-p "#ffffff")))

(ert-deftest md-tui-preview-test-code-block-background-resolves ()
  "`auto' derives a color, a color string passes through, nil disables."
  (let ((md-tui-preview-code-block-background nil))
    (should-not (md-tui-preview--code-block-background)))
  (let ((md-tui-preview-code-block-background "#123456"))
    (should (string= (md-tui-preview--code-block-background) "#123456")))
  ;; `auto' reads `default's background; stub it to a parseable color so
  ;; the derivation is deterministic without a real display.
  (let ((md-tui-preview-code-block-background 'auto))
    (cl-letf (((symbol-function 'face-attribute) (lambda (&rest _) "#1c1c1c")))
      (let ((derived (md-tui-preview--code-block-background)))
        (should (stringp derived))
        (should-not (string= derived "#1c1c1c"))))))

(ert-deftest md-tui-preview-test-attach-code-block-overlays-region ()
  "AC-0070-0010/0020: the located block span gets a tagged overlay that
carries only a `:background', leaving foregrounds to the text's own
faces.  The rendered visual outcome (background shows, syntax colors
survive on top) resolves only at redisplay and is not batch
observable; this asserts the overlay mechanism, mirroring the
theme-color tests above."
  (let ((md-tui-preview-code-block-background "#222222"))
    (with-temp-buffer
      (insert "prose\ndef f():\n    return 1\nmore prose")
      (md-tui-preview--attach-code-block-backgrounds
       '((:lines ("def f():" "    return 1"))))
      (let ((ovs (seq-filter (lambda (o) (overlay-get o 'md-tui-preview-code-block))
                             (overlays-in (point-min) (point-max)))))
        (should (= (length ovs) 1))
        (let ((ov (car ovs)))
          ;; The overlay carries an anonymous face that sets only the
          ;; resolved background and no foreground, so glow's
          ;; syntax-highlight foregrounds show through.  glow pads each
          ;; line to the full render width, so the covered space cells
          ;; already carry the background to the window edge -- no
          ;; `:extend' needed.
          (should (equal (overlay-get ov 'face) '(:background "#222222")))
          ;; Span covers the two code lines through the trailing newline
          ;; (so the last line extends too), stopping before the next
          ;; prose line.
          (should (string= (buffer-substring-no-properties
                            (overlay-start ov) (overlay-end ov))
                           "def f():\n    return 1\n")))))))

(ert-deftest md-tui-preview-test-attach-code-block-disabled-adds-no-overlay ()
  "With the background disabled (nil), no overlay is created."
  (let ((md-tui-preview-code-block-background nil))
    (with-temp-buffer
      (insert "def f():\n    return 1")
      (md-tui-preview--attach-code-block-backgrounds
       '((:lines ("def f():" "    return 1"))))
      (should-not (seq-filter (lambda (o) (overlay-get o 'md-tui-preview-code-block))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest md-tui-preview-test-attach-code-block-single-line-overlays-that-line ()
  "A single-line block overlays exactly its one line."
  (let ((md-tui-preview-code-block-background "#222222"))
    (with-temp-buffer
      (insert "prose\nsolo-code-line\nmore prose")
      (md-tui-preview--attach-code-block-backgrounds
       '((:lines ("solo-code-line"))))
      (let ((ovs (seq-filter (lambda (o) (overlay-get o 'md-tui-preview-code-block))
                             (overlays-in (point-min) (point-max)))))
        (should (= (length ovs) 1))
        (should (string= (buffer-substring-no-properties
                          (overlay-start (car ovs)) (overlay-end (car ovs)))
                         "solo-code-line\n"))))))

(ert-deftest md-tui-preview-test-attach-code-block-skips-when-a-line-missing ()
  "A block with a content line that cannot be located gets no overlay,
rather than shading only the lines found before it (AC-0070-0010
promises the full block)."
  (let ((md-tui-preview-code-block-background "#222222"))
    (with-temp-buffer
      (insert "def f():\n    return 1")
      (md-tui-preview--attach-code-block-backgrounds
       '((:lines ("def f():" "no such trailing line"))))
      (should-not (seq-filter (lambda (o) (overlay-get o 'md-tui-preview-code-block))
                              (overlays-in (point-min) (point-max)))))))

(ert-deftest md-tui-preview-test-attach-code-block-duplicate-last-line-spans-full-block ()
  "AC-0070-0010: when a block's last content line is identical to an
earlier one, the overlay reaches the block's real end (the second
occurrence), not the first.  Matching lines one after another advances
past the earlier copy; a two-point first/last search would instead stop
the span at the earlier occurrence, shading only the block's top."
  (let ((md-tui-preview-code-block-background "#222222"))
    (with-temp-buffer
      (insert "prose\nhead-a\nshared-tail\nhead-b\nshared-tail\nmore prose")
      (md-tui-preview--attach-code-block-backgrounds
       '((:lines ("head-a" "shared-tail" "head-b" "shared-tail"))))
      (let ((ovs (seq-filter (lambda (o) (overlay-get o 'md-tui-preview-code-block))
                             (overlays-in (point-min) (point-max)))))
        (should (= (length ovs) 1))
        (should (string= (buffer-substring-no-properties
                          (overlay-start (car ovs)) (overlay-end (car ovs)))
                         "head-a\nshared-tail\nhead-b\nshared-tail\n"))))))

;;; Keybinding Tests

(ert-deftest md-tui-preview-test-command-map-binding ()
  "`C-c C-c g' in markdown-mode resolves to the toggle command."
  (should (eq (lookup-key markdown-mode-command-map (kbd "g")) #'md-tui-preview-toggle)))

(ert-deftest md-tui-preview-test-preview-map-binding ()
  "`C-c C-c' inside the preview resolves to the toggle command."
  (should (eq (lookup-key md-tui-preview-mode-map (kbd "C-c C-c")) #'md-tui-preview-toggle)))

(ert-deftest md-tui-preview-test-preview-map-ret-binding ()
  "RET inside the preview resolves to the link-follow command."
  (should (eq (lookup-key md-tui-preview-mode-map (kbd "RET"))
              #'md-tui-preview-follow-link-at-point)))

(provide 'md-tui-preview-test)
;;; md-tui-preview-test.el ends here
