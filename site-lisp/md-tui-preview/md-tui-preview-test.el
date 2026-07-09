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

;;; Keybinding Tests

(ert-deftest md-tui-preview-test-command-map-binding ()
  "`C-c C-c g' in markdown-mode resolves to the toggle command."
  (should (eq (lookup-key markdown-mode-command-map (kbd "g")) #'md-tui-preview-toggle)))

(ert-deftest md-tui-preview-test-preview-map-binding ()
  "`C-c C-c' inside the preview resolves to the toggle command."
  (should (eq (lookup-key md-tui-preview-mode-map (kbd "C-c C-c")) #'md-tui-preview-toggle)))

(provide 'md-tui-preview-test)
;;; md-tui-preview-test.el ends here
