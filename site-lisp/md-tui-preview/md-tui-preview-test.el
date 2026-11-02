;;; md-tui-preview-test.el --- Embedded browser preview tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Covers the URL boundary, mdv process contract, style, and resource ownership.
;;; Code:
(require 'ert)
(require 'cl-lib)
(require 'url)
(require 'md-tui-preview)

(defvar evil-buffer-regexps)

(ert-deftest md-tui-preview-test-ac-0010-0020-cleanup-after-terminal-error ()
  "AC-0010-0020: terminal failure cannot skip owned resource cleanup."
  (save-window-excursion
  (let* ((source (generate-new-buffer " *cleanup-source*"))
         (buffer (generate-new-buffer " *cleanup-test*"))
         (directory (make-temp-file "browser-cleanup-" t))
         closed
         (state (make-my/tb-browser :buffer buffer :directory directory
                                    :origin source :window (selected-window) :point 4
                                    :frame 'test-frame :image 12
                                    :on-close (lambda () (setq closed t)))))
    (with-current-buffer source (insert "source"))
    (set-window-buffer (selected-window) buffer)
    (cl-letf (((symbol-function 'frame-live-p) (lambda (_) t))
              ((symbol-function 'send-string-to-terminal)
               (lambda (&rest _) (error "Terminal unavailable"))))
      (should-error (my/tb-stop state)))
    (should closed)
    (should (eq (window-buffer) source))
    (should (= (window-point) 4))
    (should-not (buffer-live-p buffer))
    (should-not (file-exists-p directory))
    (kill-buffer source))))

(ert-deftest md-tui-preview-test-ac-0020-0020-font-escaping ()
  "AC-0020-0020: CSS font names preserve quotes and backslashes."
  (should (equal (my/md-css-font "a\"b") "\"a\\\"b\""))
  (should (equal (my/md-css-font "a\\b") "\"a\\\\b\"")))

(ert-deftest md-tui-preview-test-ac-0010-0010-reject-unsaved-source ()
  "AC-0010-0010: a source without a saved .md file cannot start a preview."
  (with-temp-buffer
    (markdown-mode)
    (insert "# unsaved")
    (should-error (md-tui-preview-toggle) :type 'user-error)
    (should (buffer-modified-p)))
  (let ((file (make-temp-file "md-preview-unsaved-" nil ".md")))
    (unwind-protect
        (with-temp-buffer
          (markdown-mode)
          (setq buffer-file-name file)
          (insert "# changed")
          (should-error (md-tui-preview-toggle) :type 'user-error)
          (should (buffer-modified-p)))
      (delete-file file))))

(ert-deftest md-tui-preview-test-ac-0010-0010-source-preserved ()
  "AC-0010-0010: opening a saved file never mutates its source buffer."
  (let ((file (make-temp-file "md-preview-source-" nil ".md")) captured)
    (unwind-protect
        (with-temp-buffer
          (markdown-mode)
          (setq buffer-file-name file)
          (insert "# source 日本語\n")
          (goto-char 4)
          (set-buffer-modified-p nil)
          (let ((before (list (buffer-string) buffer-undo-list (point)
                              buffer-file-name major-mode (buffer-modified-p))))
            (cl-letf (((symbol-function 'my/md-color)
                       (lambda (face attribute _frame fallback)
                         (cond ((and (eq face 'default) (eq attribute :foreground)) "#eeeeee")
                               ((and (eq face 'default) (eq attribute :background)) "#111111")
                               (t fallback))))
                      ((symbol-function 'my/md-start)
                       (lambda (state)
                         (setf (my/md-preview-url state) "http://127.0.0.1:4649/source.md"
                               (my/md-preview-process state)
                               (make-pipe-process :name "md-preview-test-server" :noquery t))))
                      ((symbol-function 'my/terminal-browser-open-url)
                       (lambda (url mode on-close)
                         (setq captured (list url mode on-close))
                         (make-my/tb-browser :buffer (current-buffer)))))
              (md-tui-preview-toggle)
              (should (equal (list (buffer-string) buffer-undo-list (point)
                                   buffer-file-name major-mode (buffer-modified-p)) before))
              (should (equal (car captured) "http://127.0.0.1:4649/source.md"))
              (should (eq (cadr captured) #'md-tui-preview-mode))
              (funcall (caddr captured)))))
      (delete-file file))))

(ert-deftest md-tui-preview-test-ac-0010-0020-owned-browser-cleanup ()
  "AC-0010-0020: browser exit restores only its displayed origin."
  (save-window-excursion
    (let* ((source (generate-new-buffer " *browser-origin*") )
           (preview (generate-new-buffer " *browser-preview*"))
           (directory (make-temp-file "browser-test-" t))
           (closed 0)
           (state (make-my/tb-browser :origin source :point 4 :buffer preview
                                      :window (selected-window) :directory directory
                                      :on-close (lambda () (setq closed (1+ closed))))))
      (unwind-protect
          (progn
            (with-current-buffer source (insert "origin") (goto-char 4))
            (set-window-buffer (selected-window) preview)
            (my/tb-stop state)
            (should (eq (window-buffer) source))
            (should (= (window-point) 4))
            (should (= closed 1))
            (should-not (buffer-live-p preview))
            (should-not (file-exists-p directory))
            (my/tb-stop state)
            (should (= closed 1)))
        (when (buffer-live-p source) (kill-buffer source))))))

(ert-deftest md-tui-preview-test-ac-0010-0020-terminal-ownership ()
  "AC-0010-0020: replacing one terminal's preview leaves others running."
  (let* ((same (make-my/tb-browser :frame 'same-terminal))
         (other (make-my/tb-browser :frame 'other-terminal))
         (my/tb-browsers (list same other)) stopped)
    (cl-letf (((symbol-function 'frame-terminal) #'identity)
              ((symbol-function 'frame-live-p) (lambda (_frame) t))
              ((symbol-function 'my/tb-stop)
               (lambda (state)
                 (setq my/tb-browsers (delq state my/tb-browsers))
                 (push state stopped))))
      (my/tb-stop-terminal-preview 'same-terminal))
    (should (equal stopped (list same)))
    (should (equal my/tb-browsers (list other)))))

(ert-deftest md-tui-preview-test-ac-0020-0020-stylesheet ()
  "AC-0020-0020: CSS follows configured font and Emacs face colors."
  (let* ((directory (make-temp-file "md-preview-css-" t))
         (css (expand-file-name "preview.css" directory))
         (state (make-my/md-preview :directory directory :css css))
         (my/md-preview-font-family "Maple Mono Normal NL NF CN")
         (my/md-preview-font-size 12))
    (unwind-protect
        (progn
          (cl-letf (((symbol-function 'my/md-color)
                     (lambda (face attribute _frame fallback)
                       (cond ((and (eq face 'default) (eq attribute :foreground)) "#eeeeee")
                             ((and (eq face 'default) (eq attribute :background)) "#111111")
                             ((eq face 'markdown-header-face-1) "#aabbcc")
                             (t fallback)))))
            (my/md-write-css state (selected-frame)))
          (with-temp-buffer
            (insert-file-contents css)
            (let ((contents (buffer-string)))
              (should (string-match-p "display: none !important" contents))
              (should (string-match-p "--preview-font-family: \"Maple Mono Normal NL NF CN\"" contents))
              (should (string-match-p "--preview-font-size: 12pt" contents))
              (should (string-match-p "text-decoration: underline" contents))
              (should (string-match-p "--preview-foreground: #eeeeee" contents))
              (should (string-match-p "--preview-heading-1: #aabbcc" contents)))))
      (my/md-stop state))))

(ert-deftest md-tui-preview-test-ac-0040-0020-port-from-stderr ()
  "AC-0040-0020: fragmented stderr yields the actual mdv port."
  (let ((source (generate-new-buffer " *mdv-unicode-source*"))
        (state (make-my/md-preview)))
    (unwind-protect
        (progn
          (with-current-buffer source (setq buffer-file-name "/tmp/测试 文档.md"))
          (setf (my/md-preview-file state) "/tmp/测试 文档.md")
          (my/md-receive-stderr state "mdv: http://127.0.0.1:46")
          (should-not (my/md-preview-url state))
          (my/md-receive-stderr state "51/测试 文档.md  (Ctrl-C to stop)\n")
          (should (equal (my/md-preview-url state)
                         (concat "http://127.0.0.1:4651/"
                                 (url-hexify-string "测试 文档.md")))))
      (kill-buffer source))))

(ert-deftest md-tui-preview-test-ac-0040-0020-mdv-serves-saved-file ()
  "AC-0020-0010, AC-0040-0010, AC-0040-0020: real mdv serves a saved file."
  (skip-unless (executable-find "mdv"))
  (let* ((directory (make-temp-file "mdv-live-test-" t))
         (file (expand-file-name "source.md" directory))
         (linked (expand-file-name "linked.md" directory))
         (resource-directory (make-temp-file "mdv-live-css-" t))
         (css (expand-file-name "preview.css" resource-directory))
         (state (make-my/md-preview :file file :directory resource-directory
                                    :css css :stdout (generate-new-buffer " *mdv-live-stdout*"))))
    (unwind-protect
        (progn
          (with-temp-file file (insert "# Main\n\n[Linked](linked.md)\n\n```go\nfunc main() {}\n```\n\n```mermaid\ngraph LR; A-->B\n```\n"))
          (with-temp-file linked (insert "# Linked\n"))
          (cl-letf (((symbol-function 'my/md-color)
                     (lambda (_face attribute _frame _fallback)
                       (if (eq attribute :background) "#111111" "#eeeeee"))))
            (my/md-write-css state (selected-frame)))
          (my/md-start state)
          (should (process-live-p (my/md-preview-process state)))
          (should (member "--no-open" (process-command (my/md-preview-process state))))
          (should (member css (process-command (my/md-preview-process state))))
          (should (string-match-p "\\`http://127\\.0\\.0\\.1:[0-9]+/source\\.md\\'"
                                  (my/md-preview-url state)))
          (let ((url-proxy-services nil)
                (url-request-extra-headers '(("Cache-Control" . "no-cache"))))
            (with-current-buffer (url-retrieve-synchronously (my/md-preview-url state) t t 5)
              (goto-char (point-min))
              (should (search-forward "__mdv/custom.css" nil t))
              (kill-buffer (current-buffer)))
            (with-current-buffer (url-retrieve-synchronously
                                  (replace-regexp-in-string "/source.md\\'" "/__mdv/custom.css"
                                                            (my/md-preview-url state)) t t 5)
              (goto-char (point-min))
              (should (search-forward "--preview-font-family:" nil t))
              (should (search-forward "display: none !important" nil t))
              (should (search-forward "text-decoration: underline" nil t))
              (kill-buffer (current-buffer)))
            (with-current-buffer (url-retrieve-synchronously
                                  (replace-regexp-in-string "/source.md\\'" "/linked.md"
                                                            (my/md-preview-url state)) t t 5)
              (goto-char (point-min))
              (should (search-forward "HTTP/1.1 200" nil t))
              (kill-buffer (current-buffer)))
            (with-current-buffer (url-retrieve-synchronously
                                  (replace-regexp-in-string "/source.md\\'" "/linked.markdown"
                                                            (my/md-preview-url state)) t t 5)
              (goto-char (point-min))
              (should (search-forward "HTTP/1.1 415" nil t))
              (kill-buffer (current-buffer)))
            (with-current-buffer (url-retrieve-synchronously
                                  (replace-regexp-in-string "/source.md\\'"
                                                            "/__mdv/fragment?path=source.md"
                                                            (my/md-preview-url state)) t t 5)
              (goto-char (point-min))
              (should (search-forward "hasMermaid" nil t))
              (goto-char (point-min))
              (should (search-forward "mermaid" nil t))
              (goto-char (point-min))
              (should (search-forward "chroma" nil t))
              (kill-buffer (current-buffer)))))
      (my/md-stop state)
      (should-not (process-live-p (my/md-preview-process state)))
      (when (file-directory-p directory) (delete-directory directory t))
      (should-not (file-exists-p resource-directory)))))

(ert-deftest md-tui-preview-test-ac-0030-0010-generic-url-boundary ()
  "AC-0030-0010: generic browser receives only a URL and an optional mode."
  (let ((process-environment (append '("TMUX=/tmp/test" "TMUX_PANE=%1") process-environment))
        (url "http://127.0.0.1:4651/page.md") captured state)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil))
              ((symbol-function 'display-color-cells) (lambda (&optional _) #x1000000))
              ((symbol-function 'my/tb-terminal-cell) (lambda (&rest _) [8 16]))
              ((symbol-function 'my/md-start) (lambda (&rest _) (ert-fail "generic layer started mdv")))
              ((symbol-function 'my/tb-start) (lambda (_state target &optional _mode) (setq captured target))))
      (setq state (my/terminal-browser-open-url url)))
    (unwind-protect
        (progn
          (should (equal captured url))
          (should (eq (my/tb-browser-origin state) (current-buffer)))
          (should (null (my/tb-browser-on-close state))))
      (cl-letf (((symbol-function 'send-string-to-terminal) #'ignore))
        (my/tb-stop state)))))

(ert-deftest md-tui-preview-test-ac-0030-0010-direct-terminal ()
  "AC-0030-0010: outside tmux, graphics go to the terminal unwrapped."
  (let ((process-environment (cl-remove-if (lambda (entry) (string-prefix-p "TMUX" entry))
                                           process-environment))
        probed state)
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) nil))
              ((symbol-function 'display-color-cells) (lambda (&optional _) #x1000000))
              ((symbol-function 'my/tb-terminal-cell)
               (lambda (tmux _image) (setq probed (list tmux)) [8 16]))
              ((symbol-function 'my/tb-start) #'ignore))
      (setq state (my/terminal-browser-open-url "http://127.0.0.1:4652/page.md")))
    (unwind-protect
        (progn
          (should (equal probed '(nil)))
          (should (null (my/tb-browser-tmux state)))
          (should (equal (my/tb-terminal-sequence "\e_Ga=d\e\\" (my/tb-browser-tmux state))
                         "\e_Ga=d\e\\")))
      (cl-letf (((symbol-function 'send-string-to-terminal) #'ignore))
        (my/tb-stop state)))))

(ert-deftest md-tui-preview-test-ac-0030-0020-browser-protocol ()
  "AC-0030-0020: graphics relay forwards Kitty commands but discards title OSC."
  (let ((state (make-my/tb-browser :image 12)) sent)
    (cl-letf (((symbol-function 'send-string-to-terminal)
               (lambda (bytes &rest _) (push bytes sent))))
      (my/tb-graphics state "\e]0;title\a\e_Ga=T;AAAA\e\\"))
    (should (equal sent '("\e_Ga=T;AAAA\e\\")))))

(ert-deftest md-tui-preview-test-ac-0030-0020-navigation-bindings ()
  "AC-0030-0020: Markdown keymap forwards navigation, but generic mode does not."
  (should (eq (lookup-key md-tui-preview-mode-map (kbd "h")) #'my/tb-browser-back))
  (should (eq (lookup-key md-tui-preview-mode-map (kbd "l")) #'my/tb-browser-forward))
  (should (eq (lookup-key md-tui-preview-mode-map (kbd "q")) #'md-tui-preview-toggle))
  (should-not (lookup-key terminal-browser-embed-mode-map (kbd "h"))))

(provide 'md-tui-preview-test)
;;; md-tui-preview-test.el ends here
