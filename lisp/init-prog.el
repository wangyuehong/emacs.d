;;; init-prog.el --- Programming languages support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package treesit
  :ensure nil
  :init
  (setopt treesit-language-source-alist
    '((toml . ("https://github.com/tree-sitter/tree-sitter-toml"))
      (dockerfile . ("https://github.com/camdencheek/tree-sitter-dockerfile"))
      (lua . ("https://github.com/tree-sitter-grammars/tree-sitter-lua"))
      (ruby . ("https://github.com/tree-sitter/tree-sitter-ruby"))
      (css . ("https://github.com/tree-sitter/tree-sitter-css"))
      (javascript . ("https://github.com/tree-sitter/tree-sitter-javascript"))
      (typescript . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "typescript/src"))
      (tsx . ("https://github.com/tree-sitter/tree-sitter-typescript" nil "tsx/src"))
      (yaml . ("https://github.com/tree-sitter-grammars/tree-sitter-yaml"))
      (bash . ("https://github.com/tree-sitter/tree-sitter-bash"))
      (json . ("https://github.com/tree-sitter/tree-sitter-json"))
      (html . ("https://github.com/tree-sitter/tree-sitter-html"))
      (python . ("https://github.com/tree-sitter/tree-sitter-python"))
      (go . ("https://github.com/tree-sitter/tree-sitter-go"))
      (gomod . ("https://github.com/camdencheek/tree-sitter-go-mod"))))
  (dolist (entry '(("\\.toml\\'" . toml-ts-mode)
                   ("\\(?:Dockerfile\\(?:\\..*\\)?\\|\\.[Dd]ockerfile\\)\\'" . dockerfile-ts-mode)
                   ("\\.lua\\'" . lua-ts-mode)
                   ("\\.ts\\'" . typescript-ts-mode)
                   ("\\.tsx\\'" . tsx-ts-mode)
                   ("\\.ya?ml\\'" . yaml-ts-mode)
                   ("\\.jsx?\\'" . js-ts-mode)
                   ("\\.html?\\'" . html-ts-mode)))
    (add-to-list 'auto-mode-alist entry))
  (dolist (entry '((ruby-mode . ruby-ts-mode)
                   (css-mode . css-ts-mode)
                   (js-json-mode . json-ts-mode)
                   (python-mode . python-ts-mode)
                   (go-mode . go-ts-mode)
                   (go-dot-mod-mode . go-mod-ts-mode)))
    (add-to-list 'major-mode-remap-alist entry)))

(use-package breadcrumb
  :hook ((prog-mode yaml-ts-mode) . breadcrumb-mode))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
          ("C-c m" . consult-flymake)
          ("C-c M" . flymake-show-project-diagnostics)
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(use-package xref
  :ensure nil
  :bind
  (("M-]" . xref-find-references))
  :hook
  ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                         (t 'grep)))
  (xref-history-storage 'xref-window-local-history))

(use-package dumb-jump
  :functions dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-selector 'completing-read))

(use-package quickrun
  :commands quickrun
  :custom
  (quickrun-timeout-seconds 15))

(use-package bats-mode)
(use-package protobuf-mode)
(use-package terraform-mode)

(use-package web-mode
  :mode "\\.\\(php\\|jsp\\|as[cp]x\\|erb\\|djhtml\\|hbs\\|ejs\\|vue\\)$")

(use-package sh-script
  :ensure nil
  :mode (("\\.aliases\\'"      . sh-mode)
          ("\\.env\\.example\\'" . sh-mode)
          ("\\.[a-zA-Z]+rc\\'"  . sh-mode)
          ("\\.c*sh\\'"         . sh-mode)
          ("\\.cfg\\'"          . sh-mode)
          ("\\.env\\'"          . sh-mode)
          ("\\.gpms\\'"         . sh-mode)
          ("\\.sh\\'"           . sh-mode)
          ("\\.zprofile.local\\'" . sh-mode)
          ("\\.zsh\\'"          . sh-mode)
          ("\\.zshrc.local\\'"  . sh-mode)
          ("crontab.*\\'"       . sh-mode)))

;; bash-ts-mode entries must follow sh-script to take priority over
;; the broad \\.[a-zA-Z]+rc pattern in auto-mode-alist.
(dolist (entry '(("\\.bash\\'" . bash-ts-mode)
                ("\\.bash_history\\'" . bash-ts-mode)
                ("\\.bash_profile\\'" . bash-ts-mode)
                ("\\.bashrc\\.local\\'" . bash-ts-mode)
                ("\\.bashrc\\'" . bash-ts-mode)))
  (add-to-list 'auto-mode-alist entry))

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package hideshow
  :ensure nil
  :preface
  (defun my/hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
        (concat
          " "
          (propertize "..." 'face 'shadow)
          (propertize
            (format " %d lines"
              (count-lines (overlay-start ov)
                (overlay-end ov)))
            'face '(:inherit shadow :height 0.8))
          " "))))
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
          ("C-c TAB" . hs-toggle-hiding)
          ("C-c h h" . hs-hide-all)
          ("C-c h s" . hs-show-all))
  :custom
  (hs-set-up-overlay #'my/hs-display-code-line-counts)
  (hs-hide-comments-when-hiding-all t))

(use-package indent-bars
  :hook ((python-ts-mode yaml-ts-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.3))
  (indent-bars-highlight-current-depth '(:pattern "." :blend 0.6))
  (indent-bars-display-on-blank-lines 'least)
  (indent-bars-no-descend-lists t))

(use-package vbnet-mode
  :ensure nil
  :mode "\\.\\(frm\\|bas\\|cls\\|vb\\)\\'"
  :custom
  (vbnet-capitalize-keywords-p nil)
  :init
  (with-eval-after-load 'dumb-jump
    ;; file extensions
    (dolist (ext '("vb" "bas" "cls" "frm"))
      (add-to-list 'dumb-jump-language-file-exts
                   `(:language "vbnet" :ext ,ext :agtype nil :rgtype nil)))
    ;; case-insensitive (defconst -- intentional override, re-apply on upgrade)
    (add-to-list 'dumb-jump--case-insensitive-languages "vbnet")
    ;; find rules
    (dolist (rule
             '((:language "vbnet" :type "function"
                :supports ("ag" "grep" "rg" "git-grep")
                :regex "^\\s*(?:[A-Za-z]+\\s+)*(?:Function|Sub)\\s+JJJ\\s*\\("
                :tests ("Public Function test()"
                        "Private Sub test(param)"
                        "Public Async Function test()"
                        "Protected Friend Overridable Sub test()"
                        "Function test()")
                :not ("test()" "testOther()" "call test()"))
               (:language "vbnet" :type "type"
                :supports ("ag" "grep" "rg" "git-grep")
                :regex "^\\s*(?:[A-Za-z]+\\s+)*(?:Class|Structure|Interface|Module|Enum)\\s+JJJ\\b"
                :tests ("Public Class test"
                        "Public Partial Class test"
                        "Interface test"
                        "Module test"
                        "Public Enum test")
                :not ("testOther" "End Class"))
               (:language "vbnet" :type "type"
                :supports ("ag" "grep" "rg" "git-grep")
                :regex "^\\s*(?:[A-Za-z]+\\s+)*Delegate\\s+(?:Function|Sub)\\s+JJJ\\s*\\("
                :tests ("Public Delegate Function test()"
                        "Public Shadows Delegate Sub test()")
                :not ("testOther"))
               (:language "vbnet" :type "variable"
                :supports ("ag" "grep" "rg" "git-grep")
                :regex "^\\s*(?:[A-Za-z]+\\s+)*Property\\s+JJJ\\b"
                :tests ("Public Property test As String"
                        "Public Overloads Property test As String"
                        "ReadOnly Property test As Integer"
                        "Property test As Boolean")
                :not ("testOther" "End Property"))
               (:language "vbnet" :type "variable"
                :supports ("ag" "grep" "rg" "git-grep")
                :regex "^\\s*(?:[A-Za-z]+\\s+)*(?:Dim|Const)\\s+JJJ\\b"
                :tests ("Dim test As String"
                        "Private Const test As Integer = 1"
                        "Const test = 42")
                :not ("testOther" "ReDim test("))
               (:language "vbnet" :type "variable"
                :supports ("ag" "grep" "rg" "git-grep")
                :regex "^\\s*(?:(?:Public|Private|Protected|Friend)\\s+)+(?:[A-Za-z]+\\s+)*JJJ\\s+As\\b"
                :tests ("Public test As String"
                        "Private Shared test As Integer"
                        "Protected Friend Shared test As Boolean")
                :not ("testOther" "Dim test As"))))
      (add-to-list 'dumb-jump-find-rules rule))))

(use-package csv-mode
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-invisibility-default nil))

(provide 'init-prog)
;;; init-prog.el ends here
