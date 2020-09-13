;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package lsp-mode
  :diminish
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root t)
  (lsp-client-packages '(lsp-go))
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-snippet nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-flycheck-live-reporting nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-keep-workspace-alive nil)
  (lsp-modeline-code-actions-enable nil)
  (lsp-modeline-diagnostics-enable nil)
  (lsp-restart 'auto-restart)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 2)
  (lsp-eldoc-enable-hover nil)

  :config
  (with-eval-after-load 'company
    (defun my-lsp-company-hook ()
      (progn
        (push '(company-capf :with company-yasnippet :with company-dabbrev) company-backends)))
    (add-hook 'lsp-after-open-hook 'my-lsp-company-hook)))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  )

(use-package lsp-ivy :after lsp-mode)

(provide 'init-lsp)
