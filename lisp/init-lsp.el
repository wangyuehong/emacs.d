;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package lsp-mode
  :diminish
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (ruby-mode . lsp-deferred)
         (lsp-mode . (lambda ()
                       (lsp-enable-which-key-integration)
                       (add-hook 'before-save-hook #'lsp-format-buffer t t)
                       (add-hook 'before-save-hook #'lsp-organize-imports t t))))

  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root nil)
  (lsp-client-packages '(lsp-go lsp-solargraph))
  (lsp-enable-file-watchers nil)
  (lsp-enable-folding nil)
  (lsp-enable-indentation nil)
  (lsp-headerline-breadcrumb-enable nil)
  (lsp-enable-links nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-enable-semantic-highlighting nil)
  (lsp-enable-snippet t)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-text-document-color nil)
  (lsp-flycheck-live-reporting nil)
  (lsp-diagnostics-provider :flycheck)
  (lsp-keep-workspace-alive nil)
  (lsp-modeline-code-actions-enable :file)
  (lsp-modeline-diagnostics-scope :workspace)
  (lsp-restart 'auto-restart)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 2)
  (lsp-eldoc-enable-hover nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil))

(provide 'init-lsp)
