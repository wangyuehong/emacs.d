;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package lsp-mode
  :diminish
  :commands (lsp lsp-deferred)
  :hook ((go-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-auto-guess-root t)
  (lsp-client-packages '(lsp-go))
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
  (lsp-eldoc-enable-hover nil)
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-enable t)
  (lsp-ui-doc-delay 0.2)
  (lsp-ui-doc-include-signature t)
  (lsp-ui-sideline-enable nil)
  )

(use-package dap-mode
  :functions dap-hydra/nil
  :diminish
  :bind (:map lsp-mode-map
              ("<f5>" . dap-debug)
              ("M-<f5>" . dap-hydra))
  :hook ((after-init . dap-auto-configure-mode)
         (dap-stopped . (lambda (_args) (dap-hydra)))
         (dap-terminated . (lambda (_args) (dap-hydra/nil)))
         (go-mode . (lambda () (require 'dap-dlv-go))))
  :init
  (setq dap-auto-configure-features '(sessions locals breakpoints expressions controls)))

(provide 'init-lsp)
