;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package lsp-mode
  :diminish
  :commands lsp
  :hook (lsp-mode . lsp-enable-which-key-integration)
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  :custom
  (lsp-keymap-prefix "C-c l")
  (lsp-client-packages '(lsp-go))
  (lsp-auto-guess-root t)
  (lsp-restart 'auto-restart)
  (lsp-idle-delay 0.5)
  (lsp-log-io nil)
  (lsp-enable-folding nil)
  (lsp-enable-links nil)
  (lsp-diagnostic-package :flycheck)
  (lsp-lens-auto-enable t)
  (lsp-flycheck-live-reporting nil)
  (lsp-prefer-capf t)
  (lsp-enable-snippet t)
  (lsp-enable-file-watchers nil)
  (lsp-enable-text-document-color nil)
  (lsp-enable-symbol-highlighting nil)
  (lsp-enable-indentation nil)
  (lsp-enable-on-type-formatting nil)
  (lsp-keep-workspace-alive nil)
  (lsp-eldoc-enable-hover t)
  (lsp-signature-auto-activate t)
  (lsp-signature-doc-lines 2)
  (lsp-gopls-hover-kind "NoDocumentation")

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
