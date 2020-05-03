;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package lsp-mode
  :diminish
  :commands lsp
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . lsp-find-definition))
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq lsp-auto-guess-root t
        lsp-log-io nil
        lsp-client-packages '(lsp-go)
        lsp-diagnostic-package :none
        lsp-keep-workspace-alive nil
        lsp-enable-indentation nil
        lsp-enable-snippet nil
        lsp-enable-folding nil
        lsp-enable-links nil
        lsp-enable-completion-at-point nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-on-type-formatting nil
        lsp-flycheck-live-reporting nil
        lsp-restart 'auto-restart
        lsp-enable-file-watchers nil
        lsp-gopls-hover-kind "NoDocumentation"
        ))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (:map lsp-mode-map
        ([remap xref-find-references] . lsp-ui-peek-find-references)
        :map lsp-ui-peek-mode-map
        ("j" . lsp-ui-peek--select-next)
        ("k" . lsp-ui-peek--select-prev)
        ("C-j" . lsp-ui-peek--select-next-file)
        ("C-k" . lsp-ui-peek--select-prev-file)
        )
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-eldoc-enable-hover nil ; disable eldoc displays in minibuffer
        ))

(use-package company-lsp
  :commands company-lsp
  :after lsp-mode
  :init
  (setq company-lsp-cache-candidates 'auto)
  :config
  (push '(company-lsp company-yasnippet :with company-dabbrev) company-backends)
  )

(use-package lsp-ivy :after lsp-mode)

(provide 'init-lsp)
