;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package lsp-mode
  :diminish
  :commands lsp
  :hook ((lsp-mode . lsp-enable-which-key-integration))
  :bind (:map lsp-mode-map
              ([remap xref-find-definitions] . lsp-find-definition)
              ([remap xref-find-references] . lsp-find-references))
  :init
  (setq read-process-output-max (* 1024 1024)) ;; 1MB
  (setq
   lsp-keymap-prefix "C-c l"
   lsp-auto-guess-root t
   lsp-keep-workspace-alive nil
   lsp-enable-indentation nil
   lsp-enable-on-type-formatting nil
   lsp-flycheck-live-reporting nil
   lsp-restart 'auto-restart
   lsp-enable-file-watchers nil
   ))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :bind (("C-c u" . lsp-ui-imenu))
  :init
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-delay 0.2
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-doc-border (face-foreground 'default)
        lsp-eldoc-enable-hover nil ; disable eldoc displays in minibuffer
        ))

(use-package company-lsp
  :commands company-lsp
  :after lsp-mode
  :init
  (setq company-lsp-cache-candidates 'auto)
  :config
  (push '(company-yasnippet company-dabbrev company-lsp :separate) company-backends)
  ;; (push '(company-lsp :with company-yasnippet) company-backends)
  )

(use-package lsp-ivy :after lsp-mode)

(provide 'init-lsp)
