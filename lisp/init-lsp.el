;;; init-lsp.el --- lsp by eglot. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :functions eglot-server-capable
  :preface
  (defun my/eglot-organize-imports ()
    "Apply gopls organize imports if available.

Calls `eglot-code-actions` with the kind source.organizeImports and
non-interactively applies it when supported by the server."
    (eglot-code-actions nil nil "source.organizeImports" t))

  (defun my/eglot-setup-hooks ()
    (add-hook 'before-save-hook 'my/eglot-organize-imports nil t)
    (when (eglot-server-capable :documentFormattingProvider)
      (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  :bind (:map eglot-mode-map
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l a" . eglot-code-actions)
              ("C-c l e" . eglot-rename)
              ("C-c l r" . eglot-reconnect))

  :hook ((eglot-managed-mode . my/eglot-setup-hooks)
         (eglot-managed-mode . eglot-inlay-hints-mode))

  :init
  (setq eglot-stay-out-of '(company))
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  ;; Set Eglot user options explicitly for robustness
  (setq eglot-autoshutdown t
        eglot-confirm-server-initiated-edits nil
        eglot-events-buffer-size 0
        eglot-extend-to-xref t
        eglot-report-progress nil
        eglot-sync-connect nil)

  :config
  ;; gopls configuration per upstream recommendations
  ;; See: https://tip.golang.org/gopls/editor/emacs
  (setq-default eglot-workspace-configuration
                '((:gopls
                   . ((completeUnimported . t)
                      (gofumpt           . t)
                      (staticcheck       . t)
                      (analyses . ((unusedparams . t)
                                   (unusedwrite  . t)
                                   (nilness      . t)
                                   (shadow       . t)
                                   ;; Suppress selected Staticcheck style checks
                                   (ST1000       . :json-false)
                                   (ST1020       . :json-false)
                                   (ST1021       . :json-false))))))

  ))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
          ("C-c l l" . consult-eglot-symbols))
  :custom
  (consult-eglot-show-kind-name t))

(use-package mason
  :config
  (mason-ensure))

(provide 'init-lsp)
;;; init-lsp.el ends here
