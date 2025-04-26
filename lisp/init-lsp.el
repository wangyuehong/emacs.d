;;; init-lsp.el --- lsp by eglot. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :functions eglot-server-capable
  :preface
  (defun my/eglot-organize-imports ()
    "Check if `source.organizeImports` is available and execute it if possible."
    (let* ((actions (eglot-code-actions nil nil nil nil))
            (organize-imports-action
              (seq-find (lambda (action)
                          (string-prefix-p "source.organizeImports" (plist-get action :kind)))
                actions)))
      (if organize-imports-action
        (eglot-code-actions nil nil "source.organizeImports" t))))

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
  :hook ((eglot-managed-mode . my/eglot-setup-hooks))
  :config
  (setq-default eglot-workspace-configuration
    '((:gopls . ((completeUnimported . t)
                  (gofumpt     . t)
                  (staticcheck . t)))))

  :init
  (setq eglot-stay-out-of '(company))
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eglot-events-buffer-size 0)
  (eglot-extend-to-xref t)
  (eglot-report-progress nil)
  (eglot-sync-connect nil))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
          ("C-c l l" . consult-eglot-symbols))
  :custom
  (consult-eglot-show-kind-name t))


(provide 'init-lsp)
;;; init-lsp.el ends here
