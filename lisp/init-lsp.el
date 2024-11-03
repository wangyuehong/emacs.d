;;; init-lsp.el --- lsp by eglot. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :preface
  (defun my/eglot-organize-imports () (interactive)
         (eglot-code-actions nil nil "source.organizeImports" t))
  (defun my/eglot-setup-hooks () (interactive)
    (add-hook 'before-save-hook 'my/eglot-organize-imports nil t)
    (when (eglot--server-capable :documentFormattingProvider)
      (add-hook 'before-save-hook 'eglot-format-buffer nil t)))
  (defun my/eglot-capf ()
    (add-to-list 'completion-at-point-functions #'cape-dabbrev))

  :bind (:map eglot-mode-map
              ("C-c l t" . eglot-find-typeDefinition)
              ("C-c l i" . eglot-find-implementation)
              ("C-c l a" . eglot-code-actions)
              ("C-c l b" . flymake-show-buffer-diagnostics)
              ("C-c l e" . eglot-rename)
              ("C-c l p" . flymake-show-project-diagnostics)
              ("C-c l r" . eglot-reconnect))
  :hook ((eglot-managed-mode . my/eglot-capf)
          (eglot-managed-mode . my/eglot-setup-hooks))
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
            ("C-c l l" . consult-eglot-symbols))
    :custom
    (consult-eglot-show-kind-name t))
  (setq-default eglot-workspace-configuration
    '((:gopls . ((completeUnimported . t)
                  (staticcheck . t)
                  (gofumpt     . t)
                  (usePlaceholders . t)))))

  :init
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  (setq completion-category-overrides '((eglot (styles orderless flex))))
  :custom
  (eglot-autoshutdown t)
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-events-buffer-size 0))

(provide 'init-lsp)
;;; init-lsp.el ends here
