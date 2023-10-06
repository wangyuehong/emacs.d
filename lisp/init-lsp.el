;;; init-lsp.el --- lsp by eglot. -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

(use-package eglot
  :preface
  (defun my/eglot-organize-imports () (interactive)
         (eglot-code-actions nil nil "source.organizeImports" t))
  (defun my/eglot-setup-hooks () (interactive)
         (add-hook 'before-save-hook 'my/eglot-organize-imports nil t)
         (add-hook 'before-save-hook 'eglot-format-buffer nil t))
  (defun my/eglot-capf ()
    (setq-local completion-at-point-functions
                (list (cape-super-capf
                       #'eglot-completion-at-point
                       ;; #'tabnine-completion-at-point
                       #'yasnippet-capf
                       #'cape-file))))
  :bind (:map eglot-mode-map
              ("C-c l e" . eglot-rename)
              ("C-c l r" . eglot-reconnect)
              ("C-c l a" . eglot-code-actions)
              ("C-c l b" . flymake-show-buffer-diagnostics)
              ("C-c l p" . flymake-show-project-diagnostics))
  :hook ((go-mode . eglot-ensure)
         (eglot-managed-mode . my/eglot-setup-hooks)
         (eglot-managed-mode . my/eglot-capf))
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("C-c l l" . consult-eglot-symbols)))
  (setq-default eglot-workspace-configuration
                '(:gopls (:usePlaceholders t)))

  :custom
  (eglot-events-buffer-size 0)
  (eglot-confirm-server-initiated-edits nil))


(provide 'init-lsp)

;;; init-lsp.el ends here
