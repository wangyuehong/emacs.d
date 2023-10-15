;;; init-lsp.el --- lsp by eglot. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package eglot
  :preface
  ;; (defun my/eglot-organize-imports () (interactive)
  ;;        (eglot-code-actions nil nil "source.organizeImports" t))
  (defun my/eglot-setup-hooks () (interactive)
         ;; (add-hook 'before-save-hook 'my/eglot-organize-imports nil t)
         (add-hook 'before-save-hook 'eglot-format-buffer nil t))
  (defun my/eglot-capf ()
    (setq-local my/completion-functions (list #'yasnippet-capf
                                          #'cape-dabbrev
                                          #'cape-abbrev
                                          #'cape-file))
    (when (fboundp #'tabnine-completion-at-point)
      (setq-local my/completion-functions (cons #'tabnine-completion-at-point
                                            my/completion-functions)))
    (setq-local completion-at-point-functions
      (list (apply #'cape-super-capf (cons #'eglot-completion-at-point
                                       my/completion-functions)))))
  :bind (:map eglot-mode-map
              ("C-c l a" . eglot-code-actions)
              ("C-c l b" . flymake-show-buffer-diagnostics)
              ("C-c l e" . eglot-rename)
              ("C-c l p" . flymake-show-project-diagnostics)
              ("C-c l r" . eglot-reconnect))
  :hook ((go-mode . eglot-ensure)
         (eglot-managed-mode . my/eglot-setup-hooks)
         (eglot-managed-mode . my/eglot-capf))
  :config
  (use-package consult-eglot
    :bind (:map eglot-mode-map
                ("C-c l l" . consult-eglot-symbols)))
  (setq-default eglot-workspace-configuration
                '((:gopls . ((completeUnimported . t)
                             (gofumpt            . t)
                             (usePlaceholders    . t)))))

  :init
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster)
  :custom
  (eglot-confirm-server-initiated-edits nil)
  (eldoc-echo-area-use-multiline-p nil)
  (eglot-events-buffer-size 0))


(provide 'init-lsp)

;;; init-lsp.el ends here
