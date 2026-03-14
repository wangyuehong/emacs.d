;;; init-lsp.el --- lsp by eglot. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Python LSP server setting, used for eglot server selection.
;; Configure per-project via .dir-locals.el in project root:
;;   ((python-ts-mode . ((my/python-lsp-server . ty))))
(defvar-local my/python-lsp-server 'basedpyright
  "Python LSP server. Values: `basedpyright', `ty'.")
(put 'my/python-lsp-server 'safe-local-variable
  (lambda (v) (memq v '(basedpyright ty))))

(use-package eglot
  :functions eglot-server-capable
  :preface
  (defun my/eglot-organize-imports ()
    "Apply gopls organize imports if available.

Calls `eglot-code-actions` with the kind source.organizeImports and
non-interactively applies it when supported by the server."
    (eglot-code-actions nil nil "source.organizeImports" t))

  (defun my/eglot-setup-hooks ()
    (add-hook 'before-save-hook #'my/eglot-organize-imports nil t)
    (when (eglot-server-capable :documentFormattingProvider)
      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

  (defun my/python-lsp-contact (_interactive)
    "Return eglot contact for Python based on `my/python-lsp-server'."
    (pcase my/python-lsp-server
      ('basedpyright '("basedpyright-langserver" "--stdio"))
      ('ty '("ty" "server"))))
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
    eglot-confirm-server-edits nil
    eglot-events-buffer-config '(:size 0)
    eglot-extend-to-xref t
    eglot-report-progress nil
    eglot-sync-connect nil
    eglot-watch-files-outside-project-root nil)

  :config
  (add-to-list 'eglot-server-programs
    '((python-mode python-ts-mode) . my/python-lsp-contact))

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
                          (ST1021       . :json-false))))))))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
          ("C-c l l" . consult-eglot-symbols))
  :custom
  (consult-eglot-show-kind-name t))

(provide 'init-lsp)
;;; init-lsp.el ends here
