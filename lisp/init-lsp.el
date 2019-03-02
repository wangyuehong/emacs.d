;;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :diminish
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  )

(use-package lsp-clients
  :ensure nil
  )

(use-package company-lsp
  :after lsp-mode company
  :bind (("<backtab>" . company-lsp))
  :init
  (setq company-lsp-cache-candidates t)
  :config
  (push '(company-lsp company-dabbrev-code :separate) company-backends)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
