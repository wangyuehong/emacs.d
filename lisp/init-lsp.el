;;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :diminish
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  )

(use-package company-lsp
  :after lsp-mode
  :commands company-lsp
  :init
  (setq company-lsp-cache-candidates t)
  :config
  (push 'company-lsp company-backends)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
