;;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :init
  (setq lsp-auto-guess-root t)
  (setq lsp-prefer-flymake nil)
  )

(use-package company-lsp :commands company-lsp)

(provide 'init-lsp)
;;; init-lsp.el ends here
