;;; init-flycheck.el --- flycheck configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :init
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  )

(use-package avy-flycheck
  :after flycheck
  :hook (global-flycheck-mode . avy-flycheck-setup)
  )

(provide 'init-flycheck)
;;; init-flycheck.el ends here
