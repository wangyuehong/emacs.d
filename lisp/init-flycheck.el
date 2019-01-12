;;; init-flycheck.el --- flycheck configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :diminish flycheck-mode
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-emacs-lisp-load-path 'inherit)

  ;; Only check while saving and opening files
  (setq flycheck-check-syntax-automatically '(save mode-enabled))

  (use-package avy-flycheck
    :hook (global-flycheck-mode . avy-flycheck-setup)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
