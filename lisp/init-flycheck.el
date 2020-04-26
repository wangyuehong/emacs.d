;;; init-flycheck.el --- flycheck configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package flycheck
  :diminish
  :hook (after-init . global-flycheck-mode)
  :config
  (setq flycheck-global-modes
        '(not text-mode outline-mode fundamental-mode org-mode
              diff-mode shell-mode eshell-mode term-mode vterm-mode)
        flycheck-emacs-lisp-load-path 'inherit
        ;; Only check while saving and opening files
        flycheck-check-syntax-automatically '(save mode-enabled)))

(provide 'init-flycheck)
;;; init-flycheck.el ends here
