;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package flycheck
  :diminish
  :hook (prog-mode . flycheck-mode)
  :custom
  (flycheck-temp-prefix ".flycheck")
  (flycheck-check-syntax-automatically '(save mode-enabled))
  (flycheck-emacs-lisp-load-path 'inherit)
  :config
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  )

(provide 'init-flycheck)
