;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package highlight-symbol
  :diminish
  :init
  (setq highlight-symbol-idle-delay 0.5)
  (setq highlight-symbol-colors (quote ("#5c5cff" "#ff0000" "#00ff00" "#ff00ff" "#ffff00")))
  :hook
  ((prog-mode yaml-mode) . highlight-symbol-mode)
  ((prog-mode yaml-mode) . highlight-symbol-nav-mode))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode))

(use-package fic-mode :hook prog-mode)

(use-package rainbow-mode :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-highlight)
