;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package quickrun
  :commands quickrun
  :custom
  (quickrun-timeout-seconds 15))

(use-package yaml-mode)
(use-package csv-mode
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-invisibility-default nil))

(use-package dockerfile-mode)
(use-package docker-compose-mode)
(use-package protobuf-mode)

(use-package json-mode)
(use-package js2-mode)
(use-package typescript-mode
  :config (setq typescript-indent-level 2)
  :mode ("\\.ts[x]\\'" . typescript-mode))

(use-package css-mode
  :ensure nil
  :config (setq css-indent-offset 2))

(use-package web-mode
  :mode "\\.\\(phtml\\|php|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
  :config
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2))

(use-package sh-script
  :ensure nil
  :mode (("\\.alias\\'"        . sh-mode)
         ("\\.gpms\\'"         . sh-mode)
         ("\\.cfg\\'"          . sh-mode)
         ("\\.c*sh\\'"         . sh-mode)
         ("\\.[a-zA-Z]+rc\\'"  . sh-mode)
         ("crontab.*\\'"       . sh-mode)
         ("\\.bash_profile\\'" . sh-mode)
         ("\\.bash_history\\'" . sh-mode)
         ("\\.sh\\'"           . sh-mode)
         ("\\.bash\\'"         . sh-mode)
         ("\\.bashrc.local\\'" . sh-mode)
         ("\\.zsh\\'"          . sh-mode)
         ("\\.bashrc\\'"       . sh-mode)
         ("\\.env\\'"          . sh-mode)
         ("\\.env\\.example\\'" . sh-mode)
         )
  :custom
  (sh-basic-offset 2)
  (sh-indentation 2))

(use-package dockerfile-mode)

(use-package toml-mode)

(use-package terraform-mode)

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-selector 'completing-read))

(use-package makefile-executor
  :config
  (add-hook 'makefile-mode-hook 'makefile-executor-mode))

(provide 'init-prog)
