;;; init-prog.el --- Programming languages support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package display-fill-column-indicator
  :disabled t
  :ensure nil
  :hook ((prog-mode yaml-mode protobuf-mode) . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column 120)
  (setq-default display-fill-column-indicator-character ?\N{U+2506})
  :custom-face
  (fill-column-indicator ((t (:inherit shadow :foreground "dimgray")))))

(use-package breadcrumb
  :hook ((prog-mode yaml-mode protobuf-mode) . breadcrumb-mode))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(use-package xref
  :ensure nil
  :bind
  (("C-\\" . xref-find-references))
  :hook
  ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                             (t 'grep)))
  (xref-history-storage 'xref-window-local-history))

(use-package dumb-jump
  :functions dumb-jump-xref-activate
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-selector 'completing-read))

(use-package citre
  :defines (citre-peek-keymap)
  :init
  (require 'citre-config)
  :bind (:map prog-mode-map
          ("C-c c j" . citre-jump)
          ("C-c c p" . citre-peek)
          ("C-c c u" . citre-update-this-tags-file)
          :map citre-peek-keymap
          ("C-n" . citre-peek-next-tag)
          ("C-p" . citre-peek-prev-tag))
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-enable-capf-integration nil))

(use-package quickrun
  :commands quickrun
  :custom
  (quickrun-timeout-seconds 15))

(use-package docker-compose-mode)
(use-package dockerfile-mode)
(use-package js2-mode)
(use-package lua-mode)
(use-package protobuf-mode)
(use-package ruby-mode :ensure nil)
(use-package terraform-mode)
(use-package toml-mode)
(use-package typescript-mode :mode ("\\.ts[x]\\'" . typescript-mode))
(use-package yaml-mode)

(use-package css-mode :ensure nil)

(use-package web-mode
  :mode "\\.\\(php\\|jsp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|vue\\)$")

(use-package sh-script
  :ensure nil
  :mode (("\\.aliases\\'"      . sh-mode)
          ("\\.env\\.example\\'" . sh-mode)
          ("\\.[a-zA-Z]+rc\\'"  . sh-mode)
          ("\\.bash\\'"         . sh-mode)
          ("\\.bash_history\\'" . sh-mode)
          ("\\.bash_profile\\'" . sh-mode)
          ("\\.bashrc.local\\'" . sh-mode)
          ("\\.bashrc\\'"       . sh-mode)
          ("\\.c*sh\\'"         . sh-mode)
          ("\\.cfg\\'"          . sh-mode)
          ("\\.env\\'"          . sh-mode)
          ("\\.gpms\\'"         . sh-mode)
          ("\\.sh\\'"           . sh-mode)
          ("\\.zprofile.local\\'" . sh-mode)
          ("\\.zsh\\'"          . sh-mode)
          ("\\.zshrc.local\\'"  . sh-mode)
          ("crontab.*\\'"       . sh-mode)))

(provide 'init-prog)
;;; init-prog.el ends here
