;;; init-prog.el --- Programming languages support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(use-package xref
  :ensure nil
  :hook ((xref-after-return xref-after-jump) . recenter)
  :custom
  (xref-search-program (cond ((executable-find "rg") 'ripgrep)
                             (t 'grep)))
  (xref-history-storage 'xref-window-local-history)
  (xref-show-xrefs-function #'xref-show-definitions-completing-read)
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package dumb-jump
  :init
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  :custom
  (dumb-jump-quiet t)
  (dumb-jump-aggressive t)
  (dumb-jump-force-searcher 'rg)
  (dumb-jump-selector 'completing-read))

(use-package citre
  :init
  (require 'citre-config)
  :bind (("C-c c j" . citre-jump)
         ("C-c c u" . citre-update-this-tags-file))
  :custom
  (citre-auto-enable-citre-mode-modes '(ruby-mode))
  (citre-enable-capf-integration nil)
  (citre-prompt-language-for-ctags-command t)
  (citre-use-project-root-when-creating-tags t))

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

(use-package css-mode
  :ensure nil
  :config (setq css-indent-offset 2))

(use-package web-mode
  :mode "\\.\\(phtml\\|php\\|[gj]sp\\|as[cp]x\\|erb\\|djhtml\\|html?\\|hbs\\|ejs\\|jade\\|swig\\|tm?pl\\|vue\\)$"
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

(provide 'init-prog)
;;; init-prog.el ends here
