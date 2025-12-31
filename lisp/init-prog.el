;;; init-prog.el --- Programming languages support -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package display-fill-column-indicator
  :disabled t
  :ensure nil
  :hook ((prog-mode yaml-mode) . display-fill-column-indicator-mode)
  :init
  (setq-default fill-column 120)
  (setq-default display-fill-column-indicator-character ?\N{U+2506})
  :custom-face
  (fill-column-indicator ((t (:inherit shadow :foreground "dimgray")))))

(use-package breadcrumb
  :hook ((prog-mode yaml-mode) . breadcrumb-mode))

(use-package flymake
  :ensure nil
  :bind (:map flymake-mode-map
          ("C-c m" . consult-flymake)
          ("C-c M" . flymake-show-project-diagnostics)
          ("M-n" . flymake-goto-next-error)
          ("M-p" . flymake-goto-prev-error))
  :hook (prog-mode . flymake-mode))

(use-package flymake-easy)

(use-package xref
  :ensure nil
  :bind
  (("M-]" . xref-find-references))
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
  :disabled t
  :defines (citre-peek-keymap)
  :init
  (require 'citre-config)
  :bind (:map prog-mode-map
          ("C-c c j" . citre-jump)
          ("C-c c p" . citre-peek)
          ("C-c c u" . citre-update-this-tags-file)
          :map citre-peek-keymap
          ("C-g" . citre-peek-abort)
          ("C-n" . citre-peek-next-tag)
          ("C-p" . citre-peek-prev-tag))
  :custom
  (citre-auto-enable-citre-mode-modes '(prog-mode))
  (citre-enable-capf-integration nil))

(use-package quickrun
  :commands quickrun
  :custom
  (quickrun-timeout-seconds 15))

(use-package bats-mode)
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

(use-package ansi-color
  :ensure nil
  :hook (compilation-filter . ansi-color-compilation-filter))

(use-package hideshow
  :ensure nil
  :preface
  (defun hs-display-code-line-counts (ov)
    "Display line counts when hiding codes."
    (when (eq 'code (overlay-get ov 'hs))
      (overlay-put ov 'display
        (concat
          " "
          (propertize "..." 'face 'shadow)
          (propertize
            (format " %d lines"
              (count-lines (overlay-start ov)
                (overlay-end ov)))
            'face '(:inherit shadow :height 0.8))
          " "))))
  :hook (prog-mode . hs-minor-mode)
  :bind (:map hs-minor-mode-map
          ("C-c TAB" . hs-toggle-hiding)
          ("C-c h h" . hs-hide-all)
          ("C-c h s" . hs-show-all))
  :custom
  (hs-set-up-overlay #'hs-display-code-line-counts)
  (hs-hide-comments-when-hiding-all t))

(use-package indent-bars
  :hook ((python-mode yaml-mode) . indent-bars-mode)
  :custom
  (indent-bars-color '(highlight :face-bg t :blend 0.3))
  (indent-bars-highlight-current-depth '(:pattern "." :blend 0.6))
  (indent-bars-display-on-blank-lines 'least)
  (indent-bars-no-descend-lists t))

(use-package csv-mode
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-invisibility-default nil))

(provide 'init-prog)
;;; init-prog.el ends here
