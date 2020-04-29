;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(use-package fullframe
  :config (with-eval-after-load 'ibuffer (fullframe ibuffer ibuffer-quit)))

(use-package ibuffer-vc)

(provide 'init-ibuffer)
