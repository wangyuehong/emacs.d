;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package key-chord :config (key-chord-mode t))

(define-key minibuffer-local-map (kbd "C-h") 'left-char)
(define-key minibuffer-local-map (kbd "C-l") 'right-char)

(use-package which-key
  :diminish
  :init (setq which-key-idle-delay 0.6)
  :hook (after-init . which-key-mode)
  )

(provide 'init-keybind)
