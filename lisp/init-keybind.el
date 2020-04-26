;;; init-keybind.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package key-chord :config (key-chord-mode t))

(require 'smartchr)

(define-key minibuffer-local-map (kbd "C-h") 'left-char)
(define-key minibuffer-local-map (kbd "C-l") 'right-char)

(use-package which-key
  :diminish
  :hook (after-init . which-key-mode)
  )

(provide 'init-keybind)
;;; init-keybind.el ends here
