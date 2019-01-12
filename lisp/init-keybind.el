;;; init-keybind.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(require-package 'key-chord)

(require 'key-chord)
(key-chord-mode t)

(require 'smartchr)

(require-package 'move-text)
(global-set-key (kbd "M-k") 'move-text-up)
(global-set-key (kbd "M-j") 'move-text-down)

(define-key minibuffer-local-map (kbd "C-h") 'left-char)
(define-key minibuffer-local-map (kbd "C-l") 'right-char)

(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)

(provide 'init-keybind)

;;; init-keybind.el ends here
