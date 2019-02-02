;;; init-keybind.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package key-chord
  :config
  (key-chord-mode t)
  )

(require 'smartchr)

(use-package move-text
  :bind (("M-k" . move-text-up)
         ("M-j" . move-text-down))
  )

(define-key minibuffer-local-map (kbd "C-h") 'left-char)
(define-key minibuffer-local-map (kbd "C-l") 'right-char)

(use-package which-key
  :hook (after-init . which-key-mode)
  )

(provide 'init-keybind)

;;; init-keybind.el ends here
