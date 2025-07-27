;;; init-im.el --- Input Method configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; (use-package mozc
;;   :if (executable-find "mozc_emacs_helper")
;;   :custom
;;   (mozc-candidate-style 'echo-area)
;;   (default-input-method "japanese-mozc"))

;; (use-package popup)
;; (use-package pyim
;;   :bind (("M-i" . pyim-toggle-input-ascii)
;;           :map pyim-mode-map
;;           ("C-f" . pyim-next-page)
;;           ("C-b" . pyim-previous-page))
;;   :custom
;;   (pyim-default-scheme 'quanpin)
;;   (pyim-page-length 6)
;;   (pyim-page-tooltip 'popup)
;;   :config
;;   (require 'popup)
;;   (use-package pyim-basedict)
;;   (setq-default pyim-punctuation-translate-p '(no)) ;; 默认使用半角标点符号
;;   (pyim-basedict-enable))

;; (use-package im-bridge
;;   :ensure nil ;; site-lisp/im-bridge
;;   :hook
;;   ((after-init . imb-evil-mode)))

;; ;; Input method switching functions and keybindings
;; (defun my/set-input-method-pyim ()
;;   "Set default input method to pyim and activate it."
;;   (interactive)
;;   (setq default-input-method "pyim")
;;   (set-input-method "pyim")
;;   (message "Input method set to: pyim"))

;; (defun my/set-input-method-mozc ()
;;   "Set default input method to japanese-mozc and activate it."
;;   (interactive)
;;   (setq default-input-method "japanese-mozc")
;;   (set-input-method "japanese-mozc")
;;   (message "Input method set to: japanese-mozc"))

;; (global-set-key (kbd "C-c i j") #'my/set-input-method-mozc)
;; (global-set-key (kbd "C-c i i") #'my/set-input-method-pyim)

(provide 'init-im)
;;; init-im.el ends here
