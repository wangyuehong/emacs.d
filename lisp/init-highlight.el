;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package symbol-overlay
  :diminish
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         :map symbol-overlay-map
         ("c" . symbol-overlay-remove-all)
         )
  :hook ((prog-mode yaml-mode) . symbol-overlay-mode)
  :init
  (setq symbol-overlay-idle-time 0.1)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit highlight bold))))
  (symbol-overlay-face-1 ((t (:background "brightblue" :inverse-video t))))
  (symbol-overlay-face-2 ((t (:background "brightgreen" :inverse-video t))))
  (symbol-overlay-face-3 ((t (:background "brightred" :inverse-video t))))
  (symbol-overlay-face-4 ((t (:background "brightmagenta" :inverse-video t))))
  (symbol-overlay-face-5 ((t (:background "cyan" :inverse-video t))))
  (symbol-overlay-face-6 ((t (:background "brightyellow" :inverse-video t))))
  (symbol-overlay-face-7 ((t (:background "whiteblack" :inverse-video t))))
  (symbol-overlay-face-8 ((t (:background "magenta" :inverse-video t))))
  )

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode))

(use-package fic-mode :hook prog-mode)

(use-package rainbow-mode :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-highlight)
