;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual)
   :prefix ","
   "a"  'rg-project
   "A"  'rg
   "c"  'evilnc-comment-or-uncomment-lines
   "d"  'delete-trailing-whitespace
   "e"  'evil-iedit-state/iedit-mode
   "w"  'save-buffer
   "W"  'save-some-buffers
   "q"  'quickrun
   "k"  'kill-buffer-and-window
   "K"  'kill-other-buffers
   "P"  'paradox-upgrade-packages
   "r"  'helm-show-kill-ring
   ","  'git-gutter-hydra/body
   "v"  'revert-buffer
   "nn" 'narrow-to-region
   "nd" 'narrow-to-defun
   "nw" 'widen
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual)
   :keymaps 'override
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "h"  'symbol-overlay-put
   "i"  'counsel-imenu
   "g"  'magit-status
   "t"  'git-timemachine
   "f"  'flycheck-list-errors
   "l"  'helm-ls-git-ls
   "s"  'swiper
   "x"  'amx
   "y"  'youdao-dictionary-search-at-point+
   )
)
(provide 'init-general)
