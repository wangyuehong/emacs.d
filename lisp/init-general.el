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
   ","  'diff-hl-hydra/body
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual)
   :keymaps 'override
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "C"  'calendar
   "c"  'calculator
   "h"  'symbol-overlay-put
   "i"  'helm-semantic-or-imenu
   "g"  'magit-status
   "t"  'git-timemachine
   "f"  'flycheck-list-errors
   "l"  'helm-ls-git-ls
   "x"  'helm-M-x
   "y"  'youdao-dictionary-search-at-point+
   )
)
(provide 'init-general)
