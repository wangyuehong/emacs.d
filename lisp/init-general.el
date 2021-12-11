;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual)
   :prefix ","
   ","  'diff-hl-hydra/body
   "A"  'rg
   "a"  'rg-project
   "c"  'evilnc-comment-or-uncomment-lines
   "d"  'delete-trailing-whitespace
   "e"  'evil-iedit-state/iedit-mode
   "K"  'kill-other-buffers
   "k"  'kill-buffer-and-window
   "q"  'quickrun
   "r"  'helm-show-kill-ring
   "W"  'save-some-buffers
   "w"  'save-buffer
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual emacs)
   :keymaps 'override
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "C"  'calendar
   "c"  'calculator
   "f"  'flycheck-list-errors
   "p"  'paradox-upgrade-packages
   "g"  'magit-status
   "h"  'symbol-overlay-put
   "i"  'helm-imenu
   "l"  'helm-browse-project
   "o"  'helm-occur
   "t"  'git-timemachine
   "x"  'helm-M-x
   "y"  'youdao-dictionary-search-at-point+
   )
)
(provide 'init-general)
