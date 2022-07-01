;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual emacs)
   :keymaps 'override
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "!"  'shell-command
   ","  'diff-hl-hydra/body
   "a"  'rg-project
   "A"  'rg
   "c"  'evilnc-comment-or-uncomment-lines
   "C"  'calculator
   "d"  'delete-trailing-whitespace
   "e"  'evil-iedit-state/iedit-mode
   "f"  'flycheck-list-errors
   "g"  'magit-status
   "h"  'symbol-overlay-put
   "i"  'helm-imenu
   "k"  'kill-buffer-and-window
   "K"  'kill-other-buffers
   "l"  'helm-browse-project
   "o"  'helm-occur
   "p"  'paradox-upgrade-packages
   "q"  'quickrun
   "r"  'helm-show-kill-ring
   "t"  'git-timemachine
   "w"  'save-buffer
   "W"  'save-some-buffers
   "x"  'helm-M-x
   "y"  'youdao-dictionary-search-at-point+
   )
)
(provide 'init-general)
