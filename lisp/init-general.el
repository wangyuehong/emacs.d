;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "!"  'shell-command
   "-"  'evil-window-split
   "|"  'evil-window-vsplit
   ","  'diff-hl-hydra/body
   "a"  'rg-project
   "A"  'rg
   "b"  'helm-mini
   "c"  'evilnc-comment-or-uncomment-lines
   "d"  'osx-dictionary-search-word-at-point
   "e"  'evil-iedit-state/iedit-mode
   "f"  'flycheck-list-errors
   "g"  'magit-status
   "h"  'symbol-overlay-put
   "i"  'helm-imenu
   "j"  'helm-bookmarks
   "k"  'kill-buffer-and-window
   "K"  'kill-other-buffers
   "l"  'helm-browse-project
   "O"  'helm-occur
   "o"  'evil-window-next
   "p"  'paradox-upgrade-packages
   "q"  'quickrun
   "r"  'helm-show-kill-ring
   "t"  'git-timemachine
   "w"  'save-buffer
   "W"  'save-some-buffers
   "x"  'helm-M-x
   )
)
(provide 'init-general)
