;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual)
   :prefix ";"
   "a"  'rg-menu
   "r"  'my/replace-at-point-or-region)
  (general-define-key
   :states '(normal visual)
   :keymaps 'override
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "!"  'shell-command
   "a"  'rg-project
   "A"  'rg
   "c"  'evilnc-comment-or-uncomment-lines
   "d"  'fanyi-dwim2
   "e"  'evil-iedit-state/iedit-mode
   "f"  'flymake-show-buffer-diagnostics
   "g"  'magit-status
   "h"  'symbol-overlay-put
   "i"  'helm-imenu
   "j"  'helm-bookmarks
   "k"  'kill-buffer-and-window
   "K"  'kill-other-buffers
   "l"  'helm-browse-project
   "o"  'helm-occur
   "p"  'package-upgrade-all
   "q"  'quickrun
   "r"  'helm-show-kill-ring
   "t"  'git-timemachine
   "v"  'diff-hl-hydra/body
   "w"  'save-buffer
   "W"  'save-some-buffers
   "x"  'helm-M-x))
(provide 'init-general)
