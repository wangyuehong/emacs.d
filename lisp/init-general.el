;;; init-general.el --- config general. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix ";"
    "a" 'embark-act
    "d" 'fanyi-dwim2
    "r" 'my/replace-at-point-or-region)
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"
    "!"  'shell-command
    "a"  'rg-project
    "A"  'rg
    "c"  'evilnc-comment-or-uncomment-lines
    "d" '(:keymap diff-hl-command-map :package diff-hl :wk "diff-hl")
    "e"  'evil-iedit-state/iedit-mode
    "E"  '(:keymap envrc-command-map :package envrc :wk "direnv")
    "f"  'consult-flymake
    "F"  'flymake-show-project-diagnostics
    "g"  'magit-status
    "h"  'symbol-overlay-put
    "i"  'imenu
    "k"  'kill-buffer-and-window
    "K"  'kill-other-buffers
    "l"  'consult-ls-git
    "o"  'consult-line
    "p"  '(:keymap project-prefix-map :package project :wk "project")
    "P"  'package-upgrade-all
    "q"  'quickrun
    "t"  '(:which-key "toggles")
    "tl" '(toggle-truncate-lines     :which-key "toggle-truncate-lines")
    "tn" '(display-line-numbers-mode :which-key "toggle-display-line-numbers")
    "tp" '(electric-pair-mode        :which-key "electric-pair-mode")
    "r"  'consult-yank-from-kill-ring
    "w"  'save-buffer
    "W"  'save-some-buffers
    "x"  'execute-extended-command)
  )

(provide 'init-general)
;;; init-general.el ends here
