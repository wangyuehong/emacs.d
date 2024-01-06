;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package general
  :demand t
  :bind (:repeat-map my/general-repeat-map
          ("l" . toggle-truncate-lines)
          ("t" . git-timemachine-toggle)
          ("n" . display-line-numbers-mode))
  :config
  (general-evil-setup t)
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix ";"
    "r" 'my/replace-at-point-or-region
    "a" 'embark-act
    ";" 'popper-toggle)
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"
    "!"  'shell-command
    "a"  'rg-project
    "A"  'rg
    "c"  'evilnc-comment-or-uncomment-lines
    "d"  'fanyi-dwim2
    "e"  'evil-iedit-state/iedit-mode
    "f"  'consult-flymake
    "F"  'flymake-show-project-diagnostics
    "g"  'magit-status
    "h"  'symbol-overlay-put
    "i"  'imenu
    "k"  'kill-buffer-and-window
    "K"  'kill-other-buffers
    "l"  'consult-ls-git
    "o"  'consult-line
    "p"  'package-upgrade-all
    "q"  'quickrun
    "t"  '(:which-key "toggles")
    "tl" '(toggle-truncate-lines     :which-key "toggle-truncate-lines")
    "tt" '(git-timemachine-toggle    :which-key "toggle-timemachine")
    "tn" '(display-line-numbers-mode :which-key "toggle-display-line-numbers")
    "r"  'consult-yank-from-kill-ring
    "w"  'save-buffer
    "W"  'save-some-buffers
    "x"  'execute-extended-command)
  )

(provide 'init-general)
