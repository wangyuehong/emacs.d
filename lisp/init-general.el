;;; init-general.el --- config general. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package general
  :functions (general-evil-setup general-define-key)
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"

    "SPC"  '(:wk "ai")
    "SPC g" '(gptel         :wk "gptel")
    "SPC m" '(gptel-menu    :wk "gptel-menu")
    "SPC r" '(gptel-rewrite :wk "gptel-rewrite")

    "A" 'rg
    "a" 'rg-project
    "c" '(evilnc-comment-or-uncomment-lines :wk "comment-or-uncomment")
    "e" '(evil-iedit-state/iedit-mode       :wk "iedit")
    "g" 'magit-status
    "h" 'symbol-overlay-put
    "i" 'imenu
    "l" '(consult-ls-git                    :wk "ls-git")
    "o" '(my/consult-line                   :wk "occur")
    "q" 'quickrun
    "r" '(consult-yank-from-kill-ring       :wk "yank-from-kill-ring")
    "w" 'save-buffer
    "x" '(execute-extended-command          :wk "M-x")

    "t"  '(:wk "toggles")
    "tl" '(toggle-truncate-lines       :wk "toggle-truncate-lines")
    "tm" '(xterm-mouse-mode            :wk "toggle-mouse-mode")
    "tn" '(display-line-numbers-mode   :wk "toggle-display-line-numbers")
    "tp" '(electric-pair-mode          :wk "toggle-electric-pair-mode")
    "tt" '(git-timemachine-toggle      :wk "toggle-git-timemachine")
    ))

(provide 'init-general)
;;; init-general.el ends here
