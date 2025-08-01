;;; init-general.el --- config general. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'transient)
(transient-define-prefix my/current-buffer-actions ()
  "Current buffer actions."
  [["Copy"
     ("c" "Copy relative/abs path" my/copy-buffer-path)
     ("C" "Copy absolute path" my/copy-buffer-absolute-path)
     ("n" "Copy file name" my/copy-buffer-file-name)
     ("r" "Copy region with location" my/copy-region-with-location)]
    ["Open in External Program"
      ("oc" "Open in Cursor" my/open-file-in-cursor)
      ("of" "Open in Finder" my/open-in-finder)
      ("ov" "Open in VS Code" my/open-file-in-vscode)
      ("ot" "Open in Typora" my/open-file-in-typora)]
    ["Misc"
      ("q" "Quit" transient-quit-one)]])

(use-package general
  :functions (general-evil-setup general-define-key)
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"

    "SPC" 'claude-code-transient

    ";" 'embark-act

    "a"  '(:wk "ai")
    "aa" '(gptel         :wk "gptel")
    "am" '(gptel-menu    :wk "gptel-menu")
    "aq" '(gptel-quick   :wk "gptel-quick")
    "ar" '(gptel-rewrite :wk "gptel-rewrite")
    "as" '(gptel-send    :wk "gptel-send")
    "at" '(gptel-tools   :wk "gptel-tools")

    "b" '(my/current-buffer-actions         :wk "buffer-actions")
    "c" '(evilnc-comment-or-uncomment-lines :wk "comment-or-uncomment")
    "e" '(evil-iedit-state/iedit-mode       :wk "iedit")
    "g" 'magit-status
    "h" 'symbol-overlay-put
    "i" 'imenu
    "K" '(my/kill-other-buffers       :wk "kill-other-buffers")
    "l" '(consult-ls-git              :wk "ls-git")
    "L" '(consult-ls-git-ls-ignored   :wk "ls-git-ignored")

    "o" '(my/consult-line             :wk "occur")
    "q" 'quickrun
    "r" 'rg-project
    "R" 'rg

    "t"  '(:wk "toggles")
    "tl" '(toggle-truncate-lines     :wk "truncate-lines")
    "tm" '(xterm-mouse-mode          :wk "xterm-mouse-mode")
    "tn" '(display-line-numbers-mode :wk "display-line-numbers")
    "tp" '(electric-pair-mode        :wk "electric-pair-mode")
    "tr" '(read-only-mode            :wk "read-only-mode")
    "tt" '(git-timemachine-toggle    :wk "git-timemachine")

    "w" 'save-buffer
    "x" '(execute-extended-command    :wk "M-x")
    "y" '(consult-yank-from-kill-ring :wk "yank-from-kill-ring")
    ))

(provide 'init-general)
;;; init-general.el ends here
