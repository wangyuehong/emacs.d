;;; init-general.el --- config general. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'transient)
(require 'code-ref)

(transient-define-prefix my/copy-path-menu ()
  "Copy file path in various formats."
  [["Path Formats"
    ("a" "Absolute path" cref-copy-buffer-absolute-path)
    ("c" "Git relative" cref-copy-buffer-git-path)
    ("n" "File name only" cref-copy-buffer-file-name)]
   ["Quick Actions"
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix my/copy-region-content-menu ()
  "Copy region with location and content."
  [["With Content"
    ("a" "Absolute path" cref-copy-region-with-absolute-location)
    ("r" "Git relative" cref-copy-region-with-git-location)
    ("n" "File name only" cref-copy-region-with-filename-location)]
   ["Quick Actions"
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix my/copy-region-menu ()
  "Copy region location in various formats."
  [["Location Only"
    ("a" "Absolute path" cref-copy-region-location-absolute)
    ("r" "Git relative" cref-copy-region-location-git)
    ("n" "File name only" cref-copy-region-location-filename)]
   ["With Content"
    ("w" "With content ..." my/copy-region-content-menu)]
   ["Quick Actions"
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix my/open-external-menu ()
  "Open file in external program."
  [["Programs"
    ("c" "Cursor" my/open-file-in-cursor)
    ("f" "Finder" my/open-in-finder)
    ("v" "VS Code" my/open-file-in-vscode)
    ("t" "Typora" my/open-file-in-typora)]
   ["Quick Actions"
    ("q" "Quit" transient-quit-one)]])

(transient-define-prefix my/current-buffer-actions ()
  "Current buffer actions."
  [["Actions"
    ("c" "Copy path ..." my/copy-path-menu)
    ("r" "Copy region ..." my/copy-region-menu)
    ("o" "Open in ..." my/open-external-menu)]
   ["Quick Actions"
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
    "d" '(diff-hl-command-map               :wk "diff-hl")
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
