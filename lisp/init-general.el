;;; init-general.el --- config general. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'transient)
(require 'code-ref)

(transient-define-prefix my/copy-path-menu ()
  "Copy file path in various formats."
  [["Path Formats"
    ("a" "Absolute path" cref-copy-buffer-absolute-path)
    ("r" "Git relative" cref-copy-buffer-git-path)
    ("n" "File name only" cref-copy-buffer-file-name)]
   ["Quick Actions"
    ("q" "Quit" transient-quit-one)]])

(defvar my/copy-region-content-mode 'auto
  "Content mode for `my/copy-region-menu': `no', `auto', or `full'.")

(defun my/copy-region--cycle-content (&rest _)
  "Cycle `my/copy-region-content-mode' no -> auto -> full -> no."
  (setq my/copy-region-content-mode
        (pcase my/copy-region-content-mode
          ('no 'auto)
          ('auto 'full)
          (_ 'no))))

(transient-define-infix my/copy-region--infix-content ()
  "Toggle the region content mode between no, auto and full."
  :class 'transient-lisp-variable
  :variable 'my/copy-region-content-mode
  :description (lambda () (format "Content: %s" my/copy-region-content-mode))
  :reader #'my/copy-region--cycle-content)

(defun my/copy-region-absolute ()
  "Copy region with absolute path and current content mode."
  (interactive)
  (cref-copy-region 'absolute my/copy-region-content-mode))

(defun my/copy-region-git ()
  "Copy region with Git-relative path and current content mode."
  (interactive)
  (cref-copy-region 'git my/copy-region-content-mode))

(defun my/copy-region-filename ()
  "Copy region with file-name-only path and current content mode."
  (interactive)
  (cref-copy-region 'filename my/copy-region-content-mode))

(transient-define-prefix my/copy-region-menu ()
  "Copy region location with a selectable content mode."
  ["Content"
   ("-c" my/copy-region--infix-content)]
  [["Path Style"
    ("a" "Absolute path" my/copy-region-absolute)
    ("r" "Git relative" my/copy-region-git)
    ("n" "File name only" my/copy-region-filename)]
   ["Quick Actions"
    ("q" "Quit" transient-quit-one)]]
  (interactive)
  (setq my/copy-region-content-mode 'auto)
  (transient-setup 'my/copy-region-menu))

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

    "SPC" 'agentmux-transient

    ";" 'embark-act

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
    "tv" '(my/toggle-respect-visual-line-mode :wk "respect-visual-line")

    "w" 'save-buffer
    "x" '(execute-extended-command    :wk "M-x")
    "y" '(consult-yank-from-kill-ring :wk "yank-from-kill-ring")
    ))

(provide 'init-general)
;;; init-general.el ends here
