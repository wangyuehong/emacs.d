;;; init-general.el --- general configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package general
  :after evil
  :config
  (general-evil-setup t)
  (general-define-key
   :states '(normal visual)
   :prefix ","
   "="  'align
   "a"  'ag-regexp-project-at-point
   "A"  'ag-regexp
   "e"  'iedit-mode
   "E"  'iedit-mode-toggle-on-function
   "f"  'flycheck-list-errors
   "w"  'save-buffer
   "W"  'save-some-buffers
   "l"  'helm-ls-git-ls
   "i"  'counsel-imenu
   "s"  'swiper
   "q"  'quickrun
   "/"  'evilnc-comment-or-uncomment-lines
   "k"  'kill-buffer-and-window
   "K"  'kill-other-buffers
   "p"  'projectile-command-map
   "P"  'list-packages
   "g"  'magit-status
   "r"  'counsel-yank-pop
   ","  'git-gutter-map
   "v"  'revert-buffer
   "x"  'counsel-M-x
   "nn" 'narrow-to-region
   "nd" 'narrow-to-defun
   "nw" 'widen
   ;; "pp" 'wgrep-toggle-readonly-area
   ;; "pe" 'wgrep-finish-edit
   ;; "pk" 'wgrep-abort-changes
   ;; "ps" 'wgrep-save-all-buffers
   "d"  'delete-trailing-whitespace
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual)
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "SPC" 'avy-goto-word-1
   "j" 'avy-goto-char
   "l" 'avy-goto-line
   "e" 'er/expand-region
   "y" 'youdao-dictionary-search-at-point+
   "t" 'multi-term
   )
)
(provide 'init-general)
;;; init-general.el ends here
