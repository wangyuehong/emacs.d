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
   "a"  'ag-regexp-project-at-point
   "A"  'ag-regexp
   "c"  'evilnc-comment-or-uncomment-lines
   "d"  'delete-trailing-whitespace
   "e"  'evil-iedit-state/iedit-mode
   "f"  'flycheck-list-errors
   "w"  'save-buffer
   "l"  'helm-ls-git-ls
   "i"  'counsel-imenu
   "s"  'swiper
   "q"  'quickrun
   "k"  'kill-buffer-and-window
   "K"  'kill-other-buffers
   "p"  'projectile-command-map
   "P"  'paradox-upgrade-packages
   "g"  'magit-status
   "r"  'counsel-yank-pop
   ","  'git-gutter-map
   "v"  'revert-buffer
   "x"  'counsel-M-x
   "nn" 'narrow-to-region
   "nd" 'narrow-to-defun
   "nw" 'widen
   )

  (general-define-key
   ;; :states '(normal motion insert emacs)
   :states '(normal visual)
   :keymaps 'override
   ;; :non-normal-prefix "C-SPC"
   :prefix "SPC"
   "hc" 'highlight-symbol-remove-all
   "hh" 'highlight-symbol
   "t" 'multi-term
   "y" 'youdao-dictionary-search-at-point+
   "f" 'projectile-find-file
   "p" 'projectile-switch-project
   )
)
(provide 'init-general)
;;; init-general.el ends here
