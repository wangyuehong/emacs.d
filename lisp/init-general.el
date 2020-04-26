;;; init-general.el --- general configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package general
  :demand t
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
   "W"  'save-some-buffers
   "q"  'quickrun
   "k"  'kill-buffer-and-window
   "K"  'kill-other-buffers
   "P"  'paradox-upgrade-packages
   "r"  'counsel-yank-pop
   ","  'git-gutter-map
   "v"  'revert-buffer
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
   "i"  'counsel-imenu
   "g"  'magit-status
   "f"  'helm-ls-git-ls
   "s"  'swiper
   "t" 'multi-term
   "x"  'amx
   "y" 'youdao-dictionary-search-at-point+
   )
)
(provide 'init-general)
;;; init-general.el ends here
