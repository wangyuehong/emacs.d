;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package uniquify
  :ensure nil
  :init
  (setq
   uniquify-buffer-name-style 'reverse
   ;; uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
   uniquify-separator " • "
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"
   ))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :bind (("C-r" . undo-tree-undo)))

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package expand-region
  :init (setq expand-region-contract-fast-key "z"))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package iedit)

(use-package simple
  :ensure nil
  :hook (((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        line-move-visual nil
        track-eol t
        save-interprogram-paste-before-kill t
        set-mark-command-repeat-pop t)

  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package ediff
  :ensure nil
  :hook(;; show org ediffs unfolded
        (ediff-prepare-buffer . outline-show-all)
        ;; restore window layout when done
        (ediff-quit . winner-undo))
  :config
  (setq ediff-window-setup-function 'ediff-setup-windows-plain)
  (setq ediff-split-window-function 'split-window-horizontally)
  (setq ediff-merge-split-window-function 'split-window-horizontally))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(provide 'init-edit)
