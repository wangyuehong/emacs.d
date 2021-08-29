;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

(use-package expand-region
  :custom
  (expand-region-contract-fast-key "z"))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package iedit)

(use-package simple
  :ensure nil
  :hook ((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace)
  :custom
  (column-number-mode t)
  (line-number-mode t)
  (size-indication-mode t)
  (line-move-visual nil)
  (column-number-indicator-zero-based nil)
  (track-eol t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (set-mark-command-repeat-pop t)
  :init
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
  :custom
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package xclip :demand xclip-mode)

(provide 'init-edit)
