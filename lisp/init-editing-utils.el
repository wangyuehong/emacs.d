;;; init-editing-utils.el --- edit configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package unfill)

(when (fboundp 'electric-pair-mode)
  (add-hook 'after-init-hook 'electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (add-hook 'after-init-hook 'electric-indent-mode))

(use-package list-unicode-display)

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 tab-width 4
 line-spacing 0.2
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 scroll-conservatively 10000
 ring-bell-function 'ignore
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(add-hook 'after-init-hook 'transient-mark-mode)

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode)
  )

;; (when (maybe-require-package 'indent-guide)
;;   (add-hook 'prog-mode-hook 'indent-guide-mode)
;;   (add-hook 'yaml-mode-hook 'indent-guide-mode)
;;   (setq indent-guide-delay 1.5)
;;   (after-load 'indent-guide
;;     (diminish 'indent-guide-mode)))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :bind (("C-r" . undo-tree-undo))
  )
;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(add-hook 'after-init-hook 'show-paren-mode)

(use-package expand-region
  :init
  (setq expand-region-contract-fast-key "z")
  )

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode)
  )

(use-package iedit)

(use-package super-save
  :diminish
  :init
  (setq super-save-auto-save-when-idle t)
  (setq super-save-idle-duration 2)
  :config
  (super-save-mode)
  )

(provide 'init-editing-utils)
;;; init-editing-utils.el ends here