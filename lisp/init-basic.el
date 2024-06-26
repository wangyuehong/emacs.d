;;; init-basic.el --- basic configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode) (set-scroll-bar-mode nil))
(unless (display-graphic-p) (menu-bar-mode -1))

(fset 'yes-or-no-p 'y-or-n-p)

(setq load-prefer-newer t)

(put 'narrow-to-defun  'disabled nil)
(put 'narrow-to-region 'disabled nil)

(setq-default
 case-fold-search t
 history-length 100
 history-delete-duplicates t
 compilation-scroll-output t
 line-spacing 0.2
 scroll-preserve-screen-position 'always
 tooltip-delay 0.5
 truncate-lines nil
 xref-prompt-for-identifier nil
 scroll-conservatively 10000
 ring-bell-function 'ignore
 display-warning-minimum-level :error
 warning-minimum-level :error
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'transient-mark-mode)
(add-hook 'after-init-hook 'repeat-mode)

(add-hook 'after-change-major-mode-hook
          (lambda ()
            (modify-syntax-entry ?_ "w")))

(setq-default major-mode 'text-mode
              fill-column 80
              tab-width 4
              indent-tabs-mode nil)

(setq visible-bell t
      inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
      delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
      make-backup-files nil             ; Forbide to make backup files
      auto-save-default nil             ; Disable auto save
      create-lockfiles nil              ; No lockfiles

      adaptive-fill-regexp "[ t]+|[ t]*([0-9]+.|*+)[ t]*"
      adaptive-fill-first-line-regexp "^* *$"
      sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
      sentence-end-double-space nil)

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Encoding
;; UTF-8 as the default coding system
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; Explicitly set the prefered coding systems to avoid annoying prompt
;; from emacs (especially on Microsoft Windows)
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

(set-locale-environment "en_US.UTF-8")
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)

(use-package bookmark
  :ensure nil
  :bind
  (("C-x j j" . bookmark-jump))
  :init
  (setq bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)))

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

(use-package hideshow
  :ensure nil
  :hook (prog-mode . hs-minor-mode))

(use-package eldoc
  :ensure nil)

(use-package imenu
  :ensure nil
  :hook (imenu-after-jump . recenter)
  :custom (imenu-max-item-length 108))

(use-package exec-path-from-shell
  :when (eq system-type 'darwin)
  :hook (after-init . exec-path-from-shell-initialize))

(provide 'init-basic)
;;; init-basic.el ends here
