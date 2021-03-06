;; -*- coding: utf-8; lexical-binding: t; -*-

(when (fboundp 'menu-bar-mode)
  (menu-bar-mode -1))
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

(fset 'yes-or-no-p 'y-or-n-p)

(setq load-prefer-newer t)

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
 truncate-partial-width-windows nil)

(add-hook 'after-init-hook 'transient-mark-mode)

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
  :init
  (setq bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory))
  )

(use-package so-long
  :ensure nil
  :when (>= emacs-major-version 27)
  :hook (after-init . global-so-long-mode))

(use-package server
  :ensure nil
  :config
  (unless (server-running-p)
    (server-start)))

(use-package eldoc :ensure nil :diminish)

(provide 'init-basic)
