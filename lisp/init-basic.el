;;; init-basic.el --- basic configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :preface
  (defun my/coding-setup ()
    (modify-syntax-entry ?_ "w")
    (pcase major-mode
      ('lua-ts-mode
        (modify-syntax-entry ?- "w 12"))
      (_ (modify-syntax-entry ?- "w"))))
  :hook
  ((prog-mode yaml-ts-mode) . my/coding-setup)
  (after-init . transient-mark-mode)
  (after-init . repeat-mode)
  :init
  ;; Encoding
  ;; UTF-8 as the default coding system
  (when (fboundp 'set-charset-priority)
    (set-charset-priority 'unicode))

  (set-locale-environment "en_US.UTF-8")
  (set-language-environment 'utf-8)
  (set-selection-coding-system 'utf-8)
  (modify-coding-system-alist 'process "*" 'utf-8)

  (setopt use-short-answers t)
  (put 'other-window 'repeat-map nil)
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
    ring-bell-function #'ignore
    display-warning-minimum-level :error
    warning-minimum-level :error
    truncate-partial-width-windows nil)

  (setq-default
    major-mode 'text-mode
    tab-width 4
    indent-tabs-mode nil)

  (setq inhibit-compacting-font-caches t  ; Don’t compact font caches during GC.
    delete-by-moving-to-trash t       ; Deleting files go to OS's trash folder
    make-backup-files nil             ; Forbid making backup files
    auto-save-default nil             ; Disable auto save
    create-lockfiles nil              ; No lockfiles

    sentence-end "\\([。！？]\\|……\\|[.?!][]\"')}]*\\($\\|[ \t]\\)\\)[ \t\n]*"
    sentence-end-double-space nil)

  (setq use-file-dialog nil)
  (setq use-dialog-box nil)
  (setq inhibit-startup-screen t)

  (setq frame-title-format
    '((:eval (if (buffer-file-name)
               (abbreviate-file-name (buffer-file-name))
               "%b")))))

;; Workaround: disable auto-composition-mode in TUI to prevent emoji
;; rendering artifacts (cursor misalignment, screen corruption on redraw).
;; Terminals cannot compose characters (ligatures, ZWJ emoji) anyway.
(use-package emacs
  :unless (display-graphic-p)
  :config
  (setq-default auto-composition-mode nil))

(use-package quail
  :ensure nil
  :config
  (setq quail-japanese-use-double-n t))

(use-package bookmark
  :ensure nil
  :bind (("C-x j j" . bookmark-jump)
          ("C-x j d" . bookmark-delete))
  :init
  (setq bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)))

(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

(use-package eldoc
  :ensure nil)

(use-package imenu
  :ensure nil
  :hook (imenu-after-jump . recenter)
  :custom (imenu-max-item-length 108))

(use-package repeat
  :ensure nil
  :custom
  (repeat-echo-function #'repeat-echo-message))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)
          ("C-h x" . helpful-command)))

(provide 'init-basic)
;;; init-basic.el ends here
