;;; init.el --- my emacs configuration. -*- lexical-binding: t no-byte-compile: t -*-
;;; Commentary:
;;; Code:

;; (setq use-package-compute-statistics t)
;; C-x use-package-report

;; Produce backtraces when errors occur
(setq debug-on-error t)

;; Don't pass case-insensitive to `auto-mode-alist'
(setq auto-mode-case-fold nil)

;; load path
(defun update-load-path (&rest _)
  "Update `load-path'."
  (dolist (dir '("site-lisp" "lisp"))
    (push (expand-file-name dir user-emacs-directory) load-path)))

(defun add-subdirs-to-load-path (&rest _)
  "Add subdirectories to `load-path'."
  (let ((default-directory (expand-file-name "site-lisp" user-emacs-directory)))
    (normal-top-level-add-subdirs-to-load-path)))

(advice-add #'package-initialize :after #'update-load-path)
(advice-add #'package-initialize :after #'add-subdirs-to-load-path)

(update-load-path)

(require 'init-package)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
;; Core Basics
(require 'init-env)
(require 'init-basic)
(require 'init-clipboard)

;; Core UI & Editing Fundamentals
(require 'init-theme)
(require 'init-highlight)
(require 'init-edit)

;; Major Mode System (Evil)
(require 'init-evil)

;; Keybindings (Depends on commands and Evil state)
(require 'init-keybind)
(require 'init-general) ; uses Evil

;; UI Components (Depends on icons, theme, evil state tags)
(require 'init-ui)

;; Window Management (May use icons)
(require 'init-window)

;; Search, Completion, Navigation Tools
(require 'init-search)
(require 'init-completion)
(require 'init-yasnippet)

;; File & Session Management
(require 'init-dired)
(require 'init-session)

;; Programming Core Support
(require 'init-prog)

;; Language Specific / LSP
(require 'init-lsp)
(require 'init-go)
(require 'init-python)

;; Integrations & External Tools
(require 'init-git)
(require 'init-term)
(require 'init-im)
(require 'init-ai)

;; Custom Utilities
(require 'init-utils)

;; Miscellaneous
(require 'init-misc)

;; Local Overrides (Must be last)
(require 'init-local nil t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-safe-themes t)
(when (file-exists-p custom-file)
  (load custom-file))

(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(setq debug-on-error nil)

(provide 'init)
;;; init.el ends here
