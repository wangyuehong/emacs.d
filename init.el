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
(require 'init-basic)
(require 'init-clipboard)
(require 'init-theme)
(require 'init-window)
(require 'init-keybind)
(require 'init-edit)
(require 'init-evil)
(require 'init-general)
(require 'init-highlight)
(require 'init-search)
(require 'init-dired)
(require 'init-session)
(require 'init-completion)
(require 'init-yasnippet)
(require 'init-git)
(require 'init-prog)
(require 'init-lsp)
(require 'init-go)
(require 'init-python)
(require 'init-misc)
(require 'init-ai)

(require 'init-local nil t)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(setq custom-safe-themes t)
(when (file-exists-p custom-file)
  (load custom-file))

(setq debug-on-error nil)

(provide 'init)
;;; init.el ends here
