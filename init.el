;; -*- coding: utf-8; lexical-binding: t; -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

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

(require 'init-elpa)
(require 'init-package)

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-basic)
(require 'init-theme)
(require 'init-window)
(require 'init-keybind)
(require 'init-edit)
(require 'init-evil)
(require 'init-general)
(require 'init-highlight)
(require 'init-ivy)
(require 'init-hydra)
(require 'init-search)
(require 'init-dired)
(require 'init-session)
(require 'init-company)
(require 'init-helm)
(require 'init-yasnippet)
(require 'init-git)
(require 'init-prog)
(require 'init-flycheck)
(require 'init-lsp)
(require 'init-go)
(require 'init-perl)
(require 'init-ruby)

;; Allow access from emacsclient
(add-hook 'after-init-hook
          (lambda ()
            (require 'server)
            (unless (server-running-p)
              (server-start))))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq debug-on-error nil)

(provide 'init)
