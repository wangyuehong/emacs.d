;; -*- coding: utf-8; lexical-binding: t; -*-

;; Produce backtraces when errors occur
(setq debug-on-error t)

;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

;; Adjust garbage collection thresholds during startup, and thereafter
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 128 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Bootstrap config
(require 'init-site-lisp)
(require 'init-elpa)
(require 'init-package)

;; (require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------
(require 'init-basic)
(require 'init-keybind)
(require 'init-edit)
(require 'init-themes)
(require 'init-evil)
(require 'init-general)
(require 'init-search)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-sessions)
(require 'init-highlight)
(require 'init-company)
(require 'init-ivy)
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
