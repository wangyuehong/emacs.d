;; -*- coding: utf-8; lexical-binding: t; -*-

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t)
  (setq straight-vc-git-default-clone-depth 1))

(eval-when-compile
  (require 'use-package))

;; init straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Required by `use-package'
(use-package diminish)
(use-package bind-key)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

;; A modern Packages Menu
(use-package paradox
  :custom
  (paradox-execute-asynchronously t)
  (paradox-github-token t)
  (paradox-display-star-count nil))

;; Auto update packages
(use-package auto-package-update
  :init
  (defalias 'upgrade-packages #'auto-package-update-now)
  :custom
  (auto-package-update-delete-old-versions t)
  (auto-package-update-hide-results t))

(defun upgrade-all-packages ()
  "update all packages using auto-package-update and straight-pull-all"
  (interactive)
  (auto-package-update-now-async)
  (straight-pull-all))

(provide 'init-package)
