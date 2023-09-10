;; -*- coding: utf-8; lexical-binding: t; -*-

;; Initialize packages
(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

;; Install use-package
(straight-use-package 'use-package)

;; Configure use-package to use straight.el by default
;; (use-package straight
;;   :custom
;;   (straight-use-package-by-default t))

;; Should set before loading `use-package'
(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-always-defer t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(eval-when-compile
  (require 'use-package))

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

(provide 'init-package)
