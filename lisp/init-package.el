;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Initialize packages
(package-initialize)

;; Setup `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Should set before loading `use-package'
(setq use-package-always-ensure t
  use-package-always-defer t
  use-package-expand-minimally t
  package-install-upgrade-built-in t
  use-package-enable-imenu-support t)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(provide 'init-package)
;;; init-package.el ends here
