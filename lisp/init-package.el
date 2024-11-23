;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package package
  :ensure nil
  :init
  (setq use-package-always-ensure t
    use-package-always-defer t
    use-package-expand-minimally t
    package-install-upgrade-built-in t
    use-package-enable-imenu-support t)
  :config
  ;; install into separate package dirs for each emacs version
  (let ((versioned-package-dir
          (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
            user-emacs-directory)))
    (setq package-user-dir versioned-package-dir))
  (setq package-archives
    '(("melpa"   . "https://melpa.org/packages/")
       ("gnu"    . "https://elpa.gnu.org/packages/")
       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(provide 'init-package)
;;; init-package.el ends here
