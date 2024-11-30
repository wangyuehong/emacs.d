;;; init-theme.el --- theme config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package srcery-theme
  :init (load-theme 'srcery t)
  :custom (srcery-invert-region nil))

(use-package modus-themes
  :disabled t
  :init (load-theme 'modus-operandi-tinted t)
  :custom
  (modus-themes-bold-constructs t)
  (modus-themes-italic-constructs t))

(provide 'init-theme)
;;; init-theme.el ends here
