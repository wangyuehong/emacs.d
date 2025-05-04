;;; init-theme.el --- theme config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package srcery-theme
  :unless (display-graphic-p)
  :init (load-theme 'srcery t)
  :custom (srcery-invert-region nil))

(use-package color-theme-sanityinc-solarized
  :if (display-graphic-p)
  :init (load-theme 'sanityinc-solarized-light t))

(provide 'init-theme)
;;; init-theme.el ends here
