;;; init-theme.el --- theme config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package srcery-theme
  :init (load-theme 'srcery t)
  :custom (srcery-invert-region nil))

(use-package gruvbox-theme)

(provide 'init-theme)
;;; init-theme.el ends here
