;;; init-theme.el --- theme config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package srcery-theme
  :init
  (load-theme 'srcery t)
  ;; Template delimiters in a soft violet, a hue srcery's palette leaves
  ;; unused, so they read apart from comments, brackets and every token.
  (custom-theme-set-faces
    'srcery
    '(tmpl-delimiter-face ((t :foreground "#A48BD4" :weight normal))))
  :custom (srcery-invert-region nil))

(provide 'init-theme)
;;; init-theme.el ends here
