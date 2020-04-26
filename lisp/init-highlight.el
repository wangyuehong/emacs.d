;;; init-highlight.el --- highlight configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package highlight-symbol
  :diminish
  :init
  (setq highlight-symbol-idle-delay 0.5)
  (setq highlight-symbol-colors (quote ("#5c5cff" "#ff0000" "#00ff00" "#ff00ff" "#ffff00")))
  :hook
  ((prog-mode yaml-mode) . highlight-symbol-mode)
  ((prog-mode yaml-mode) . highlight-symbol-nav-mode)
  ;; :config
  ;; (set-face-attribute 'highlight-symbol-face nil
  ;;                     :inherit nil
  ;;                     :background "#626262")
  )

(use-package fic-mode :hook prog-mode)

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(provide 'init-highlight)

;;; init-highlight.el ends here
