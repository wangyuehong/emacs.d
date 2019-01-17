;;; init-highlight.el --- highlight configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package highlight-symbol
  :diminish
  :init
  (setq highlight-symbol-idle-delay 0.3)
  (setq highlight-symbol-colors (quote ("#5c5cff" "#ff0000" "#00ff00" "#ff00ff" "#ffff00")))
  :hook
  ((prog-mode yaml-mode) . highlight-symbol-mode)
  ((prog-mode yaml-mode) . highlight-symbol-nav-mode)
  :config
  (set-face-attribute 'highlight-symbol-face nil
                      :inherit nil
                      :background "#626262")
  (global-set-key (kbd "<f3>") 'highlight-symbol-next)
  (global-set-key (kbd "<f4>") 'highlight-symbol-prev)
  (global-set-key (kbd "<f5>") 'highlight-symbol)
  (global-set-key (kbd "ESC <f3>") 'highlight-symbol-remove-all)
  (global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)
  )

(use-package fic-mode :hook prog-mode)
;; (set-face-foreground 'fic-face "#7f7f7f")
;; (set-face-background 'fic-face "#ffff00")

(provide 'init-highlight)

;;; init-highlight.el ends here