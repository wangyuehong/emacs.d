(require-package 'highlight-symbol)

(require 'highlight-symbol)

(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "<f4>") 'highlight-symbol-prev)
(global-set-key (kbd "<f5>") 'highlight-symbol-at-point)
(global-set-key (kbd "ESC <f3>") 'highlight-symbol-remove-all)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

(setq highlight-symbol-colors (quote ("brightblue" "brightred" "brightgreen" "brightmagenta" "brightyellow" "brightcyan")))

;; (add-hook 'prog-mode-hook 'hl-line-mode)

;; (set-face-background 'hl-line "#3a3a3a")

(require-package 'fic-mode)

(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(set-face-foreground 'font-lock-fic-face "brightblack")
(set-face-background 'font-lock-fic-face "brightyellow")

(provide 'init-highlight)
