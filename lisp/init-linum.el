(global-linum-mode 1)
(setq linum-format 'dynamic)

(require-package 'hlinum)
(require 'hlinum)
(hlinum-activate)

(set-face-foreground 'linum-highlight-face "brightwhite")
(set-face-background 'linum-highlight-face "brightred")

(provide 'init-linum)
