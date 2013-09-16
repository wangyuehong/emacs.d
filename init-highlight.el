(require-package 'highlight-symbol)

(require 'highlight-symbol)

(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "<f4>") 'highlight-symbol-prev)
(global-set-key (kbd "<f5>") 'highlight-symbol-at-point)
(global-set-key (kbd "ESC <f3>") 'highlight-symbol-remove-all)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

(require-package 'hl-line+)
(require 'hl-line+)

(toggle-hl-line-when-idle 1)
(hl-line-when-idle-interval 1)
(set-face-background 'hl-line "brightblack")

(provide 'init-highlight)
