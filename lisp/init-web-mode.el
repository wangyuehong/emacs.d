(require-package 'web-mode)

(require 'web-mode)

(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-current-element-highlight t)

(set-face-foreground 'web-mode-html-tag-face "#268bd2")
(set-face-foreground 'web-mode-html-attr-name-face "#dc322f")
(set-face-foreground 'web-mode-html-attr-equal-face "#268bd2")
(set-face-foreground 'web-mode-html-attr-value-face "#859900")

(provide 'init-web-mode)