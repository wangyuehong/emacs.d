(require-package 'web-mode)

(require 'web-mode)

(add-hook 'web-mode-hook (lambda () (whitespace-mode -1)))

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(provide 'init-web-mode)
