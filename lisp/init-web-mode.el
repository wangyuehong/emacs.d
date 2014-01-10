(require-package 'web-mode)

(require 'web-mode)

(defun wangyh/web-mode-hook()
  (whitespace-mode -1)
  (fic-mode -1)
  (rainbow-mode -1)
  (rainbow-delimiters-mode -1))

(add-hook 'web-mode-hook 'wangyh/web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"  . "\\.phtml\\'")
        ("erb"  . "\\.erb")))

(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-current-element-highlight t)

(set-face-foreground 'web-mode-html-tag-face "#268bd2")
(set-face-foreground 'web-mode-html-attr-name-face "#dc322f")
(set-face-foreground 'web-mode-html-attr-equal-face "#268bd2")
(set-face-foreground 'web-mode-html-attr-value-face "#859900")
(set-face-background 'web-mode-current-element-highlight-face "#444444")

(provide 'init-web-mode)
