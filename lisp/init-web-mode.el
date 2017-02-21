(require-package 'web-mode)

(require 'web-mode)

(defun wangyh/web-mode-hook()
)

(add-hook 'web-mode-hook 'wangyh/web-mode-hook)

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))

(setq web-mode-engines-alist
      '(("php"  . "\\.phtml\\'")
        ("erb"  . "\\.erb")))

(setq web-mode-enable-comment-keywords t)
(setq web-mode-enable-current-element-highlight nil)

(setq web-mode-markup-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(setq web-mode-code-indent-offset 2)

(set-face-foreground 'web-mode-html-tag-face "#268bd2")
(set-face-foreground 'web-mode-html-attr-name-face "#dc322f")
(set-face-foreground 'web-mode-html-attr-equal-face "#268bd2")
(set-face-foreground 'web-mode-html-attr-value-face "#859900")
;; (set-face-background 'web-mode-current-element-highlight-face "#4e4e4e")

;; disable fci-mode
; https://github.com/alpaker/Fill-Column-Indicator/issues/46
(add-hook 'after-change-major-mode-hook
          (lambda () (if (string= major-mode "web-mode")
                    (turn-off-fci-mode) (turn-on-fci-mode))))

(provide 'init-web-mode)
