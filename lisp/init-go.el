(require-package 'go-mode)
(require-package 'go-snippets)
(require-package 'toml-mode)
(require-package 'golint)

(after-load 'go-mode
  (define-key go-mode-map (kbd "C-c d") 'godoc-at-point)
  (add-hook 'before-save-hook #'gofmt-before-save))

(provide 'init-go)
