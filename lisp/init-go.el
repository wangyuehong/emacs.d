(require-package 'go-mode)
(require-package 'go-snippets)
(require-package 'toml-mode)

(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'init-go)