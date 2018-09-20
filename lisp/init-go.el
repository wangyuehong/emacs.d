(require-package 'go-mode)
(require-package 'go-snippets)
(require-package 'toml-mode)
(require-package 'golint)

(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'init-go)
