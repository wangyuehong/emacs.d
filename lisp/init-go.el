(require-package 'go-mode)
(require-package 'toml-mode)

(add-hook 'before-save-hook #'gofmt-before-save)

(provide 'init-go)
