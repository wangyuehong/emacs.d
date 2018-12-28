(require-package 'go-mode)
(require-package 'go-snippets)
(require-package 'toml-mode)

(require-package 'lsp-go)
(add-hook 'go-mode-hook #'lsp)

(require-package 'flycheck-gometalinter)
(eval-after-load 'flycheck
  '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

(after-load 'go-mode
  (define-key go-mode-map (kbd "C-c d") 'godoc-at-point)
  (add-hook 'before-save-hook #'gofmt-before-save))

(provide 'init-go)
