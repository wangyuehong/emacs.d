(require-package 'go-mode)
(require-package 'go-snippets)
(require-package 'toml-mode)
(require-package 'golint)
;; (require-package 'company-go)

;; (add-hook 'go-mode-hook
;;           (lambda ()
;;             (set (make-local-variable 'company-backends) '(company-go))
;;             (company-mode)))

(after-load 'go-mode
  (define-key go-mode-map (kbd "C-c d") 'godoc-at-point)
  (add-hook 'before-save-hook #'gofmt-before-save))

(provide 'init-go)
