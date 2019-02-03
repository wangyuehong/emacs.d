;; init-go.el --- Golang configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Go packages:
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports

(use-package go-mode
  :bind (:map go-mode-map
              ([remap evil-jump-to-tag] . godef-jump)
              )
  :init
  (setq gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp)
  )

(use-package go-snippets)
(use-package toml-mode)

(use-package flycheck-gometalinter
  :init
  (setq flycheck-gometalinter-fast t)
  ;; (setq flycheck-gometalinter-deadline "5s")
  ;; (setq flycheck-gometalinter-errors-only t)
  ;; (setq flycheck-gometalinter-tests t)
  ;; (setq flycheck-gometalinter-disable-linters '("gotype" "gocyclo"))
  ;; (setq flycheck-gometalinter-enable-linters '("golint"))
  ;; (setq flycheck-gometalinter-disable-all t)
  ;; (setq flycheck-gometalinter-config "/path/to/gometalinter-config.json")
  :config
  (flycheck-gometalinter-setup)
  )

(provide 'init-go)
;;; init-go.el ends here
