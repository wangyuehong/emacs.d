;; init-go.el --- Golang configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Go packages:
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/derekparker/delve/cmd/dlv

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

(use-package go-dlv
  :after go-mode
  )
(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
              ("C-c t p" . go-test-current-project)
              ("C-c t f" . go-test-current-file)
              ("C-c t ." . go-test-current-test)
              ("C-c t x" . go-run)))

(use-package go-gen-test
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-t" . go-gen-test-dwim)))

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
