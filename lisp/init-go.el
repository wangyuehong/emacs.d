;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package go-mode)

(use-package go-dlv)
(use-package go-impl)
(use-package go-fill-struct)
(use-package go-snippets)
(use-package gotest :custom (go-test-verbose t))
(use-package go-gen-test :custom (go-gen-test-use-testify t))
(use-package go-tag)

(use-package flycheck-golangci-lint
  :if (executable-find "golangci-lint")
  :custom
  (flycheck-golangci-lint-enable-all t)
  (flycheck-golangci-lint-disable-linters '("nolintlint" "nonamedreturns")))

(provide 'init-go)
