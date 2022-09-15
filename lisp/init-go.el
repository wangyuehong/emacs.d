;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package go-mode
  ;; :config
  ;; (when (executable-find "gofumpt")
  ;;   (setq gofmt-command "gofumpt"))
  ;; (add-hook 'before-save-hook #'gofmt-before-save)
  )

(use-package go-dlv)
(use-package go-impl)
(use-package go-fill-struct)
(use-package go-snippets)
(use-package gotest :custom (go-test-verbose t))
(use-package go-gen-test :custom (go-gen-test-executable "gotests -template testify"))
(use-package go-tag)
(use-package toml-mode)

;; Install: See https://github.com/golangci/golangci-lint#install
(use-package flycheck-golangci-lint
  :if (executable-find "golangci-lint")
  :defines flycheck-disabled-checkers
  :hook (go-mode . (lambda ()
                     "Enable golangci-lint."
                     (setq flycheck-disabled-checkers '(go-gofmt
                                                        go-golint
                                                        go-vet
                                                        go-build
                                                        go-test
                                                        go-errcheck))
                     (flycheck-golangci-lint-setup))))

(provide 'init-go)
