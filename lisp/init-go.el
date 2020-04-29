;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package go-mode
  :bind (:map go-mode-map
              ([remap evil-jump-to-tag] . lsp-find-definition)
              )
  :config
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp)
  (use-package go-dlv)
  (use-package go-impl)
  (use-package go-fill-struct)
  (use-package go-snippets)
  )

(use-package gotest :after go-mode
  :bind (:map go-mode-map
              ("C-c t p" . go-test-current-project)
              ("C-c t f" . go-test-current-file)
              ("C-c t ." . go-test-current-test)
              ("C-c t x" . go-run)))

(use-package go-gen-test :after go-mode
  :bind (:map go-mode-map
              ("C-c C-t" . go-gen-test-dwim))
  :init
  (setq go-gen-test-executable "gotests -template testify")
  )

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
