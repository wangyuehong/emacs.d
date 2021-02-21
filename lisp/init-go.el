;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package go-mode
  :config
  (when (executable-find "goimports")
    (setq gofmt-command "goimports"))
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp-deferred)
  (use-package go-dlv)
  (use-package go-impl)
  (use-package go-fill-struct)
  (use-package go-snippets)
  (use-package gotest :custom (go-test-verbose t))
  (use-package go-gen-test :custom (go-gen-test-executable "gotests -template testify"))
  (use-package go-tag)
  :mode-hydra
  ((:title "go-hydra" :foreign-keys warn :color red :quit-key "q")
   ("Test"
    (("t" go-test-current-file)
     ("g" go-gen-test-dwim))
    "Code"
    (("f" go-fill-struct)
     ("i" go-impl))
   ))
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
