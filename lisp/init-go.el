;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c i" . go-import-add))
  :config
  (use-package go-dlv)
  (use-package go-impl)
  (use-package go-fill-struct)
  (use-package go-snippets)
  (use-package gotest
    :bind (:map go-mode-map
				("C-c t t" . go-test-current-test)
				("C-c t p" . go-test-current-project))
	:config
	(setq go-test-args "-failfast -race -count=1 ")
    :custom
    (go-test-verbose t))

  (use-package go-gen-test
    :bind (:map go-mode-map
                ("C-c t g" . go-gen-test-dwim)))

  (use-package go-tag
    :bind (:map go-mode-map
                ("C-c t a" . go-tag-add)
                ("C-c t r" . go-tag-remove)))
  )

(use-package flycheck-golangci-lint
  :if (executable-find "golangci-lint")
  :custom
  (flycheck-golangci-lint-enable-all t)
  (flycheck-golangci-lint-disable-linters '("nolintlint" "nonamedreturns")))

(provide 'init-go)
