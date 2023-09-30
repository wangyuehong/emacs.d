;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package go-mode
  :bind (:map go-mode-map
              ("C-c i" . go-import-add))
  :config
  (use-package go-tag)
  (use-package go-gen-test)
  (use-package go-snippets)
  (use-package gotest
    :bind (:map go-mode-map
				("C-c t t" . go-test-current-test)
				("C-c t p" . go-test-current-project))
	:config
	(setq go-test-args "-failfast -race")
    :custom
    (go-test-verbose t)))

(provide 'init-go)
