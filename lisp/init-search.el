;; -*- coding: utf-8; lexical-binding: t; -*-

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(use-package wgrep
  :custom
  (wgrep-auto-save-buffer t)
  (wgrep-change-readonly-file t))

(use-package rg
  :if (executable-find "rg")
  :hook (after-init . rg-enable-default-bindings)
  :custom
  (rg-group-result t)
  (rg-show-columns t)
  (rg-command-line-flags '("--hidden" "--glob '!.git/'"))
  )

(use-package avy
  :custom
  (avy-keys (string-to-list "asdfghjklqweruiopzxcv"))
  (avy-all-windows t)
  (avy-background t)
  (avy-highlight-first t)
  (avy-timeout-seconds 0.25)
  )

(provide 'init-search)
