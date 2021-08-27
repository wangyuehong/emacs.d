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
  )

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package avy
  :custom
  (avy-keys (string-to-list "asdfghjklqweruiopzxcv"))
  (avy-all-windows t)
  (avy-background t)
  (avy-highlight-first t)
  )

(provide 'init-search)
