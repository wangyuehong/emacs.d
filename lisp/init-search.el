;;; init-search.el --- Search configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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
  (rg-keymap-prefix nil)
  (rg-command-line-flags '("--hidden" "--glob '!.git/'"))
  (rg-group-result t)
  (rg-show-columns t))

(use-package avy
  :custom
  (avy-all-windows 'all-frames)
  (avy-background t)
  (avy-highlight-first t)
  (avy-keys (string-to-list "asdfghjklqweruiopcvbnm"))
  (avy-indent-line-overlay t)
  (avy-timeout-seconds 0.4))

(provide 'init-search)
;;; init-search.el ends here
