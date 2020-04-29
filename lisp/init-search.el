;; -*- coding: utf-8; lexical-binding: t; -*-

(setq-default grep-highlight-matches t
              grep-scroll-output t)

;; C-c C-p wgrep-change-to-wgrep-mode
;; C-c C-c wgrep-finish-edit
;; C-c C-k wgrep-abort-changes
(use-package wgrep
  :init
  (setq wgrep-enable-key "e"
        wgrep-auto-save-buffer t
        wgrep-change-readonly-file t))

(use-package ag
  :if (executable-find "ag")
  :init
  (setq-default ag-highlight-search t)
  (setq-default ag-reuse-window t)
  (setq-default ag-reuse-buffers t)
  )

(use-package wgrep-ag :hook (ag-mode . wgrep-ag-setup))

(use-package anzu
  :diminish
  :bind (([remap query-replace] . anzu-query-replace)
         ([remap query-replace-regexp] . anzu-query-replace-regexp)
         :map isearch-mode-map
         ([remap isearch-query-replace] . anzu-isearch-query-replace)
         ([remap isearch-query-replace-regexp] . anzu-isearch-query-replace-regexp))
  :hook (after-init . global-anzu-mode))

(use-package avy
  :init
  (setq avy-keys (string-to-list "asdfghjklqweruiopzxcv"))
  (setq avy-all-windows t)
  (setq avy-background t)
  (setq avy-highlight-first t)
  )

(provide 'init-search)
