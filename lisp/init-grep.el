;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(use-package wgrep)
;; C-c C-p wgrep-change-to-wgrep-mode
;; C-c C-c wgrep-finish-edit
;; C-c C-k wgrep-abort-changes

(use-package ag
  :if (executable-find "ag")
  :init
  (setq-default ag-highlight-search t)
  (setq-default ag-reuse-window t)
  (setq-default ag-reuse-buffers t)
  )

(use-package wgrep-ag
  :after (rep ag)
  :hook (ag-mode . wgrep-ag-setup)
  )

(provide 'init-grep)
;;; init-grep.el ends here