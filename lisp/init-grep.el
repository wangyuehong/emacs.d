;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(use-package wgrep)

(when (executable-find "ag")
  (use-package ag
    :init
    (setq-default ag-highlight-search t)
    (setq-default ag-reuse-window t)
    (setq-default ag-reuse-buffers t)
    ))

(provide 'init-grep)
;;; init-grep.el ends here