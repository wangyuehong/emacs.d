;;; init-grep.el --- Settings for grep and grep-like tools -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq-default grep-highlight-matches t
              grep-scroll-output t)

(require-package 'wgrep)

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (setq-default ag-reuse-window t)
  (setq-default ag-reuse-buffers t))

(after-load 'ag
  (define-key ag-mode-map (kbd "k") nil))

(provide 'init-grep)

;;; init-grep.el ends here