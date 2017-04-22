(setq-default grep-highlight-matches t
              grep-scroll-output t)

(when *is-a-mac*
  (setq-default locate-command "mdfind"))

(when (executable-find "ag")
  (require-package 'ag)
  (require-package 'wgrep-ag)
  (setq-default ag-highlight-search t)
  (setq-default ag-reuse-window t)
  (setq-default ag-reuse-buffers t))

(after-load 'ag
  (define-key ag-mode-map (kbd "k") 'previous-line)
  (define-key ag-mode-map (kbd "h") 'left-char))

(provide 'init-grep)
