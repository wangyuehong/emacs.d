(require-package 'dired+)

(setq diredp-hide-details-initially-flag nil)
(toggle-diredp-find-file-reuse-dir 1)

(after-load 'dired
  (require 'dired+)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file)
  (add-hook 'dired-mode-hook
            (lambda () (guide-key/add-local-guide-key-sequence "%"))))


;; hide .DS_Store on mac
(when *is-a-mac*
  (setq-default dired-omit-files-p t)
  (require 'dired-x)
  (setq dired-omit-files
        (concat dired-omit-files "\\|.DS_Store")))

(provide 'init-dired)
