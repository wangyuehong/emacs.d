(require-package 'dired+)
(require-package 'dired-sort)

(toggle-diredp-find-file-reuse-dir 1)
(setq-default diredp-hide-details-initially-flag nil
              dired-dwim-target t)

(when (maybe-require-package 'diredfl)
  (diredfl-global-mode))

(after-load 'dired
  (require 'dired+)
  (require 'dired-sort)
  (when (fboundp 'global-dired-hide-details-mode)
    (global-dired-hide-details-mode -1))
  (setq dired-recursive-deletes 'top)
  (define-key dired-mode-map [mouse-2] 'dired-find-file))

(provide 'init-dired)
