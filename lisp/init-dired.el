;; init-dired.el --- dired configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)
(setq-default ;; Always copy/delete recursively
 dired-recursive-copies  'always
 dired-recursive-deletes 'top
 dired-dwim-target t
 diredp-hide-details-initially-flag nil
 )

(use-package diredfl :init (diredfl-global-mode 1))

(provide 'init-dired)
;;; init-dired.el ends here
