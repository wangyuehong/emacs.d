;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . auto-revert-mode)
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-alh")
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  )

(use-package dired-single
  :bind
  (:map dired-mode-map
        ;; ("C-x C-j" . dired-single-up-directory)
        ([remap dired-find-file] . dired-single-buffer)
        ([remap dired-up-directory] . dired-single-up-directory)))

(use-package dired-aux :ensure nil :init (setq-default dired-dwim-target t))

(use-package dired-x :ensure nil :demand)

(use-package diredfl :init (diredfl-global-mode 1))

(provide 'init-dired)
