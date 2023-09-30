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
  (dired-listing-switches "-AFhlv")
  (dired-dwim-target t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-auto-revert-buffer #'dired-directory-changed-p)
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil)))

(use-package dired-aux
  :ensure nil
  :after dired
  :custom
  (dired-vc-rename-file t)
  (dired-isearch-filenames 'dwim)
  (dired-create-destination-dirs 'ask))

(use-package dired-x
  :ensure nil
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-verbose nil))

(use-package diredfl
  :hook (dired-mode . diredfl-mode))

(provide 'init-dired)
