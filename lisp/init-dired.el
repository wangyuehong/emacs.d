;;; init-dired.el --- dired config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package dired
  :ensure nil
  :hook
  (dired-mode . dired-hide-details-mode)
  (dired-mode . auto-revert-mode)
  :config
  (unbind-key "j" dired-jump-map)
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil))
  :custom
  (dired-recursive-deletes 'always)
  (dired-recursive-copies 'always)
  (dired-hide-details-hide-symlink-targets nil)
  (dired-listing-switches "-Ahlv")
  (dired-dwim-target t)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-auto-revert-buffer #'dired-directory-changed-p))

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
  :after dired
  :hook (dired-mode . diredfl-mode)
  :custom-face
  (diredfl-dir-name ((t (:background unspecified)))))

(use-package dired-subtree
  :after dired
  :bind
  (:map dired-mode-map
    ("<tab>" . dired-subtree-toggle)
    ("TAB" . dired-subtree-toggle))
  :custom
  (dired-subtree-use-backgrounds nil))

(use-package nerd-icons-dired
  :after nerd-icons
  :hook
  (dired-mode . nerd-icons-dired-mode))

(provide 'init-dired)
;;; init-dired.el ends here
