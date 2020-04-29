;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package dired :ensure nil
  :config
  (setq dired-recursive-deletes 'always
        dired-recursive-copies 'always))

(use-package dired-single
  :bind
  (:map dired-mode-map
        ([remap dired-find-file] . dired-single-buffer)
        ([remap dired-up-directory] . dired-single-up-directory)))

(use-package dired-aux :ensure nil :init (setq-default dired-dwim-target t))

(use-package dired-x :ensure nil :demand)

(use-package diredfl :init (diredfl-global-mode 1))

(provide 'init-dired)
