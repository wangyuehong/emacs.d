;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package magit
  :mode (("\\COMMIT_EDITMSG\\'" . text-mode)
         ("\\MERGE_MSG\\'" . text-mode))
  :bind (("C-x g" . magit-status))
  :init
  (setq-default
   magit-process-popup-time 10
   magit-diff-refine-hunk t
   magit-completing-read-function 'ivy-completing-read)
  )

(use-package evil-magit :demand t)

(use-package git-gutter
  :diminish
  :demand t
  :init
  (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
  :config
  (global-git-gutter-mode t))

(use-package gitignore-mode)
(use-package gitconfig-mode)

(provide 'init-git)
