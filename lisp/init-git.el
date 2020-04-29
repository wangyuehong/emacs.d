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
  (setq magit-last-seen-setup-instructions "1.4.0")
  )

(use-package evil-magit :demand t)

(use-package git-gutter
  :diminish
  :demand t
  :init
  (setq git-gutter:update-threshold 2)
  (setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
  :config
  (global-git-gutter-mode t)
  (define-prefix-command 'git-gutter-map)

  (define-key git-gutter-map "j" 'git-gutter:next-hunk)
  (define-key git-gutter-map "k" 'git-gutter:previous-hunk)
  (define-key git-gutter-map "p" 'git-gutter:popup-hunk)
  (define-key git-gutter-map "v" 'git-gutter:revert-hunk)
  (define-key git-gutter-map "s" 'git-gutter:stage-hunk)
  )

(use-package gitignore-mode)
(use-package gitconfig-mode)

(provide 'init-git)
