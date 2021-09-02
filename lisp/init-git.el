;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package magit
  :custom
  (magit-process-popup-time 10)
  (magit-diff-refine-hunk t))

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-allow-async-revert t)
  (vc-handled-backends '(Git)))

(use-package diff-hl
  :hook ((after-init         . global-diff-hl-mode)
         (magit-pre-refresh  . diff-hl-magit-pre-refresh)
         (magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode         . diff-hl-dired-mode-unless-remote))
  :bind (:map diff-hl-command-map
              ("v" . diff-hl-hydra/body))
  :config
  (unless (window-system) (diff-hl-margin-mode))
  (setq-default fringes-outside-margins t)
  (diff-hl-flydiff-mode 1)
  :pretty-hydra
  ((:title "diff-hl" :foreign-keys warn :color amaranth :quit-key "q")
    ("Hunk"
     (("k" diff-hl-previous-hunk)
      ("j" diff-hl-next-hunk)
      ("p" diff-hl-show-hunk))
     "Show"
     (("C-k" diff-hl-show-hunk-previous)
      ("C-j" diff-hl-show-hunk-next))
     "Action"
     (("v" diff-hl-revert-hunk)
      ("c" diff-hl-show-hunk-copy-original-text))
     )))

(use-package git-timemachine :commands git-timemachine)

(use-package gitignore-mode)
(use-package gitconfig-mode)

(provide 'init-git)
