;;; init-git.el --- git config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :custom
  (magit-bury-buffer-function #'magit-restore-window-configuration)
  (magit-commit-show-diff nil)
  (magit-define-global-key-bindings 'recommended)
  (magit-diff-paint-whitespace nil)
  (magit-diff-refine-hunk t)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-process-popup-time 10))

(use-package vc
  :ensure nil
  :custom
  (vc-follow-symlinks t)
  (vc-allow-async-revert t)
  (vc-handled-backends '(Git)))

(use-package diff-hl
  :hook ((after-init . global-diff-hl-mode)
          (dired-mode . diff-hl-dired-mode))
  :bind (:map diff-hl-command-map
          ("k" . diff-hl-previous-hunk)
          ("j" . diff-hl-next-hunk)
          ("r" . diff-hl-revert-hunk)
          ("s" . diff-hl-show-hunk)
          ("S" . diff-hl-stage-current-hunk)
          ("U" . diff-hl-unstage-file))
  :config
  (setq-default fringes-outside-margins t)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh)))

(use-package git-modes)

(use-package git-link
  :commands git-link
  :custom
  (git-link-open-in-browser t))

(use-package smerge-mode
  :ensure nil
  :bind (:repeat-map my/smerge-basic-map
          ("n" . smerge-next)
          ("p" . smerge-prev)
          ("a" . smerge-keep-all)
          ("c" . smerge-keep-current)
          ("u" . smerge-keep-upper)
          ("l" . smerge-keep-lower)))

(provide 'init-git)
;;; init-git.el ends here
