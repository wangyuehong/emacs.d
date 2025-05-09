;;; init-git.el --- git config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package magit
  :defines (git-commit-mode-map)
  :bind(:map git-commit-mode-map
         ("C-c i" . copilot-chat-insert-commit-message))
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
  :defines (diff-hl-command-map)
  :functions (diff-hl-magit-pre-refresh diff-hl-magit-post-refresh)
  :hook ((after-init . global-diff-hl-mode)
          (dired-mode . diff-hl-dired-mode))
  :bind (:map diff-hl-command-map
          ("k" . diff-hl-previous-hunk)
          ("j" . diff-hl-next-hunk)
          ("x" . diff-hl-revert-hunk)
          ("s" . diff-hl-show-hunk)
          ("S" . diff-hl-stage-current-hunk)
          ("U" . diff-hl-unstage-file))
  :config
  (setq-default fringes-outside-margins t)
  (unless (display-graphic-p) (diff-hl-margin-mode))

  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)))

(use-package git-modes
  :mode (("\\.gitconfig.local\\'" . gitconfig-mode)))

(use-package git-link
  :commands git-link
  :custom
  (git-link-open-in-browser t))

(use-package git-timemachine
  :defines (git-timemachine-mode-map)
  :bind (("C-x v t" . git-timemachine)))

(use-package smerge-mode
  :ensure nil
  :bind-keymap ("C-c s" . smerge-basic-map)
  :bind (:repeat-map my/smerge-basic-map
          ("n" . smerge-next)
          ("p" . smerge-prev)
          ("a" . smerge-keep-all)
          ("c" . smerge-keep-current)
          ("u" . smerge-keep-upper)
          ("l" . smerge-keep-lower))
  :custom
  (smerge-command-prefix "none"))

(provide 'init-git)
;;; init-git.el ends here
