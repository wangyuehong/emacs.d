;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package magit
  :custom
  (magit-commit-show-diff nil)
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
         ("S" . diff-hl-stage-current-hunk)
         ("U" . diff-hl-unstage-file))
  :config
  (diff-hl-flydiff-mode 1)

  (setq-default fringes-outside-margins t)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))

  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  (unbind-key "n" diff-hl-command-map)
  (unbind-key "[" diff-hl-command-map)
  (unbind-key "]" diff-hl-command-map))

(use-package git-timemachine
  :commands git-timemachine
  :config
  (set-face-background 'git-timemachine-minibuffer-detail-face "black")
  :hook ((git-timemachine-mode . (lambda ()
                                   "improve `git-timemachine' buffers."
                                   ;; display different colors in mode-line
                                   (if (facep 'mode-line-active)
                                       (face-remap-add-relative 'mode-line-active 'custom-state)
                                     (face-remap-add-relative 'mode-line 'custom-state))

                                   ;; display line numbers
                                   (and (derived-mode-p 'prog-mode 'yaml-mode 'protobuf-mode)
                                        (fboundp 'display-line-numbers-mode)
                                        (display-line-numbers-mode t))))))

(use-package git-modes)

(use-package git-link
  :commands git-link
  :custom
  (git-link-open-in-browser t))

(provide 'init-git)
