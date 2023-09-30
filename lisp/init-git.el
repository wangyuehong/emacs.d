;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package magit
  :custom
  (magit-process-popup-time 10)
  (magit-ediff-dwim-show-on-hunks t)
  (magit-diff-refine-hunk t))

(use-package forge
  :after magit
  :init
  (setq auth-sources '("~/.authinfo")))

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
              ("v" . diff-hl-hydra/body))
  :config
  (diff-hl-flydiff-mode 1)
  (setq-default fringes-outside-margins t)
  (unless (display-graphic-p)
    (diff-hl-margin-mode 1))
  (with-eval-after-load 'magit
    (add-hook 'magit-pre-refresh-hook #'diff-hl-magit-pre-refresh)
    (add-hook 'magit-post-refresh-hook #'diff-hl-magit-post-refresh))

  :pretty-hydra
  ((:title "diff-hl" :foreign-keys warn :color amaranth :quit-key "q")
    ("Hunk"
     (("k" diff-hl-previous-hunk)
      ("j" diff-hl-next-hunk)
      ("p" diff-hl-show-hunk))
     "Action"
     (("v" diff-hl-revert-hunk)
      ("c" diff-hl-show-hunk-copy-original-text)))))

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
