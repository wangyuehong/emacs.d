;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package magit
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
  :custom
  (git-gutter:update-hooks '(after-save-hook after-revert-hook))
  :config
  (global-git-gutter-mode t)
  :pretty-hydra
  ;; git-gutter-hydra
  ((:title "git-gutter" :foreign-keys warn :color amaranth :quit-key "q")
    ("Hunk"
     (("j" git-gutter:next-hunk)
      ("k" git-gutter:previous-hunk)
      ("h" (progn (goto-char (point-min))
                  (git-gutter:next-hunk 1)))
      ("l" (progn (goto-char (point-min))
                  (git-gutter:previous-hunk 1))))
     "Action"
     (("g" git-gutter)
      ("v" git-gutter:revert-hunk)
      ("p" git-gutter:popup-hunk))
     ))
  )

(use-package gitignore-mode)
(use-package gitconfig-mode)

(provide 'init-git)
