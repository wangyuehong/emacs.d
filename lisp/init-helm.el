;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package helm
  :bind (("M-x" . helm-M-x)
         ("C-x j j" . helm-bookmarks)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ("C-h" . helm-previous-source)
         ("C-l" . helm-next-source)
         ("C-j" . helm-next-line)
         ("C-k" . helm-previous-line)
         ("C-f" . helm-next-page)
         ("C-b" . helm-previous-page)
         :map helm-find-files-map
         ("DEL" . helm-find-files-up-one-level)
         )
  :commands (helm-autoresize-mode)
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  :custom
  (helm-autoresize-max-height 36)
  (helm-autoresize-min-height 36)
  )

(use-package helm-ls-git
  :commands helm-ls-git-ls
  :custom
  (helm-ls-git-status-command 'magit-status-internal)
  (helm-ls-git-show-abs-or-relative 'relative)
  (helm-ff-transformer-show-only-basename nil)
  )

(provide 'init-helm)
