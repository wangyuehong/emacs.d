;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package helm
  :bind (:map helm-map
              ("C-h" . helm-previous-source)
              ("C-l" . helm-next-source)
              ("C-j" . helm-next-line)
              ("C-k" . helm-previous-line)
              ("C-f" . helm-next-page)
              ("C-b" . helm-previous-page)))

(use-package helm-ls-git
  :commands helm-ls-git-ls
  :custom
  (helm-ls-git-show-abs-or-relative 'relative)
  (helm-ff-transformer-show-only-basename nil)
  )

(provide 'init-helm)
