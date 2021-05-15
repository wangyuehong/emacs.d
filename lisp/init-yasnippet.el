;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets :after yasnippet)

(provide 'init-yasnippet)
