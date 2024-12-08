;; init-yasnippet.el --- yasnippet config. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
