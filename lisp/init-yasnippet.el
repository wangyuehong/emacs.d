;; init-yasnippet.el --- yasnippet config. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (window-setup . yas-global-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(use-package yasnippet-capf
  :after (yasnippet cape))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
