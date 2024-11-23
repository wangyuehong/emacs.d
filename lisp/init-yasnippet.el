;; init-yasnippet.el --- yasnippet config. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :after (yasnippet cape))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
