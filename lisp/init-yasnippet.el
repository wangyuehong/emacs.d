;; init-yasnippet.el --- yasnippet config. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :hook (after-init . yas-global-mode))

(use-package yasnippet-snippets)

(use-package yasnippet-capf
  :after (yasnippet cape)
  :init
  (add-to-list 'completion-at-point-functions #'yasnippet-capf))

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
