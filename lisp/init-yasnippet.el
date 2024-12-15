;; init-yasnippet.el --- yasnippet config. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :defines yas-keymap
  :hook (after-init . yas-global-mode)
  :bind
  (:map yas-keymap
    ("C-f" . yas-next-field-or-maybe-expand)
    ("C-b" . yas-prev-field)))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
