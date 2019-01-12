;;; init-yasnippet.el --- yasnippet configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config (use-package yasnippet-snippets))

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(provide 'init-yasnippet)
;;; init-yasnippet ends here
