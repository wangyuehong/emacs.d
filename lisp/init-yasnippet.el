;; init-yasnippet.el --- yasnippet config. -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Yasnippet configurations.
;;

;;; Code:

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode))

;; (use-package yasnippet-snippets)

(provide 'init-yasnippet)
