;;; init-im.el --- Input Method configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package im-bridge
  :ensure nil ;; site-lisp/im-bridge
  :hook
  ((after-init . imb-evil-mode)))

(provide 'init-im)
;;; init-im.el ends here
