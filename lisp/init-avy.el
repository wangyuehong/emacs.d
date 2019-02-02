;;; init-avy.el --- avy configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package avy
  :init
  (setq
   avy-keys (string-to-list "asdfghjklqweruiopzxcv"))
  (setq avy-all-windows t)
  (setq avy-background t)
  (setq avy-highlight-first t)
  )
(provide 'init-avy)
;;; init-avy.el ends here
