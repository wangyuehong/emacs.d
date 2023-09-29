;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package ruby-mode
  :ensure nil
  :init
  (setq-default
   ruby-use-encoding-map nil
   ruby-insert-encoding-magic-comment nil))

(provide 'init-ruby)
