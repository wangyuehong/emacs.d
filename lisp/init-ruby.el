;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package ruby-mode
  :ensure nil
  :init
  (setq-default
   ruby-use-encoding-map nil
   ruby-insert-encoding-magic-comment nil)
  :config
  (key-chord-define ruby-mode-map "--" "->")
  (key-chord-define ruby-mode-map "==" "=>"))

(use-package yard-mode
  :hook (ruby-mode . yard-mode))

(provide 'init-ruby)
