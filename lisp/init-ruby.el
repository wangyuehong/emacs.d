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

(use-package rspec-mode
  :commands rspec-install-snippets
  :hook (ruby-mode . rspec-mode)
  :config
  (rspec-install-snippets))

(provide 'init-ruby)
