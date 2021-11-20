;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook
  ((text-mode outline-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)

  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package youdao-dictionary)
(use-package osx-dictionary)

(provide 'init-misc)
