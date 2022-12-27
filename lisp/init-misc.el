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

(use-package osx-dictionary)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-project-detection 'auto))

(provide 'init-misc)
