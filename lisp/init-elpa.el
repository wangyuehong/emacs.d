;; -*- coding: utf-8; lexical-binding: t; -*-

(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))


(setq package-archives
      '(
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")
        ("org"   . "http://orgmode.org/elpa/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
        ))

(provide 'init-elpa)
