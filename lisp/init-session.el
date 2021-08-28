;; -*- coding: utf-8; lexical-binding: t; -*-

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1)
  :custom
  (desktop-path (list user-emacs-directory))
  (desktop-auto-save-timeout 300)
  (desktop-globals-to-save
   '((comint-input-ring        . 50)
     (compile-history          . 30)
     desktop-missing-file-warning
     (dired-regexp-history     . 20)
     (extended-command-history . 30)
     (face-name-history        . 20)
     (file-name-history        . 100)
     (grep-find-history        . 30)
     (grep-history             . 30)
     (ivy-history              . 100)
     (magit-revision-history   . 50)
     (minibuffer-history       . 50)
     (query-replace-history    . 60)
     (read-expression-history  . 60)
     (regexp-history           . 60)
     (regexp-search-ring       . 20)
     register-alist
     (search-ring              . 20)
     (shell-command-history    . 50)
     tags-file-name
     tags-table-list)))

(use-package session
  :hook (after-init . session-initialize)
  :custom
  (session-globals-max-size 128)
  (session-globals-max-string (* 1 1024 1024)) ;; 1M
  (session-save-file (expand-file-name ".session" user-emacs-directory))
  (session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (session-save-file-coding-system 'utf-8)
  )

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 64)
  (recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
  (recentf-exclude
   '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
     "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
     "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
     "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
     (lambda (file) (file-in-directory-p file package-user-dir))))
  :config (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (savehist-file (expand-file-name ".savehist" user-emacs-directory))
  (savehist-additional-variables
   '(mark-ring
     global-mark-ring
     search-ring
     regexp-search-ring
     extended-command-history))
  (savehist-autosave-interval 300)
  )

(provide 'init-session)
