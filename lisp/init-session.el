;; -*- coding: utf-8; lexical-binding: t; -*-

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1)
  (setq desktop-path (list user-emacs-directory)
        desktop-auto-save-timeout 5))

(use-package session
  :hook (after-init . session-initialize)
  :init
  (setq session-globals-max-size 128)
  (setq session-globals-max-string (* 1 1024 1024)) ;; 1M
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  )

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :init (setq recentf-max-saved-items 64
              recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
              recentf-exclude
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
  :init (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
              savehist-file (expand-file-name ".savehist" user-emacs-directory)
              savehist-additional-variables '(mark-ring
                                              global-mark-ring
                                              search-ring
                                              regexp-search-ring
                                              extended-command-history)
              savehist-autosave-interval 300))
(provide 'init-session)
