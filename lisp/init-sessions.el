;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package session
  :hook (after-init . session-initialize)
  :init
  (setq session-globals-max-size 2048)
  (setq session-globals-max-string (* 8 1024 1024)) ;; 8M
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  (setq session-globals-include '(kill-ring
                                  (session-file-alist 100 t)
                                  my-dired-commands-history
                                  file-name-history
                                  search-ring
                                  regexp-search-ring))
  )

(use-package recentf
  :ensure nil
  :bind (("C-x C-r" . recentf-open-files))
  :hook (after-init . recentf-mode)
  :init
  (setq-default
   recentf-save-file (expand-file-name ".recentf" user-emacs-directory)
   recentf-max-saved-items 1000
   recentf-exclude '("/tmp/" "/ssh:"))
  (setq recentf-max-saved-items 300
        recentf-exclude
        '("\\.?cache" ".cask" "url" "COMMIT_EDITMSG\\'" "bookmarks"
          "\\.\\(?:gz\\|gif\\|svg\\|png\\|jpe?g\\|bmp\\|xpm\\)$"
          "\\.?ido\\.last$" "\\.revive$" "/G?TAGS$" "/.elfeed/"
          "^/tmp/" "^/var/folders/.+$" ; "^/ssh:"
          (lambda (file) (file-in-directory-p file package-user-dir))))
  :config
  (push (expand-file-name recentf-save-file) recentf-exclude))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :init
  (setq enable-recursive-minibuffers t ; Allow commands in minibuffers
        history-length 1000
        savehist-additional-variables '(mark-ring
                                        global-mark-ring
                                        search-ring
                                        regexp-search-ring
                                        extended-command-history)
        savehist-autosave-interval 300))

(provide 'init-sessions)
;;; init-sessions.el ends here