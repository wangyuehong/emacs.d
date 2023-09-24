;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package desktop
  :ensure nil
  :init
  (desktop-save-mode 1)
  :custom
  (desktop-load-locked-desktop t)
  (desktop-auto-save-timeout 60))

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 256)
  (recentf-auto-cleanup 'never)
  (recentf-save-file (expand-file-name ".recentf" user-emacs-directory))
  (recentf-exclude '("^/private/tmp/"
                     "^/var/folders/"
                     "^/tmp/"
                     "/ssh\\(x\\)?:"
                     "/su\\(do\\)?:"
                     "^/usr/include/"
                     "/TAGS\\'"
                     "/G?TAGS$"
                     "COMMIT_EDITMSG\\'")))

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
  (savehist-autosave-interval 300))

(provide 'init-session)
