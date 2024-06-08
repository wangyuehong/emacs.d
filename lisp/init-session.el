;;; init-session.el --- Session management -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 32)
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
                     "/.bookmarks.el"
                     "COMMIT_EDITMSG\\'")))

(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (savehist-file (expand-file-name ".savehist" user-emacs-directory))
  (savehist-autosave-interval 300))

(provide 'init-session)
;;; init-session.el ends here
