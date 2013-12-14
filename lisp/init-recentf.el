(recentf-mode 1)
(setq recentf-max-saved-items 1000
      recentf-exclude '("/tmp/" "/ssh:" ".emacs.d/elpa/"))

(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))

(provide 'init-recentf)
