(add-hook 'after-init-hook 'recentf-mode)
(setq-default
 recentf-max-saved-items 100
 recentf-exclude '("/tmp/" "/ssh:"))

(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))

(provide 'init-recentf)
