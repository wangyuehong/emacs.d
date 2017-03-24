(add-hook 'after-init-hook (lambda () (recentf-mode 1)))
(setq-default
 recentf-max-saved-items 1000
 recentf-exclude '("/tmp/" "/ssh:"))

(setq recentf-save-file (expand-file-name ".recentf" user-emacs-directory))

(provide 'init-recentf)
