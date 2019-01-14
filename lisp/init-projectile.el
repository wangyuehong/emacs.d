;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :diminish
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix "P ")
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-sort-order 'recentf)
  (setq projectile-use-git-grep t)
  (setq projectile-git-submodule-command nil)
  :config
  (projectile-update-mode-line) ; Update mode-line at the first time
  )

(provide 'init-projectile)
;;; init-projectile.el ends here
