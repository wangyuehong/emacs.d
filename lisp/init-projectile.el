;;; init-projectile.el --- Use Projectile for navigation within projects -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :hook (after-init . projectile-mode)
  :init
  (setq projectile-mode-line-prefix " P")
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-sort-order 'recentf)
  (setq projectile-git-submodule-command nil)
  (setq projectile-indexing-method 'hybrid)
  ;; (setq projectile-enable-caching t)
  ;; (setq projectile-files-cache-expire 3600)
  :config
  (projectile-update-mode-line) ; Update mode-line at the first time
  (after-load 'ag
    (setq ag-project-root-function (lambda (f) (projectile-project-root))))
  (projectile-register-project-type 'go '("go.mod"))
  )

(provide 'init-projectile)
;;; init-projectile.el ends here
