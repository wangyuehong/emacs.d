(require-package 'projectile)
(projectile-global-mode)

(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)
(setq projectile-switch-project-action 'projectile-dired)

(require-package 'helm-projectile)
(helm-projectile-on)

(provide 'init-projectile)
