(require-package 'projectile)
(projectile-global-mode)

(setq projectile-completion-system 'helm)
(setq projectile-enable-caching t)

(require-package 'helm-projectile)
(helm-projectile-on)

(provide 'init-projectile)
