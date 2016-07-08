(when (maybe-require-package 'projectile)
  (add-hook 'after-init-hook 'projectile-global-mode)
  )

(after-load 'projectile
  (setq projectile-completion-system 'helm)
  (setq projectile-enable-caching t)
  (setq projectile-switch-project-action 'projectile-dired)

  (require-package 'helm-projectile)
  (helm-projectile-on)

  (setq-default
   projectile-mode-line
   '(:eval
     (if (file-remote-p default-directory)
         " Pr"
       (format " Pr[%s]" (projectile-project-name)))))
  )

(provide 'init-projectile)
