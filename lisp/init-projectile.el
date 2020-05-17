;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package projectile
  :bind (:map projectile-mode-map
              ("C-c p" . projectile-command-map))
  :hook (prog-mode . projectile-mode)
  :custom
  (projectile-mode-line-prefix " Proj")
  (projectile-switch-project-action #'projectile-dired)
  (projectile-sort-order 'recentf)
  (projectile-git-submodule-command nil)
  (projectile-indexing-method 'hybrid)
  (projectile-completion-system 'ivy)
  :config
  ;; (with-eval-after-load 'ag
  ;;   (setq ag-project-root-function (lambda (f) (projectile-project-root))))
  (projectile-register-project-type 'go '("go.mod"))
  )

(provide 'init-projectile)
