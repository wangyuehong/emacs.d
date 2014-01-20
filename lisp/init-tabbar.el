(require-package 'tabbar)

(require-package 'tabbar-ruler)

(setq tabbar-ruler-global-tabbar t)
(require 'tabbar-ruler)

;; grouping by projectile
(tabbar-ruler-group-by-projectile-project)

;; conflict with evil-mode
;;(global-set-key (kbd "C-c t") 'tabbar-ruler-move)

;; fix keybind conflict in dired-mode
(define-key dired-mode-map (kbd "M-l") 'tabbar-ruler-forward)

(global-set-key (kbd "M-h") 'tabbar-ruler-backward)
(global-set-key (kbd "M-l") 'tabbar-ruler-forward)
(global-set-key (kbd "M-k") 'tabbar-ruler-up)
(global-set-key (kbd "M-j") 'tabbar-ruler-up)

(provide 'init-tabbar)
