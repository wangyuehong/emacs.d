(require-package 'tabbar)

(require-package 'tabbar-ruler)

(setq tabbar-ruler-global-tabbar t)
(require 'tabbar-ruler)

;; conflict with evil-mode
;;(global-set-key (kbd "C-c t") 'tabbar-ruler-move)

(global-set-key (kbd "M-h") 'tabbar-ruler-backward)
(global-set-key (kbd "M-l") 'tabbar-ruler-forward)
(global-set-key (kbd "M-k") 'tabbar-ruler-up)

(provide 'init-tabbar)
