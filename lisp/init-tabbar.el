(require-package 'tabbar)

(require-package 'tabbar-ruler)

(setq tabbar-ruler-global-tabbar t)
(require 'tabbar-ruler)

;; conflict with evil-mode
;;(global-set-key (kbd "C-c t") 'tabbar-ruler-move)

(provide 'init-tabbar)
