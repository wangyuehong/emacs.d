(require-package 'helm)
(require 'helm-config)

(global-set-key (kbd "C-x C-b") 'helm-mini)

(require-package 'helm-cmd-t)

(global-set-key (kbd "C-c t") 'helm-cmd-t)
(provide 'init-helm)