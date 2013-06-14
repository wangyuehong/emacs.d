(require-package 'helm)
(require 'helm-config)
(global-set-key (kbd "C-x C-b") 'helm-mini)

(require-package 'helm-ls-git)
(require 'helm-ls-git)

(global-set-key (kbd "C-c l") 'helm-ls-git-ls)

(require-package 'helm-cmd-t)
(require 'helm-cmd-t)

(global-set-key (kbd "C-c t") 'helm-cmd-t)

(require-package 'helm-git-grep)
(require 'helm-git-grep)

(provide 'init-helm)
