(require-package 'helm)
(require 'helm-config)

(helm-mode 1)

(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x r h") 'helm-bookmarks)

(require-package 'helm-ls-git)
(require 'helm-ls-git)
(setq helm-ls-git-show-abs-or-relative 'relative)

(global-set-key (kbd "C-c l") 'helm-ls-git-ls)

(require-package 'helm-git-grep)
(require 'helm-git-grep)

(provide 'init-helm)
