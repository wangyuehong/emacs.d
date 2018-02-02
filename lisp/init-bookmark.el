;;(require-package 'bookmark+)
(require 'bookmark+)

(setq bmkp-bmenu-state-file (expand-file-name ".emacs-bmk-bmenu-state.el" user-emacs-directory))
(setq bmkp-bmenu-commands-file (expand-file-name ".emacs-bmk-bmenu-commands.el" user-emacs-directory))

(provide 'init-bookmark)
