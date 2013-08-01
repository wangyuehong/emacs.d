(require-package 'ctags)
(require 'ctags)

(global-set-key (kbd "<f9>") 'ctags-create-or-update-tags-table)
(setq tags-revert-without-query t)

(provide 'init-ctags)
