(require-package 'ctags)
(require 'ctags)

(global-set-key (kbd "<f9>") 'ctags-create-or-update-tags-table)
(setq tags-revert-without-query t)

(when *is-a-mac*
  (setq ctags-command "/usr/local/bin/ctags -e -R "))

(provide 'init-ctags)
