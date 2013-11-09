(require-package 'helm)
(require 'helm-config)

;; (helm-mode 1)

(setq helm-completing-read-handlers-alist
      '((describe-function . ido)
        (describe-variable . ido)
        (dired-do-copy . nil)
        (dired-do-rename . nil)
        (dired-create-directory . nil)
        (ido-find-file . nil)
        (ido-edit-input . nil)
        (read-file-name . ido)
        (read-directory-name . ido)
        ))

(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x r h") 'helm-bookmarks)

(require-package 'helm-ls-git)
(require 'helm-ls-git)
(setq helm-ls-git-show-abs-or-relative 'relative)

(provide 'init-helm)
