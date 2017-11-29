(require-package 'helm)
(require 'helm-config)

(setq helm-completing-read-handlers-alist
      '((dired-do-copy . nil)
        (dired-do-rename . nil)
        (dired-create-directory . nil)
        ))

(eval-after-load 'helm
  '(progn
     (define-key helm-map (kbd "C-h") 'helm-previous-source)
     (define-key helm-map (kbd "C-l") 'helm-next-source)
     (define-key helm-map (kbd "C-j") 'helm-next-line)
     (define-key helm-map (kbd "C-k") 'helm-previous-line)
     (define-key helm-map (kbd "C-f") 'helm-next-page)
     (define-key helm-map (kbd "C-b") 'helm-previous-page)))

(require-package 'helm-ls-git)
(require 'helm-ls-git)
(setq helm-ls-git-show-abs-or-relative 'relative)
(setq helm-ff-transformer-show-only-basename nil)

(provide 'init-helm)
