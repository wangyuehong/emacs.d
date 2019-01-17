;;; init-helm.el --- helm configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package helm
  :config
  (define-key helm-map (kbd "C-h") 'helm-previous-source)
  (define-key helm-map (kbd "C-l") 'helm-next-source)
  (define-key helm-map (kbd "C-j") 'helm-next-line)
  (define-key helm-map (kbd "C-k") 'helm-previous-line)
  (define-key helm-map (kbd "C-f") 'helm-next-page)
  (define-key helm-map (kbd "C-b") 'helm-previous-page)
  )

(use-package helm-ls-git
  :after helm
  :commands helm-ls-git-ls
  :defer t
  :init
  (setq helm-ls-git-show-abs-or-relative 'relative)
  (setq helm-ff-transformer-show-only-basename nil)
  )

(provide 'init-helm)
;;; init-helm.el ends here