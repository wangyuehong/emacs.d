;;; init-ui.el --- UI configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package nerd-icons
  :defines (nerd-icons/devicon-alist nerd-icons/octicon-alist)
  :config
  (setf (cdr (assoc "nf-dev-go"   nerd-icons/devicon-alist)) "\xe627")
  (setf (cdr (assoc "nf-oct-ruby" nerd-icons/octicon-alist)) "\xe23e"))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-project-detection 'project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-vcs-max-length 36)
  (doom-modeline-modal-icon t)
  (evil-normal-state-tag (propertize "[Normal]"))
  (evil-emacs-state-tag (propertize "[Emacs]"))
  (evil-insert-state-tag (propertize "[Insert]"))
  (evil-motion-state-tag (propertize "[Motion]"))
  (evil-visual-state-tag (propertize "[Visual]"))
  (evil-operator-state-tag (propertize "[Operator]"))
  (evil-replace-state-tag (propertize "[Replace")))

(use-package dashboard
  :hook ((after-init . dashboard-setup-startup-hook))
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-path-style 'truncate-middle)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-items '((recents . 5))))

(provide 'init-ui)
;;; init-ui.el ends here
