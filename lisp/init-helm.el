;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package helm
  :diminish
  :straight t
  :bind (("M-x" . helm-M-x)
         ("C-x j j" . helm-bookmarks)
         ("C-x b" . helm-mini)
         ("C-x C-f" . helm-find-files)
         :map helm-map
         ("C-j" . helm-next-line)
         ("C-k" . helm-previous-line)
         ("C-f" . helm-next-page)
         ("C-b" . helm-previous-page)
         ("C-e" . helm-execute-persistent-action))
  :commands (helm-autoresize-mode)
  :config
  (helm-mode 1)
  (helm-autoresize-mode t)
  :custom
  (helm-quick-update t)
  (helm-input-idle-delay 0.01)
  (helm-move-to-line-cycle-in-source)
  (helm-reuse-last-window-split-state t)
  (helm-always-two-windows            t)
  (helm-autoresize-max-height 40)
  (helm-autoresize-min-height 20)
  (helm-split-window-inside-p t))

(use-package helm-ls-git
  :straight t
  :custom
  (helm-ls-git-status-command 'magit-status-internal)
  (helm-ls-git-show-abs-or-relative 'relative)
  (helm-ff-transformer-show-only-basename nil)
  (helm-ls-git-default-sources '(helm-source-ls-git-status
                                 helm-source-ls-git-buffers
                                 helm-source-ls-git)))

(use-package helm-ag
  :straight t
  :custom
  (helm-ag-base-command "rg --no-heading --line-number --color never")
  (helm-ag-insert-at-point 'symbol)
  (helm-ag-use-temp-buffer t))

(use-package helm-swoop
  :straight t
  :bind (("M-i" . helm-swoop)
         ("C-c M-i" . helm-multi-swoop))
  :config
  (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
  (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)
  (setq helm-multi-swoop-edit-save t
        helm-swoop-speed-or-color nil))

(provide 'init-helm)
