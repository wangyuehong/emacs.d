;;; init-ivy.el --- Use ivy for minibuffer completion and more -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :diminish ivy-mode
  :hook
  (after-init . ivy-mode)
  :init
  (setq-default ivy-use-virtual-buffers t
                ivy-virtual-abbreviate 'fullpath
                ivy-count-format ""
                projectile-completion-system 'ivy
                ivy-magic-tilde nil
                ivy-dynamic-exhibit-delay-ms 150
                ivy-initial-inputs-alist
                '((Man-completion-table . "^")
                  (woman . "^")))
  :config
  ;; IDO-style directory navigation
  (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
  (define-key ivy-minibuffer-map (kbd "C-k") #'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-b") #'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-f") #'ivy-scroll-up-command)
  ;; (define-key ivy-minibuffer-map (kbd "<up>") #'ivy-previous-line-or-history)
  ;; (define-key ivy-occur-mode-map (kbd "C-c C-q") #'ivy-wgrep-change-to-wgrep-mode)
  )

(use-package counsel
  :diminish counsel-mode
  :hook (after-init . counsel-mode)
  :init
  (setq-default counsel-mode-override-describe-bindings t)
  :bind (("C-x j j" . counsel-bookmark))
  )

(use-package swiper :after ivy)

(provide 'init-ivy)
;;; init-ivy.el ends here