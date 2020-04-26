;;; init-ibuffer.el --- ibuffer settings -*- lexical-binding: t -*-
;;; Commentary:

;; TODO: enhance ibuffer-fontification-alist
;;   See http://www.reddit.com/r/emacs/comments/21fjpn/fontifying_buffer_list_for_emacs_243/

;;; Code:
(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init
  (setq-default ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold)))
  ;; (setq ibuffer-filter-group-name-face 'font-lock-doc-face)
  )

(use-package fullframe
  :config
  (with-eval-after-load 'ibuffer
    (fullframe ibuffer ibuffer-quit))
  )

(use-package ibuffer-vc)

(provide 'init-ibuffer)
;;; init-ibuffer.el ends here
