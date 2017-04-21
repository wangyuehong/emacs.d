(require-package 'magit)

(setq magit-last-seen-setup-instructions "1.4.0")

;; (after-load 'session
;;   (add-to-list 'session-mode-disable-list 'git-commit-mode))

(setq-default
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(require-package 'evil-magit)
(require 'evil-magit)

(require-package 'git-gutter)

(global-git-gutter-mode t)
(git-gutter:linum-setup)

(setq git-gutter:update-threshold 2)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

(define-prefix-command 'git-gutter-map)
(global-set-key (kbd "C-x g") 'git-gutter-map)

(define-key git-gutter-map "j" 'git-gutter:next-hunk)
(define-key git-gutter-map "k" 'git-gutter:previous-hunk)
(define-key git-gutter-map "p" 'git-gutter:popup-hunk)
(define-key git-gutter-map "v" 'git-gutter:revert-hunk)

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))

;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)

(require-package 'magit-gitflow)
(require 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(provide 'init-git)
