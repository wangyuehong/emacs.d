(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)
(require-package 'git-timemachine)
(require-package 'git-link)

(require-package 'magit)
(setq magit-last-seen-setup-instructions "1.4.0")

(setq-default
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

(require-package 'evil-magit)
(require 'evil-magit)

(after-load 'magit
  (add-hook 'magit-popup-mode-hook 'sanityinc/no-trailing-whitespace))

(require-package 'fullframe)
(after-load 'magit
  (fullframe magit-status magit-mode-quit-window))

(when (maybe-require-package 'git-commit)
  (add-hook 'git-commit-mode-hook 'goto-address-mode))

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
(define-key git-gutter-map "s" 'git-gutter:stage-hunk)

(require-package 'magit-gitflow)
(add-hook 'magit-mode-hook 'turn-on-magit-gitflow)

(require-package 'git-messenger)
;; {{ git-messenger
;; show details to play `git blame' game
(setq git-messenger:show-detail t)
(add-hook 'git-messenger:after-popup-hook
          (lambda (msg)
            ;; extract commit id and put into the kill ring
            (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
              ;; get the first 7 characters as hash because that's `git log' use
              (let ((hash (substring-no-properties (match-string 2 msg) 0 7)))
                (copy-yank-str hash)
                (message "commit hash %s => clipboard & kill-ring" hash)))))
(global-set-key (kbd "C-x v p") 'git-messenger:popup-message)
;; }}

(provide 'init-git)
