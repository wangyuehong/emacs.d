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

(eval-after-load "magit"
  '(progn
     (define-key magit-mode-map (kbd "k") 'previous-line)
     (define-key magit-mode-map (kbd "K") 'magit-discard)
     (define-key magit-mode-map (kbd "j") 'next-line)
     (define-key magit-mode-map (kbd "C-f") 'scroll-up)
     (define-key magit-mode-map (kbd "C-b") 'scroll-down)

     (define-key magit-file-section-map (kbd "j") 'next-line)
     (define-key magit-file-section-map (kbd "k") 'previous-line)
     (define-key magit-file-section-map (kbd "K") 'magit-discard)

     (define-key magit-hunk-section-map (kbd "j") 'next-line)
     (define-key magit-hunk-section-map (kbd "k") 'previous-line)
     (define-key magit-hunk-section-map (kbd "K") 'magit-discard)

     (define-key magit-unstaged-section-map (kbd "j") 'next-line)
     (define-key magit-unstaged-section-map (kbd "k") 'previous-line)
     (define-key magit-unstaged-section-map (kbd "K") 'magit-discard)

     (define-key magit-staged-section-map (kbd "j") 'next-line)
     (define-key magit-staged-section-map (kbd "k") 'previous-line)
     (define-key magit-staged-section-map (kbd "K") 'magit-discard)

     (define-key magit-stash-section-map (kbd "j") 'next-line)
     (define-key magit-stash-section-map (kbd "k") 'previous-line)
     (define-key magit-stash-section-map (kbd "K") 'magit-stash-drop)

     (define-key magit-stashes-section-map (kbd "j") 'next-line)
     (define-key magit-stashes-section-map (kbd "k") 'previous-line)
     (define-key magit-stashes-section-map (kbd "K") 'magit-stash-clear)

     (define-key magit-untracked-section-map (kbd "j") 'next-line)
     (define-key magit-untracked-section-map (kbd "k") 'previous-line)
     (define-key magit-untracked-section-map (kbd "K") 'magit-discard)

     (define-key magit-branch-section-map (kbd "j") 'next-line)
     (define-key magit-branch-section-map (kbd "k") 'previous-line)
     (define-key magit-branch-section-map (kbd "K") 'magit-branch-delete)

     (define-key magit-remote-section-map (kbd "j") 'next-line)
     (define-key magit-remote-section-map (kbd "k") 'previous-line)
     (define-key magit-remote-section-map (kbd "K") 'magit-remote-remove)

     (define-key magit-tag-section-map (kbd "j") 'next-line)
     (define-key magit-tag-section-map (kbd "k") 'previous-line)
     (define-key magit-tag-section-map (kbd "K") 'magit-tag-delete)

     (define-key magit-diff-mode-map (kbd "j") 'next-line)
     (define-key magit-diff-mode-map (kbd "k") 'previous-line)
     (define-key magit-diff-mode-map (kbd "h") 'left-char)
     (define-key magit-diff-mode-map (kbd "l") 'right-char)
     ;; (define-key magit-diff-mode-map (kbd "C-f") 'scroll-up)
     ;; (define-key magit-diff-mode-map (kbd "C-b") 'scroll-down)


     ;; (define-key magit-log-mode-map (kbd "j") 'next-line)
     ;; (define-key magit-log-mode-map (kbd "k") 'previous-line)
     ;; (define-key magit-log-mode-map (kbd "C-f") 'scroll-up)
     ;; (define-key magit-log-mode-map (kbd "C-b") 'scroll-down)
     ))

;; change magit diff colors
;; (eval-after-load 'magit
;;   '(progn
;;      (set-face-attribute 'magit-diff-add nil
;;                          :inherit 'default
;;                          :background "#cefece"
;;                          :foreground "black")
;;      (set-face-attribute 'magit-diff-del nil
;;                          :inherit 'default
;;                          :background "#f4c6c6"
;;                          :foreground "black")
;;      (set-face-attribute 'diff-refine-added nil
;;                          :inherit 'default
;;                          :inverse-video 'nil
;;                          :background "#cefece"
;;                          :foreground "red")
;;      (set-face-attribute 'diff-refine-removed nil
;;                          :inherit 'default
;;                          :inverse-video 'nil
;;                          :background "#f4c6c6"
;;                          :foreground "green")
;;      ))

;;; When we start working on git-backed files, use git-wip if available

;; (after-load 'magit
;;   (global-magit-wip-save-mode)
;;   (diminish 'magit-wip-save-mode))

;; (after-load 'magit
;;   (diminish 'magit-auto-revert-mode))


(require-package 'git-gutter-fringe)

(setq git-gutter:update-threshold 2)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))

(define-prefix-command 'git-gutter-map)
(global-set-key (kbd "C-x g") 'git-gutter-map)

(if (window-system)
    ;; window-system
    (progn
      (require 'git-gutter-fringe)
      (global-git-gutter-mode t))
  ;; terminal
  (progn
    (require 'git-gutter)
    (global-git-gutter-mode t)
    (git-gutter:linum-setup)
    )
  )

(define-key git-gutter-map "j" 'git-gutter:next-hunk)
(define-key git-gutter-map "k" 'git-gutter:previous-hunk)
(define-key git-gutter-map "p" 'git-gutter:popup-hunk)
(define-key git-gutter-map "v" 'git-gutter:revert-hunk)

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



(provide 'init-git)
