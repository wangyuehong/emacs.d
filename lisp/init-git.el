(require-package 'magit)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

(require-package 'fullframe)
(fullframe magit-status magit-mode-quit-window :magit-fullscreen nil)

;;; When we start working on git-backed files, use git-wip if available

(after-load 'vc-git
  (global-magit-wip-save-mode)
  (diminish 'magit-wip-save-mode))


(require-package 'git-gutter-fringe)

(setq git-gutter:update-threshold 2)
(setq git-gutter:update-hooks '(after-save-hook after-revert-hook))
;; (setq git-gutter:diff-option "HEAD")

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
    ;; (global-git-gutter-mode t)
    (setq git-gutter:separator-sign "|")

    (define-key git-gutter-map "t" 'toggle-git-gutter-linum)))

(defun toggle-git-gutter-linum ()
  "toggle git-gutter and linum mode."
  (interactive)
  (if (or linum-mode git-gutter-mode)
      (if linum-mode
          (progn
              (linum-mode 0)
              (git-gutter-mode 1))
          (progn
              (git-gutter-mode 0)
              (linum-mode 1)))))

(defun change-git-gutter-diff-option (option)
  "set git-gutter:diff-option and reload"
  (setq git-gutter:diff-option option)
  (git-gutter))

(defun git-gutter-diff-none ()
  "git-gutter with git diff, no diff if staged"
  (interactive)
  (change-git-gutter-diff-option ""))

(defun git-gutter-diff-head ()
  "git-gutter with git HEAD, include diff of staged and unstaged"
  (interactive)
  (change-git-gutter-diff-option "HEAD"))

(defun git-gutter-diff-cached ()
  "git-gutter with git --cached, only show diff of staged"
  (interactive)
  (change-git-gutter-diff-option "--cached"))

(define-key git-gutter-map "j" 'git-gutter:next-hunk)
(define-key git-gutter-map "k" 'git-gutter:previous-hunk)
(define-key git-gutter-map "p" 'git-gutter:popup-hunk)
(define-key git-gutter-map "v" 'git-gutter:revert-hunk)
(define-key git-gutter-map "dn" 'git-gutter-diff-none)
(define-key git-gutter-map "dh" 'git-gutter-diff-head)
(define-key git-gutter-map "dc" 'git-gutter-diff-cached)
(define-key git-gutter-map "l" 'magit-file-log)

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



(provide 'init-git)