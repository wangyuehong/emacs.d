(require-package 'magit)
(require-package 'git-gutter-fringe)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

(after-load 'magit
  ;; Don't let magit-status mess up window configurations
  ;; http://whattheemacsd.com/setup-magit.el-01.html
  (defadvice magit-status (around magit-fullscreen activate)
    (window-configuration-to-register :magit-fullscreen)
    ad-do-it
    (delete-other-windows))

  (defun magit-quit-session ()
    "Restores the previous window configuration and kills the magit buffer"
    (interactive)
    (kill-buffer)
    (when (get-register :magit-fullscreen)
      (ignore-errors
        (jump-to-register :magit-fullscreen))))

  (define-key magit-status-mode-map (kbd "q") 'magit-quit-session))


;;; When we start working on git-backed files, use git-wip if available

(after-load 'vc-git
  (global-magit-wip-save-mode)
  (diminish 'magit-wip-save-mode))


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

(define-key git-gutter-map "j" 'git-gutter:next-hunk)
(define-key git-gutter-map "k" 'git-gutter:previous-hunk)
(define-key git-gutter-map "p" 'git-gutter:popup-hunk)
(define-key git-gutter-map "r" 'git-gutter:revert-hunk)
(define-key git-gutter-map "l" 'magit-file-log)

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



(provide 'init-git)
