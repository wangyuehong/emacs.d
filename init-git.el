(require-package 'magit)
(require-package 'git-gutter-fringe)
(require-package 'git-blame)
(require-package 'git-commit-mode)
(require-package 'gitignore-mode)
(require-package 'gitconfig-mode)

(setq-default
 magit-save-some-buffers nil
 magit-process-popup-time 10
 magit-diff-refine-hunk t
 magit-completing-read-function 'magit-ido-completing-read)

(global-set-key [(meta f12)] 'magit-status)

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

(when *is-a-mac*
  (after-load 'magit
    (add-hook 'magit-mode-hook (lambda () (local-unset-key [(meta h)])))))



;; Convenient binding for vc-git-grep
(global-set-key (kbd "C-x v f") 'vc-git-grep)



;;; git-svn support

(after-load 'magit-key-mode
  (require 'magit-svn))

(after-load 'compile
  (dolist (defn (list '(git-svn-updated "^\t[A-Z]\t\\(.*\\)$" 1 nil nil 0 1)
                      '(git-svn-needs-update "^\\(.*\\): needs update$" 1 nil nil 2 1)))
    (add-to-list 'compilation-error-regexp-alist-alist defn)
    (add-to-list 'compilation-error-regexp-alist (car defn))))

(defvar git-svn--available-commands nil "Cached list of git svn subcommands")

(defun git-svn (dir)
  "Run a git svn subcommand in DIR."
  (interactive "DSelect directory: ")
  (unless git-svn--available-commands
    (setq git-svn--available-commands
          (string-all-matches "^  \\([a-z\\-]+\\) +" (shell-command-to-string "git svn help") 1)))
  (let* ((default-directory (vc-git-root dir))
         (compilation-buffer-name-function (lambda (major-mode-name) "*git-svn*")))
    (compile (concat "git svn "
                     (ido-completing-read "git-svn command: " git-svn--available-commands nil t)))))


(require-package 'git-messenger)
(global-set-key (kbd "C-x v p") #'git-messenger:popup-message)


;;; github

(require-package 'yagist)
(require-package 'github-browse-file)
(require-package 'bug-reference-github)
(add-hook 'prog-mode-hook 'bug-reference-prog-mode)



(provide 'init-git)
