;;; init-sessions.el --- Save and restore editor sessions between restarts -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; save a list of open files in ~/.emacs.d/.emacs.desktop
(setq desktop-path (list user-emacs-directory)
      desktop-auto-save-timeout 600)
(desktop-save-mode 1)

(defun sanityinc/time-subtract-millis (b a)
  (* 1000.0 (float-time (time-subtract b a))))

(defun sanityinc/desktop-time-restore (orig &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig args)
      (message "Desktop restored in %.2fms"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)))))
(advice-add 'desktop-read :around 'sanityinc/desktop-time-restore)

(defun sanityinc/desktop-time-buffer-create (orig ver filename &rest args)
  (let ((start-time (current-time)))
    (prog1
        (apply orig ver filename args)
      (message "Desktop: %.2fms to restore %s"
               (sanityinc/time-subtract-millis (current-time)
                                               start-time)
               (when filename
                 (abbreviate-file-name filename))))))
(advice-add 'desktop-create-buffer :around 'sanityinc/desktop-time-buffer-create)

;;----------------------------------------------------------------------------
;; Restore histories and registers after saving
;;----------------------------------------------------------------------------
(setq-default history-length 100)
(add-hook 'after-init-hook 'savehist-mode)
(setq savehist-file (expand-file-name ".savehist" user-emacs-directory))

(use-package session
  :hook (after-init . session-initialize)
  :init
  (setq session-save-file (expand-file-name ".session" user-emacs-directory))
  (setq session-name-disable-regexp "\\(?:\\`'/tmp\\|\\.git/[A-Z_]+\\'\\)")
  (setq session-save-file-coding-system 'utf-8)
  )

;; save a bunch of variables to the desktop file
;; for lists specify the len of the maximal saved data also
(setq desktop-globals-to-save
      '((comint-input-ring        . 20)
        (compile-history          . 20)
        desktop-missing-file-warning
        (dired-regexp-history     . 20)
        (extended-command-history . 20)
        (face-name-history        . 20)
        (file-name-history        . 20)
        (grep-find-history        . 20)
        (grep-history             . 20)
        (ivy-history              . 20)
        (magit-revision-history   . 20)
        (minibuffer-history       . 20)
        (org-clock-history        . 10)
        (org-refile-history       . 10)
        (org-tags-history         . 10)
        (query-replace-history    . 20)
        (read-expression-history  . 10)
        (regexp-history           . 10)
        (regexp-search-ring       . 20)
        register-alist
        (search-ring              . 20)
        (shell-command-history    . 10)
        tags-file-name
        tags-table-list))

(provide 'init-sessions)
;;; init-sessions.el ends here