;; init-dired.el --- dired configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'dired+)
(toggle-diredp-find-file-reuse-dir 1)
(setq ;; Always copy/delete recursively
 dired-recursive-copies  'always
 dired-recursive-deletes 'top
 dired-dwim-target t
 )

(use-package diredfl
  :config
  (diredfl-global-mode))

(defun +dired|sort-directories-first ()
  "List directories first in dired buffers."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))
(add-hook 'dired-after-readin-hook #'+dired|sort-directories-first)

;; Automatically create missing directories when creating new files
(defun +dired|create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))
(push #'+dired|create-non-existent-directory find-file-not-found-functions)

(provide 'init-dired)
;;; init-dired.el ends here
