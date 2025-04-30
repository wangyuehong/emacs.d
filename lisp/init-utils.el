;;; init-utils.el --- Utility functions and configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package open-junk-file
  :custom
  (open-junk-file-format "~/junk/%Y-%m-%dT%H-%M-%S.md"))

(defun my/open-file-in-vscode ()
  "Open the current file in Visual Studio Code and jump to the current position."
  (interactive)
  (if (buffer-file-name)
    (let ((file-path (buffer-file-name))
           (line-num (number-to-string (line-number-at-pos)))
           (col-num (number-to-string (current-column))))
      (shell-command (format "code --goto %s:%s:%s" (shell-quote-argument file-path) line-num col-num)))
    (message "Buffer is not visiting a file.")))

(defun my/open-file-in-typora ()
  "Open the current file in Typora."
  (interactive)
  (if-let* ((file-path (buffer-file-name)))
    (shell-command (format "open -a Typora %s" (shell-quote-argument file-path)))
    (message "Buffer is not visiting a file.")))

(defun my/open-in-finder ()
  "Open the current directory in Finder."
  (interactive)
  (when-let* ((dir (cond
                    ((eq major-mode 'dired-mode) (expand-file-name default-directory))
                    ((buffer-file-name) (file-name-directory (buffer-file-name)))
                    (t (expand-file-name default-directory)))))
    (shell-command (format "open %s" (shell-quote-argument dir)))))

(defun my/popup-visible-p ()
  "Return non-nil if any popup.el instance is currently visible."
  (when (and (boundp 'popup-instances)
             (fboundp 'popup-live-p)
             (fboundp 'popup-hidden-p))
    (cl-some (lambda (popup)
               (and popup
                    (popup-live-p popup)
                    (not (popup-hidden-p popup))))
             popup-instances)))

(provide 'init-utils)
;;; init-utils.el ends here
