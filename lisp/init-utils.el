;;; init-utils.el --- Utility functions and configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(require 'cl-lib)
(require 'seq)

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
                     ((eq major-mode 'dired-mode)
                       (expand-file-name default-directory))
                     ((buffer-file-name)
                       (file-name-directory (buffer-file-name)))
                     (t
                       (expand-file-name default-directory)))))
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

(defun my/quit-window-dwim ()
  "DWIM `quit-window'.

- single visible window  -> run `quit-window'.
- multiple visible windows -> ask the user which one to close (delete).

If at least one window shows a non‑file buffer,
the first such window becomes the *default* selection; otherwise the default
is the currently selected window."
  (interactive)
  (let ((wins (window-list nil 'no-minibuffer)))
    (if (= (length wins) 1)
      (quit-window)
      ;; Build (display . window) pairs
      (let* ((pairs
               (cl-loop for w in wins
                 for idx from 1
                 for buf   = (window-buffer w)
                 for name  = (buffer-name buf)
                 for disp  = (format "%s %s <%d>"
                               (if (eq w (selected-window)) "●" " ")
                               name idx)
                 collect (cons disp w)))
              ;; Window list that show non-file buffers
              (nonfile-wins
                (seq-filter (lambda (w)
                              (not (buffer-file-name (window-buffer w))))
                  wins))
              ;; Choose default window: first non-file or the current one
              (default-win (or (car nonfile-wins) (selected-window)))
              ;; Find its display string
              (default (car (seq-find (lambda (p) (eq (cdr p) default-win))
                              pairs)))
              ;; Prompt user
              (choice  (completing-read "Close window: "
                         (mapcar #'car pairs)
                         nil t nil nil default))
              (target  (alist-get choice pairs nil nil #'string=)))
        (when (window-live-p target)
          (delete-window target))))))

(provide 'init-utils)
;;; init-utils.el ends here
