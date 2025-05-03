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

(defun my/quit-window-dwim ()
  "DWIM `quit-window'.

If the frame has only one non‑minibuffer window, run `quit-window'.
Otherwise list visible windows, let the user pick one, and delete it."
  (interactive)
  (let ((wins (window-list nil 'no-minibuffer)))
    (if (= (length wins) 1)
        (quit-window)
      ;; Build (display . window) pairs in one pass.
      (let* ((pairs
              (cl-loop for w in wins
                       for idx from 1
                       for disp = (format "%s %s <%d>"
                                          (if (eq w (selected-window)) "●" " ")
                                          (buffer-name (window-buffer w))
                                          idx)
                       collect (cons disp w)))
             ;; Default = pair whose cdr is the selected window.
             (default (car (seq-find (lambda (p) (eq (cdr p) (selected-window)))
                                     pairs)))
             (choice (completing-read "Close window: "
                                      (mapcar #'car pairs)
                                      nil t nil nil default))
             (target (alist-get choice pairs nil nil #'string=)))
        (when (window-live-p target)
          (delete-window target))))))

;; (defun my/quit-window-dwim ()
;;   "DWIM `quit-window' command.

;; - If the current frame has only one non‑minibuffer window, run `quit-window'.
;; - Otherwise list every visible window in the frame, let the user pick one
;;   with `completing-read', and delete that window with `delete-window'."
;;   (interactive)
;;   ;; Get all visible windows, excluding the minibuffer window.
;;   (let* ((wins  (window-list nil 'no-minibuffer))
;;          (nwins (length wins)))
;;     (if (= nwins 1)
;;         ;; Single window case: just bury buffer / close help, etc.
;;         (quit-window)
;;       ;; Multiple windows: build completion candidates.
;;       (let ((counter     1)     ; running index to disambiguate duplicates
;;             (candidates  nil)   ; list of display strings
;;             (mapping     nil)   ; alist (display-string . window)
;;             (default     nil))  ; default candidate (current window)
;;         ;; Construct candidate list.
;;         (dolist (w wins)
;;           (let* ((buf     (window-buffer w))
;;                  (name    (buffer-name buf))
;;                  ;; Mark the current window with a bullet.
;;                  (prefix  (if (eq w (selected-window)) "●" " "))
;;                  (display (format "%s %s <%d>" prefix name counter)))
;;             (setq candidates (cons display candidates))
;;             (setq mapping   (cons (cons display w) mapping))
;;             (when (eq w (selected-window))
;;               (setq default display))
;;             (setq counter (1+ counter))))
;;         ;; Restore original order.
;;         (setq candidates (nreverse candidates))
;;         (setq mapping    (nreverse mapping))
;;         ;; Prompt the user.
;;         (let* ((choice (completing-read
;;                         "Close window: "
;;                         candidates
;;                         nil         ; no predicate
;;                         t           ; require-match
;;                         nil nil
;;                         default))
;;                (target (cdr (assoc choice mapping))))
;;           ;; Delete the chosen window.
;;           (when (window-live-p target)
;;             (delete-window target)))))))

(provide 'init-utils)
;;; init-utils.el ends here
