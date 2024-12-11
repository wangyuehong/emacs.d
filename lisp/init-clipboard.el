;;; init-clipboard.el --- Clipboard settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package xclip
  :commands (xclip-set-selection)
  :bind (("C-x C-x" . my/copy-to-clipboard))
  :preface
  (defun my/copy-buffer-fullpath ()
    "Copy full path into the yank ring and OS clipboard"
    (interactive)
    (when buffer-file-name
      (xclip-set-selection 'clipboard (file-truename buffer-file-name))
      (message "copied to clipboard!")))

  (defun my/copy-to-clipboard ()
    "Copy selected-string-or-current-line to clipboard."
    (interactive)
    (let* ((in-region (region-active-p))
           (thing (if in-region
                      (buffer-substring-no-properties (region-beginning) (region-end))
                    (thing-at-point 'line))))
      (xclip-set-selection 'clipboard thing)
      (if in-region (deactivate-mark)))
    (message "copied to clipboard!")))

(provide 'init-clipboard)
;;; init-clipboard.el ends here
