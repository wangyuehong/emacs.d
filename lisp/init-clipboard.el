;;; init-clipboard.el --- Clipboard settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package xclip
  :commands (xclip-set-selection)
  :bind (("C-x C-x" . my/copy-to-clipboard))
  :preface
  (defun my/copy-to-clipboard ()
    "Copy the active region or the current line to the clipboard."
    (interactive)
    (let ((text-to-copy
            (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
              (buffer-substring-no-properties (line-beginning-position) (line-end-position)))))
      (xclip-set-selection 'clipboard text-to-copy)
      (message "Copied to clipboard!")
      (when (use-region-p)
        (deactivate-mark)))))

(provide 'init-clipboard)
;;; init-clipboard.el ends here
