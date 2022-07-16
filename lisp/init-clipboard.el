;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package xclip
  :commands (xclip-set-selection copy-fullpath-of-current-buffer)
  :bind (("C-x C-x" . copy-to-x-clipboard))
  :init
  (defun copy-fullpath-of-current-buffer ()
    "Copy full path into the yank ring and OS clipboard"
    (interactive)
    (when buffer-file-name
      (xclip-set-selection 'clipboard (file-truename buffer-file-name))
      (message "file full path => clipboard & yank ring")))

  (defun copy-to-x-clipboard ()
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
