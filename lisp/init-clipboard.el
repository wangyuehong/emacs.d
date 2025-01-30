;;; init-clipboard.el --- Clipboard settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package xclip
  :commands (xclip-set-selection)
  :bind (("C-x C-x" . my/copy-to-clipboard))
  :preface
  (defun my/copy-buffer-path ()
    "Copy the buffer's file path to the clipboard.
If the file is part of a project (managed by `project.el`), copy the relative path;
otherwise, copy the full absolute path."
    (interactive)
    (when buffer-file-name
      (let* ((proj (project-current))
              (proj-root (when proj (project-root proj)))
              (path (if proj-root
                      (file-relative-name buffer-file-name proj-root)
                      (file-truename buffer-file-name))))
        (xclip-set-selection 'clipboard path)
        (message "Copied to clipboard: %s" path))))

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
