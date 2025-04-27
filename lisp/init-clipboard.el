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
      (let ((path (my/get-buffer-display-path)))
        (xclip-set-selection 'clipboard path)
        (message "Copied to clipboard: %s" path))))

  (defun my/copy-buffer-absolute-path ()
    "Copy the buffer's absolute file path to the clipboard."
    (interactive)
    (when buffer-file-name
      (let ((path (file-truename buffer-file-name)))
        (xclip-set-selection 'clipboard path)
        (message "Copied absolute path to clipboard: %s" path))))

  (defun my/copy-buffer-file-name ()
    "Copy the buffer's file name (without directory) to the clipboard."
    (interactive)
    (when buffer-file-name
      (let ((filename (file-name-nondirectory buffer-file-name)))
        (xclip-set-selection 'clipboard filename)
        (message "Copied file name to clipboard: %s" filename))))

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
        (deactivate-mark))))

  (defun my/get-buffer-display-path ()
    "Return the buffer's display path.
Relative for project files, absolute for others, or buffer name if no file."
    (let ((proj (project-current)))
      (cond
       ((and buffer-file-name proj) ; Project file
        (file-relative-name buffer-file-name (project-root proj)))
       (buffer-file-name ; Non-project file
        (file-truename buffer-file-name))
       (t ; Not file (e.g., *scratch*)
        (buffer-name)))))

  (defun my/find-max-backtick-sequence (text)
    "Return the length of the longest sequence of consecutive backticks in TEXT."
    (let ((max-len 0)
           (current-len 0)
           (len (length text)))
      (dotimes (i len)
        (if (eq (aref text i) ?\`)
          (setq current-len (1+ current-len))
          (setq max-len (max max-len current-len)
            current-len 0)))
      (max max-len current-len)))

  (defun my/copy-region-with-location ()
    "Copy the selected region text and its start location to clipboard.
Uses relative path for project files, auto-saves modified files,
and formats the output as a Markdown code block, adjusting backtick fence length as needed."
    (interactive)
    (if (and (use-region-p) (> (region-end) (region-beginning)))
        (let* ((start (region-beginning))
               (end (region-end))
               (selected-text (buffer-substring-no-properties start end))
               ;; buffer-file-name is used directly below
               (location-prefix (my/get-buffer-display-path))
              (start-line (line-number-at-pos start)) ; Directly use position
              (saved-message "")
              ;; --- Calculate fence ---
              (max-inner-ticks (my/find-max-backtick-sequence selected-text))
               (fence-len (max 3 (1+ max-inner-ticks)))
               (fence (make-string fence-len ?\`)))

          (when (and buffer-file-name (buffer-modified-p))
            (save-buffer)
            (setq saved-message "Saved buffer. "))

          (let* ((location-string (format "@%s:%d" location-prefix start-line))
                 (final-string (format "%s\n%s\n%s\n%s"
                                       location-string
                                       fence ; Opening fence
                                       selected-text
                                       fence))) ; Closing fence
            (xclip-set-selection 'clipboard final-string)
            (message "%sCopied region with location: %s" saved-message location-string)
            (deactivate-mark))) ; Deactivate region after copy
      (message "No active or non-empty region selected."))))

(provide 'init-clipboard)
;;; init-clipboard.el ends here
