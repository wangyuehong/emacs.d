;;; init-clipboard.el --- Clipboard settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package xclip
  :commands (xclip-set-selection)
  :bind (("C-x C-x" . my/copy-to-clipboard))
  :preface
  (defun my/copy-to-clipboard-with-message (text &optional message-prefix)
    "Copy TEXT to clipboard and show a message.
MESSAGE-PREFIX is used to customize the message."
    (xclip-set-selection 'clipboard text)
    (message "%s: %s" (or message-prefix "Copied to clipboard") text))

  (defun my/copy-buffer-path-generic (style &optional with-prefix message-prefix)
    "Generic function to copy buffer path based on STYLE.
STYLE determines the path format (see `my/format-file-path').
WITH-PREFIX adds parent folder name when non-nil.
MESSAGE-PREFIX customizes the message shown."
    (when buffer-file-name
      (condition-case err
          (let ((path (if (eq style 'display)
                          (my/get-buffer-display-path)
                        (my/format-file-path style with-prefix))))
            (my/copy-to-clipboard-with-message path message-prefix))
        (error (message "Error: %s" (error-message-string err))))))

  (defun my/copy-buffer-path ()
    "Copy the buffer's file path to the clipboard.
If the file is part of a Git repository, copy the relative path;
otherwise, copy the full absolute path."
    (interactive)
    (my/copy-buffer-path-generic 'display))

  (defun my/copy-buffer-absolute-path ()
    "Copy the buffer's absolute file path to the clipboard."
    (interactive)
    (my/copy-buffer-path-generic 'absolute nil "Copied absolute path to clipboard"))

  (defun my/copy-buffer-file-name ()
    "Copy the buffer's file name (without directory) to the clipboard."
    (interactive)
    (my/copy-buffer-path-generic 'filename nil "Copied file name to clipboard"))


  (defun my/copy-buffer-git-path (&optional with-prefix)
    "Copy the buffer's Git-relative path to the clipboard.
With prefix argument or WITH-PREFIX non-nil, include repository name."
    (interactive "P")
    (my/copy-buffer-path-generic 'git with-prefix))

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

  (defun my/get-git-root ()
    "Get the Git repository root directory for the current buffer.
Returns nil if not in a Git repository."
    (when buffer-file-name
      (let ((git-root (locate-dominating-file buffer-file-name ".git")))
        (when git-root
          (expand-file-name git-root)))))


  (defun my/get-git-repo-name ()
    "Get the name of the Git repository."
    (when-let ((git-root (my/get-git-root)))
      (file-name-nondirectory
        (directory-file-name git-root))))

  (defun my/format-file-path (style &optional with-prefix)
    "Format the current buffer's file path according to STYLE.
STYLE can be:
  'absolute - Full absolute path
  'git - Relative to Git root
  'filename - Just the filename
WITH-PREFIX adds the parent folder name when non-nil."
    (unless buffer-file-name
      (error "Current buffer is not visiting a file"))

    (let ((absolute-path (file-truename buffer-file-name)))
      (pcase style
        ('absolute absolute-path)
        ('filename (file-name-nondirectory absolute-path))
        ('git
         (let ((git-root (my/get-git-root)))
           (if git-root
               (let ((rel-path (file-relative-name absolute-path git-root)))
                 (if with-prefix
                     (concat (my/get-git-repo-name) "/" rel-path)
                     rel-path))
             (error "Not in a Git repository"))))
        (_ (error "Invalid style: %s" style)))))

  (defun my/get-buffer-display-path ()
    "Return the buffer's display path.
Relative for Git repository files, absolute for others, or buffer name if no file."
    (let ((git-root (my/get-git-root)))
      (cond
        ((and buffer-file-name git-root) ; Git repository file
          (file-relative-name buffer-file-name git-root))
        (buffer-file-name ; Non-Git file
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

  (defun my/get-region-or-line ()
    "Get the active region boundaries or current line if no region.
Returns a list of (start end) positions."
    (if (and (use-region-p) (> (region-end) (region-beginning)))
        (let ((start (region-beginning))
              (end (region-end)))
          ;; Move end to end of previous line to avoid including extra line
          (save-excursion
            (goto-char end)
            (setq end (line-end-position 0))) ; 0 means previous line
          (list start end))
      (list (line-beginning-position) (line-end-position))))

  (defun my/save-buffer-if-modified ()
    "Save buffer if it has been modified and return save message."
    (if (and buffer-file-name (buffer-modified-p))
        (progn
          (save-buffer)
          "Saved buffer. ")
      ""))

  (defun my/format-region-with-fence (text)
    "Format TEXT with appropriate Markdown code fence.
Automatically adjusts fence length based on content."
    (let* ((max-inner-ticks (my/find-max-backtick-sequence text))
           (fence-len (max 3 (1+ max-inner-ticks))))
      (make-string fence-len ?\`)))

  (defun my/copy-region-with-location-internal (location-path)
    "Internal function to copy region with location.
LOCATION-PATH is the formatted file path to use."
    (let* ((bounds (my/get-region-or-line))
           (start (car bounds))
           (end (cadr bounds))
           (selected-text (buffer-substring-no-properties start end))
           (start-line (line-number-at-pos start))
           (end-line (line-number-at-pos end))
           (saved-message (my/save-buffer-if-modified))
           (fence (my/format-region-with-fence selected-text))
           (location-string (if (= start-line end-line)
                               (format "@%s#L%d" location-path start-line)
                             (format "@%s#L%d-L%d" location-path start-line end-line)))
           (final-string (format "%s\n%s\n%s\n%s"
                                location-string
                                fence
                                selected-text
                                fence)))
      (xclip-set-selection 'clipboard final-string)
      (message "%sCopied %s with location: %s"
               saved-message
               (if (use-region-p) "region" "line")
               location-string)
      (when (use-region-p)
        (deactivate-mark))))

  (defun my/copy-region-with-formatted-location (style &optional with-prefix)
    "Copy the selected region with formatted location to clipboard.
STYLE can be 'absolute, 'git, or 'filename.
WITH-PREFIX adds parent folder name when non-nil."
    (let ((location-path (condition-case err
                            (my/format-file-path style with-prefix)
                          (error (my/format-file-path 'absolute)))))
      (my/copy-region-with-location-internal location-path)))

  (defun my/copy-region-with-location ()
    "Copy the selected region text and its start location to clipboard.
Uses relative path for Git repository files, auto-saves modified files,
and formats the output as a Markdown code block, adjusting backtick fence length as needed."
    (interactive)
    (my/copy-region-with-location-internal (my/get-buffer-display-path)))

  (defun my/copy-region-with-absolute-location ()
    "Copy region with absolute file path location."
    (interactive)
    (my/copy-region-with-formatted-location 'absolute))

  (defun my/copy-region-with-git-location (&optional with-prefix)
    "Copy region with Git-relative path location.
With prefix argument or WITH-PREFIX non-nil, include repository name."
    (interactive "P")
    (my/copy-region-with-formatted-location 'git with-prefix))

  (defun my/copy-region-with-filename-location ()
    "Copy region with filename-only location."
    (interactive)
    (my/copy-region-with-formatted-location 'filename)))

(provide 'init-clipboard)
;;; init-clipboard.el ends here
