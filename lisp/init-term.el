;;; init-term.el --- Terminal emulator configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package emamux)

(defun my/set-terminal-title ()
  "Set terminal title to current buffer name."
  (send-string-to-terminal
   (format "\033]2;EMACS: %s\033\\"
           (if buffer-file-name
               (file-name-nondirectory buffer-file-name)
             (buffer-name)))))

(unless (display-graphic-p)
  (add-hook 'buffer-list-update-hook #'my/set-terminal-title))

(provide 'init-term)
;;; init-term.el ends here
