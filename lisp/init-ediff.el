(setq-default
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain)

(defun configure-ediff ()
  (define-key ediff-mode-map (kbd "k") 'ediff-previous-difference)
  (define-key ediff-mode-map (kbd "j") 'ediff-next-difference))

(add-hook 'ediff-startup-hook 'configure-ediff)

(provide 'init-ediff)
