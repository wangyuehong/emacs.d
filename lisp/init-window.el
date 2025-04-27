;;; init-window.el --- window config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package winner-mode
  :ensure nil
  :hook (after-init . winner-mode)
  :bind (:map window-prefix-map
          ("u" . winner-undo)
          ("C-r" . winner-redo)
          :repeat-map winner-repeat-map
          ("u" . winner-undo)
          ("C-r" . winner-redo))
  :custom
  (winner-dont-bind-my-keys t))

(use-package shackle
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-rule nil)
  (shackle-default-size 0.4)
  (shackle-default-alignment 'below)
  (shackle-select-reused-windows t)
  (shackle-rules '((compilation-mode :select t :align t)
                    (flymake-diagnostics-buffer-mode  :select t :align t)
                    (flymake-project-diagnostics-mode :select t :align t)
                    (go-test-mode      :select t :align t)
                    (help-mode         :select t :align t)
                    (magit-log-mode    :select t :same  t)
                    (magit-status-mode :select t :same  t)
                    (occur-mode        :select t :align t)
                    (rg-mode           :select t :align t)
                    ("*quickrun*"      :select t :align t))))

(defun my/quit-window-dwim ()
  "Do-What-I-Mean behaviour for a general `quit-window'.

The DWIM behaviour of this command is as follows:

- When in `vterm-mode', send `q' to vterm.
- When only one window, call `quit-window' to close it.
- When more than one windows, call `delete-windows-on' to select a window to delete."
  (interactive)
  (cond
   ((derived-mode-p 'vterm-mode)
    (vterm-send-string "q"))
   ((= (length (window-list)) 1)
    (quit-window))
   (t
    (let ((buffer (read-buffer "Delete windows on buffer: " (current-buffer) t)))
      (delete-windows-on buffer)))))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(defun my/kill-other-buffers ()
  "Kill all other buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(provide 'init-window)
;;; init-window.el ends here
