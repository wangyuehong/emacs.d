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

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(defun my/kill-other-buffers ()
  "Kill all other buffers except the current one."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(defun my/scroll-other-window-smooth-down ()
  "Scroll other window down by a few lines smoothly."
  (interactive)
  (scroll-other-window 5))

(defun my/scroll-other-window-smooth-up ()
  "Scroll other window up by a few lines smoothly."
  (interactive)
  (scroll-other-window -5))

(use-package nerd-icons-ibuffer
  :after nerd-icons
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(provide 'init-window)
;;; init-window.el ends here
