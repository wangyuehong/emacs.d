;;; init-window.el --- window config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package tab-bar
  :ensure nil
  :hook
  (after-init . tab-bar-mode)
  (after-init . tab-bar-history-mode)
  :custom
  (tab-bar-close-button-show nil)
  (tab-bar-history-buttons-show nil)
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-show nil)
  (tab-bar-tab-hints t)
  (tab-bar-tab-name-function 'tab-bar-tab-name-all))

(use-package shackle
  :hook (after-init . shackle-mode)
  :custom
  (shackle-default-rule nil)
  (shackle-default-size 0.4)
  (shackle-default-alignment 'below)
  (shackle-select-reused-windows t)
  (shackle-rules '((compilation-mode :select t :align t)
                    (go-test-mode     :select t :align t)
                    (help-mode        :select t :align t)
                    (rg-mode          :select t :align t)
                    (flymake-diagnostics-buffer-mode  :select t :align t)
                    (flymake-project-diagnostics-mode :select t :align t)
                    (magit-status-mode :select t :same t)
                    (magit-log-mode    :select t :same t)
                    ("*quickrun*"      :select t :align t)
                    )))

(use-package popper
  :bind (("M-`" . popper-toggle))
  :custom
  (popper-display-control nil)
  (popper-group-function 'popper-group-by-project)
  (popper-reference-buffers
    '("\\*Messages\\*"
       "\\*rg\\*"
       "\\*quickrun\\*"
       "\\*Go Test\\*"
       "\\*Flymake diagnostics"
       help-mode
       compilation-mode))
  :config
  (popper-mode t)
  (popper-echo-mode t))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package emacs
  :bind (:map window-prefix-map
          ("j" . enlarge-window)
          ("k" . shrink-window)
          ("h" . enlarge-window-horizontally)
          ("l" . shrink-window-horizontally)
          ("C-j" . scroll-other-window-line)
          ("C-k" . scroll-other-window-down-line)
          ("C-f" . scroll-other-window)
          ("C-b" . scroll-other-window-down)
          :repeat-map my/window-repeat-map
          ("j" . enlarge-window)
          ("k" . shrink-window)
          ("h" . enlarge-window-horizontally)
          ("l" . shrink-window-horizontally)
          ("C-j" . scroll-other-window-line)
          ("C-k" . scroll-other-window-down-line)
          ("C-f" . scroll-other-window)
          ("C-b" . scroll-other-window-down))
  :config
  (defun scroll-other-window-line ()
    "Scroll up of one line in other window."
    (interactive)
    (scroll-other-window 1))

  (defun scroll-other-window-down-line ()
    "Scroll down of one line in other window."
    (interactive)
    (scroll-other-window-down 1)))

(provide 'init-window)
;;; init-window.el ends here
