;; -*- coding: utf-8; lexical-binding: t; -*-

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
    (shackle-default-size 0.5)
    (shackle-default-alignment 'below)
    (shackle-select-reused-windows t)
    (shackle-rules '((compilation-mode         :select t :align t :size 0.4)
                     (go-test-mode             :select t :align t :size 0.4)
                     (help-mode                :select t :align t :size 0.4)
                     (rg-mode                  :select t :align t)
                     (flymake-diagnostics-buffer-mode :select t :align t :size 0.4)
                     (flymake-project-diagnostics-mode :select t :align t :size 0.4)
                     (magit-status-mode :select t :inhibit-window-quit t :same t)
                     (magit-log-mode    :select t :inhibit-window-quit t :same t)
                     ("*quickrun*"      :select t :inhibit-window-quit t :same t)
                     )))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(use-package pulse
  :ensure nil
  :custom-face
  (pulse-highlight-start-face ((t (:inherit region :background unspecified))))
  (pulse-highlight-face ((t (:inherit region :background unspecified :extend t)))))

(use-package switchy-window
  :custom (switchy-window-delay 1.5) ;; default value
  :bind (:map switchy-window-minor-mode-map
              ("<remap> <other-window>" . switchy-window))
  :init
  (switchy-window-minor-mode)
  (defun my-pulse-line-on-window-selection-change (frame)
    (when (eq frame (selected-frame))
      (pulse-momentary-highlight-one-line)))

  (add-hook 'window-selection-change-functions
          #'my-pulse-line-on-window-selection-change))

(provide 'init-window)
