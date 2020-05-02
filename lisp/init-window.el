;; -*- coding: utf-8; lexical-binding: t; -*-
(use-package winner
  :ensure nil
  :commands (winner-undo winner-redo)
  :hook (after-init . winner-mode)
  :init (setq winner-boring-buffers '("*Completions*"
                                      "*Compile-Log*"
                                      "*inferior-lisp*"
                                      "*Fuzzy Completions*"
                                      "*Apropos*"
                                      "*Help*"
                                      "*cvs*"
                                      "*Buffer List*"
                                      "*Ibuffer*"
                                      "*esh command on file*")))

(use-package shackle
    :hook (after-init . shackle-mode)
    :config
    (setq shackle-default-rule nil
          shackle-default-alignment 'below
          shackle-rules '((flycheck-error-list-mode :select t :align t :size 0.4)
                          (help-mode :select t :align t :size 0.4)
                          (go-test-mode :select t :align t :size 0.4)
                          (ag-mode :select t :align right :size 0.4))))

(use-package ibuffer
  :ensure nil
  :bind ("C-x C-b" . ibuffer)
  :init (setq ibuffer-filter-group-name-face '(:inherit (font-lock-string-face bold))))

(defun kill-other-buffers ()
  "Kill all other buffers."
  (interactive)
  (mapc 'kill-buffer (delq (current-buffer) (buffer-list))))

(provide 'init-window)
