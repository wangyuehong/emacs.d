;;; init-term.el --- Terminal emulator configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package vterm
  :after evil
  :preface
  (defun my/vterm-copy-mode-evil-setup ()
    "Set evil state based on vterm-copy-mode.
Switch to `evil-motion-state` when `vterm-copy-mode` is enabled,
and switch to `evil-emacs-state` otherwise."
    (if vterm-copy-mode
      (evil-motion-state)
      (evil-emacs-state)))
  :hook (vterm-copy-mode . my/vterm-copy-mode-evil-setup)
  :bind (:map vterm-mode-map
          ("ESC" . vterm-copy-mode)
          :map vterm-copy-mode-map
          ("ESC" . vterm-copy-mode-done))
  :custom
  (vterm-max-scrollback 100000))

(use-package eat)

(provide 'init-term)
;;; init-term.el ends here
