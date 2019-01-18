;; init-gui-frames.el --- gui configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq use-file-dialog nil)
(setq use-dialog-box nil)
(setq inhibit-startup-screen t)

;;----------------------------------------------------------------------------
;; Window size and features
;;----------------------------------------------------------------------------
(when (fboundp 'tool-bar-mode)
  (tool-bar-mode -1))
(when (fboundp 'set-scroll-bar-mode)
  (set-scroll-bar-mode nil))

;;no menu-bar in terminal
(unless window-system
  (menu-bar-mode -1))

(setq frame-title-format
      '((:eval (if (buffer-file-name)
                   (abbreviate-file-name (buffer-file-name))
                 "%b"))))

;; Non-zero values for `line-spacing' can mess up ansi-term and co,
;; so we zero it explicitly in those cases.
(add-hook 'term-mode-hook
          (lambda ()
            (set (make-local-variable 'line-spacing)
                 0)))

;; (require-package 'disable-mouse)

(provide 'init-gui-frames)
;;; init-gui-frames.el ends here
