;;; init-term.el --- Terminal emulator configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package eat
  :custom
  (eat-kill-buffer-on-exit t)
  (eat-term-scrollback-size 524288)) ;; 512K

(provide 'init-term)
;;; init-term.el ends here
