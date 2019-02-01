;; init-rainbow.el --- rainbow configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package rainbow-mode)

(use-package rainbow-delimiters
  :after rainbow-mode
  :hook (prog-mode . rainbow-delimiters-mode)
  )

(provide 'init-rainbow)
;;; init-rainbow.el ends here