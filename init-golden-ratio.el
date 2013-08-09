(require-package 'golden-ratio)

(require 'golden-ratio)
(golden-ratio-mode 1)
(setq golden-ratio-exclude-modes '("ediff-mode" "speedbar-mode"))

;; disable golden-ratio mode for ediff
(add-hook 'ediff-before-setup-hook
          (lambda ()
            (golden-ratio-mode -1)))
(add-hook 'ediff-quit-hook
          (lambda ()
            (golden-ratio-mode 1)))

(provide 'init-golden-ratio)
