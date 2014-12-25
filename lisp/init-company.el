(require-package 'company)

(global-company-mode)

(define-key company-active-map (kbd "TAB") 'company-select-next)
(define-key company-active-map (kbd "C-j") 'company-select-next)
(define-key company-active-map (kbd "C-k") 'company-select-previous)

(setq
 company-idle-delay 0.1
 company-selection-wrap-around t
 company-require-match nil)

(global-set-key (kbd "C-c y") 'company-yasnippet)

(provide 'init-company)
