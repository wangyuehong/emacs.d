(require-package 'ace-jump-mode)
(autoload
  'ace-jump-mode
  "ace-jump-mode"
  "Emacs quick move minor mode"
  t)

(setq ace-jump-mode-scope 'global)
(define-key global-map (kbd "C-c SPC") 'ace-jump-mode)

(provide 'init-ace-jump)
