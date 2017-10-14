(require-package 'key-chord)

(require 'key-chord)
(key-chord-mode t)

(require 'smartchr)

;;get this work for terminal
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(define-key input-decode-map "\e\eOD" [(meta left)])
(define-key input-decode-map "\e\eOC" [(meta right)])

(require-package 'move-text)
(global-set-key (kbd "M-k") 'move-text-up)
(global-set-key (kbd "M-j") 'move-text-down)

(define-key minibuffer-local-map (kbd "C-h") 'left-char)
(define-key minibuffer-local-map (kbd "C-l") 'right-char)

(require-package 'which-key)
(add-hook 'after-init-hook 'which-key-mode)

(provide 'init-keybind)
