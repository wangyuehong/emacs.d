(require-package 'key-chord)

(require 'key-chord)
(key-chord-mode 1)

(require 'smartchr)

(require-package 'goto-last-change)
(require 'goto-last-change)

;;(global-set-key (kbd "C-c C-c") 'whole-line-or-region-kill-ring-save)
;;(global-set-key (kbd "C-c c") 'thing-copy-word)
;;(global-set-key (kbd "C-x C-x") 'whole-line-or-region-kill-region)
;;(global-set-key (kbd "C-x x") 'thing-paste-word)
;;(global-set-key (kbd "C-v") 'whole-line-or-region-yank)

;;get this work for terminal
(define-key input-decode-map "\e\eOA" [(meta up)])
(define-key input-decode-map "\e\eOB" [(meta down)])
(global-set-key [(meta up)] 'move-text-up)
(global-set-key [(meta down)] 'move-text-down)
(global-set-key (kbd "C-c i") 'imenu)

(global-set-key (kbd "<f1>") 'delete-other-windows)
(global-set-key (kbd "<f2>") 'delete-window)

(provide 'init-keybind)
