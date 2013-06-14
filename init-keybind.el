(require-package 'key-chord)

(require 'key-chord)
(key-chord-mode 1)

(require 'smartchr)

(require-package 'goto-last-change)
(require 'goto-last-change)

(key-chord-define-global "qq" 'goto-last-change)
(key-chord-define-global "fa" 'ffap)

;;(key-chord-define-global ";;"  "\C-e;")
;;(key-chord-define-global ",,"  "\C-e,")


(global-set-key (kbd "C-c C-c") 'whole-line-or-region-kill-ring-save)
;;(global-set-key (kbd "C-c c") 'thing-copy-word)
(global-set-key (kbd "C-x C-x") 'whole-line-or-region-kill-region)
;;(global-set-key (kbd "C-x x") 'thing-paste-word)
(global-set-key (kbd "C-v") 'whole-line-or-region-yank)

(require-package 'move-text)
(global-set-key [M-up] 'move-text-up)
(global-set-key [M-down] 'move-text-down)
(global-set-key [M-left] 'move-text-left)
(global-set-key [M-right] 'move-text-right)

(provide 'init-keybind)
