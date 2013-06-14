(require-package 'evil)
(evil-mode 1)

;;(require 'surround)
;;(global-surround-mode 1)
(setq evil-default-cursor t) ;; see http://blog.gmane.org/gmane.emacs.vim-emulation/month=20110801
(setq evil-cross-lines t)

(add-to-list 'evil-emacs-state-modes 'org-mode) 
(add-to-list 'evil-emacs-state-modes 'helm-mode) 

(setq-default evil-auto-indent t)
(setq evil-shift-width 4)
(setq evil-repeat-move-cursor t)
(setq evil-find-skip-newlines nil)
(setq evil-move-cursor-back t)
(setq evil-want-fine-undo nil)
(setq evil-regexp-search t)
(setq evil-search-wrap t)
(setq evil-flash-delay 3)
(setq evil-want-C-i-jump nil)
(setq evil-want-C-u-scroll nil)

;;ace-jump
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-mode)

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-n") 'next-line)
(define-key evil-insert-state-map (kbd "C-p") 'previous-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)

(key-chord-define evil-insert-state-map ";;" "\C-e;")
(key-chord-define evil-insert-state-map ",," "\C-e,")
(key-chord-define evil-insert-state-map "//" (smartchr '("$" "%")))

;;esc
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

(require-package 'evil-leader)
(setq evil-leader/leader "," evil-leader/in-all-states t)
(require 'evil-leader)
(evil-leader/set-key
  "w" 'save-buffer
  "W" 'save-some-buffers
  "K" 'kill-buffer-and-window
  "p" 'previous-error
  "n" 'next-error
  "g" 'magit-status
  "." 'evil-ex
)

(provide 'init-evil)