(require-package 'evil)
(evil-mode 1)

(require-package 'surround)
(require 'surround)
(global-surround-mode 1)

(setq evil-default-cursor t) ;; see http://blog.gmane.org/gmane.emacs.vim-emulation/month=20110801
(setq evil-cross-lines t)

(add-to-list 'evil-emacs-state-modes 'org-mode)

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
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-normal-state-map "Y" (kbd "y$"))

(key-chord-define evil-insert-state-map ";;" "\C-e;")
(key-chord-define evil-insert-state-map ",," "\C-e,")
(key-chord-define evil-insert-state-map "44" (smartchr '("$" "%")))
(key-chord-define evil-insert-state-map "--" (smartchr '("->" "=>")))


(key-chord-define evil-normal-state-map "qq" 'goto-last-change)
;; (key-chord-define evil-normal-state-map "fa" 'ffap)

(key-chord-define evil-insert-state-map "jj" 'evil-normal-state)

(require-package 'evil-numbers)
(require 'evil-numbers)

(require-package 'evil-leader)
(require 'evil-leader)
(global-evil-leader-mode)

(require-package 'evil-nerd-commenter)

(setq evil-leader/leader "," evil-leader/in-all-states t)
(evil-leader/set-key
  "w"  'save-buffer
  "/" 'evilnc-comment-or-uncomment-lines
  "W"  'save-some-buffers
  "k"  'kill-buffer-and-window
  "K"  'kill-other-buffers
  "p"  'previous-error
  "n"  'next-error
  "g"  'magit-status
  "r"  'revert-buffer
  "d"  'delete-trailing-whitespace
  "."  'evil-ex
  "+"  'evil-numbers/inc-at-pt
  "-"  'evil-numbers/dec-at-pt
)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (cond ((minibufferp) default-color)
                         ((evil-insert-state-p) '("#cd0000" . "#ffffff"))
                         ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                         ((buffer-modified-p)   '("#0000ee" . "#ffffff"))
                         (t default-color))))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))))

(provide 'init-evil)
