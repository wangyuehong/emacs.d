(require-package 'evil)
(evil-mode 1)

(require-package 'surround)
(require 'surround)
(global-surround-mode 1)

(add-to-list 'evil-emacs-state-modes 'org-mode)

(setq evil-default-cursor t)
(setq evil-cross-lines t)
(setq evil-search-module 'evil-search)
(setq evil-ex-search-vim-style-regexp t)

(setq-default evil-auto-indent t)
(setq evil-shift-width 4)
(setq evil-find-skip-newlines nil)
(setq evil-move-cursor-back nil)
(setq evil-want-fine-undo t)

;;ace-jump
(define-key evil-normal-state-map (kbd "SPC") 'ace-jump-char-mode)
(define-key evil-visual-state-map (kbd "SPC") 'ace-jump-char-mode)

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)

(define-key evil-normal-state-map "Y" (kbd "y$"))

(key-chord-define evil-insert-state-map ";;" "\C-e;")
;; (key-chord-define evil-insert-state-map ",," "\C-e,")
;; (key-chord-define evil-insert-state-map "44" (smartchr '("$" "%")))
(key-chord-define evil-insert-state-map "--" (smartchr '("->" "=>")))

(key-chord-define evil-normal-state-map "qq" 'goto-last-change)
(key-chord-define evil-normal-state-map "@@" 'er/expand-region)
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
  "a"  'ag
  "A"  'ag-project
  "b"  'ido-switch-buffer
  "f"  'ido-find-file
  "F"  'helm-find-files
  "w"  'save-buffer
  "l"  'helm-ls-git-ls
  "/"  'evilnc-comment-or-uncomment-lines
  "W"  'save-some-buffers
  "k"  'kill-buffer-and-window
  "K"  'kill-other-buffers
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
