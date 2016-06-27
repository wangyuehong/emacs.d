(require-package 'evil)
(evil-mode 1)

(require-package 'evil-surround)
(require 'evil-surround)
(global-evil-surround-mode 1)

;; (add-to-list 'evil-emacs-state-modes 'org-mode)
(add-to-list 'evil-emacs-state-modes 'quickrun/mode)
(add-to-list 'evil-emacs-state-modes 'inf-ruby-mode)

(setq-default
 evil-auto-indent t
 evil-default-cursor t
 evil-cross-lines t
 evil-search-module 'evil-search
 evil-symbol-word-search t
 evil-ex-search-vim-style-regexp t
 evil-shift-width 4
 evil-find-skip-newlines nil
 evil-move-cursor-back nil
 evil-want-fine-undo t
 )

;;ace-jump
(define-key evil-normal-state-map (kbd "SPC") 'avy-goto-word-1)
(define-key evil-visual-state-map (kbd "SPC") 'avy-goto-word-1)

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-normal-state-map (kbd "C-d") 'delete-char)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
(define-key evil-visual-state-map (kbd "TAB") 'evil-indent)

(key-chord-define evil-insert-state-map ";;" "\C-e;")
(key-chord-define evil-insert-state-map ",," "\C-e,")

(require-package 'evil-matchit)
(require 'evil-matchit)

(defun evilmi-customize-keybinding ()
  (evil-define-key 'normal evil-matchit-mode-map
    "%" 'evilmi-jump-items
    ",ms" 'evilmi-select-items
    ",md" 'evilmi-delete-items
    )
  )

(global-evil-matchit-mode 1)

(require-package 'evil-exchange)
(require 'evil-exchange)
(evil-exchange-install)

(require-package 'evil-nerd-commenter)
(require 'evil-nerd-commenter)

(require-package 'general)
(require 'general)
(general-evil-setup t)

(nvmap :prefix ","
  "="  'align
  ;; "SPC" 'avy-goto-word-1
  "a"  'ag-regexp-project-at-point
  "A"  'ag-regexp
  "b"  'ido-switch-buffer
  "B"  'helm-mini
  "e"  'iedit-mode-toggle-on-function
  "E"  'iedit-mode
  "f"  'flycheck-list-errors
  "w"  'save-buffer
  "W"  'save-some-buffers
  "l"  'helm-ls-git-ls
  "i"  'helm-imenu
  "o"  'occur
  "q"  'quickrun
  "Q"  'quickrun-region
  "/"  'evilnc-comment-or-uncomment-lines
  "k"  'kill-buffer-and-window
  "K"  'kill-other-buffers
  "P"  'list-packages
  "pf" 'projectile-find-file
  "pd" 'projectile-find-dir
  "pp" 'projectile-switch-project
  "g"  'magit-status
  "r"  'helm-show-kill-ring
  ;; "s"  'swiper-helm
  ","  'git-gutter-map
  ;; "r" 'wangyh/rinari-minor-mode-find-map
  ;; "R" 'wangyh/rinari-minor-mode-map
  "v"  'revert-buffer
  "x"  'smex
  "nn" 'narrow-to-region
  "nd" 'narrow-to-defun
  "nw" 'widen
  ;; "pp" 'wgrep-toggle-readonly-area
  ;; "pe" 'wgrep-finish-edit
  ;; "pk" 'wgrep-abort-changes
  ;; "ps" 'wgrep-save-all-buffers
  "d"  'delete-trailing-whitespace
  "D"  'ediff-revision
)

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
    (lambda ()
      (let ((color (cond ((minibufferp) default-color)
                         ((evil-insert-state-p) '("#dc322f" . "#ffffff"))
                         ((evil-emacs-state-p)  '("#444488" . "#ffffff"))
                         ((buffer-modified-p)   '("#268bd2" . "#ffffff"))
                         (t default-color))))
        (set-face-foreground 'mode-line-buffer-id (cdr color))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))))

(require-package 'evil-search-highlight-persist)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(setq evil-search-highlight-string-min-len 3)
(set-face-foreground 'evil-search-highlight-persist-highlight-face "black")
(set-face-background 'evil-search-highlight-persist-highlight-face "#66cccc")

(provide 'init-evil)
