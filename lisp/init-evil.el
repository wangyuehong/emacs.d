(require-package 'evil)
;; enable evil-mode
(evil-mode 1)

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

(with-eval-after-load 'evil
  (defalias #'forward-evil-word #'forward-evil-symbol))

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
(defmacro adjust-major-mode-keymap-with-evil (m &optional r)
  `(eval-after-load (quote ,(if r r m))
     '(progn
        (evil-make-overriding-map ,(intern (concat m "-mode-map")) 'normal)
        ;; force update evil keymaps after git-timemachine-mode loaded
        (add-hook (quote ,(intern (concat m "-mode-hook"))) #'evil-normalize-keymaps))))

(adjust-major-mode-keymap-with-evil "git-timemachine")
(adjust-major-mode-keymap-with-evil "browse-kill-ring")
(adjust-major-mode-keymap-with-evil "etags-select")

;; {{ @see https://github.com/timcharper/evil-surround for tutorial
(require-package 'evil-surround)
(require 'evil-surround)
(global-evil-surround-mode 1)
(defun evil-surround-prog-mode-hook-setup ()
  (push '(47 . ("/" . "/")) evil-surround-pairs-alist)
  (push '(40 . ("(" . ")")) evil-surround-pairs-alist)
  (push '(41 . ("(" . ")")) evil-surround-pairs-alist)
  (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(93 . ("[" . "]")) evil-surround-pairs-alist))
(add-hook 'prog-mode-hook 'evil-surround-prog-mode-hook-setup)
(defun evil-surround-emacs-lisp-mode-hook-setup ()
  (push '(?` . ("`" . "'")) evil-surround-pairs-alist))
(add-hook 'emacs-lisp-mode-hook 'evil-surround-emacs-lisp-mode-hook-setup)
(defun evil-surround-org-mode-hook-setup ()
  (push '(91 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(93 . ("[" . "]")) evil-surround-pairs-alist)
  (push '(?= . ("=" . "=")) evil-surround-pairs-alist))
(add-hook 'org-mode-hook 'evil-surround-org-mode-hook-setup)
;; }}

;; {{ For example, press `viW*`
(require-package 'evil-visualstar)
(require 'evil-visualstar)
(setq evil-visualstar/persistent t)
(global-evil-visualstar-mode t)
;; }}

;; ffip-diff-mode (read only) evil setup
(defun ffip-diff-mode-hook-setup ()
    (evil-local-set-key 'normal "K" 'diff-hunk-prev)
    (evil-local-set-key 'normal "J" 'diff-hunk-next)
    (evil-local-set-key 'normal "P" 'diff-file-prev)
    (evil-local-set-key 'normal "N" 'diff-file-next)
    (evil-local-set-key 'normal "q" 'ffip-diff-quit)
    (evil-local-set-key 'normal (kbd "RET") 'ffip-diff-find-file)
    (evil-local-set-key 'normal "o" 'ffip-diff-find-file))
(add-hook 'ffip-diff-mode-hook 'ffip-diff-mode-hook-setup)

(require-package 'evil-mark-replace)
(require 'evil-mark-replace)

;; {{ define my own text objects, works on evil v1.0.9 using older method
;; @see http://stackoverflow.com/questions/18102004/emacs-evil-mode-how-to-create-a-new-text-object-to-select-words-with-any-non-sp
(defmacro define-and-bind-text-object (key start-regex end-regex)
  (let ((inner-name (make-symbol "inner-name"))
        (outer-name (make-symbol "outer-name")))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key (quote ,inner-name))
       (define-key evil-outer-text-objects-map ,key (quote ,outer-name)))))

;; between dollar signs:
(define-and-bind-text-object "$" "\\$" "\\$")
;; between equal signs
(define-and-bind-text-object "=" "=" "=")
;; between pipe characters:
(define-and-bind-text-object "|" "|" "|")
;; regular expression
(define-and-bind-text-object "/" "/" "/")
;; trimmed line
(define-and-bind-text-object "l" "^ *" " *$")
;; angular template
(define-and-bind-text-object "r" "\{\{" "\}\}")
;; }}

(loop for (mode . state) in
      '((minibuffer-inactive-mode . emacs)
        (ggtags-global-mode . emacs)
        (Info-mode . emacs)
        (term-mode . emacs)
        (sdcv-mode . emacs)
        (anaconda-nav-mode . emacs)
        (log-edit-mode . emacs)
        (vc-log-edit-mode . emacs)
        (inf-ruby-mode . normal)
        (quickrun/mode . normal)
        (yari-mode . emacs)
        (erc-mode . emacs)
        (gud-mode . emacs)
        (help-mode . emacs)
        (eshell-mode . emacs)
        (shell-mode . emacs)
        (fundamental-mode . normal)
        (woman-mode . emacs)
        (sr-mode . emacs)
        (profiler-report-mode . emacs)
        (dired-mode . normal)
        (compilation-mode . emacs)
        (speedbar-mode . emacs)
        (ivy-occur-mode . emacs)
        (messages-buffer-mode . normal)
        (browse-kill-ring-mode . normal)
        (etags-select-mode . normal)
        )
      do (evil-set-initial-state mode state))

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "C-d") 'delete-char)
(define-key evil-normal-state-map (kbd "C-d") 'delete-char)

(define-key evil-normal-state-map "Y" (kbd "y$"))
(define-key evil-normal-state-map "go" 'goto-char)
(define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
(define-key evil-visual-state-map (kbd "TAB") 'evil-indent)

(key-chord-define evil-insert-state-map ";;" "\C-e;")
(key-chord-define evil-insert-state-map ",," "\C-e,")

(require-package 'evil-matchit)
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;; press ",xx" to expand region
;; then press "z" to contract, "x" to expand
(eval-after-load "evil"
  '(progn
     (setq expand-region-contract-fast-key "z")))

;; @see https://github.com/redguardtoo/evil-matchit/issues/38
(define-key evil-visual-state-map (kbd "v") 'er/expand-region)
(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
(define-key evil-insert-state-map (kbd "C-k") 'kill-line)
(define-key evil-insert-state-map (kbd "M-j") 'yas-expand)
(define-key evil-emacs-state-map (kbd "M-j") 'yas-expand)
(global-set-key (kbd "C-r") 'undo-tree-redo)

;; My frequently used commands are listed here
;; For example, for line like `"ef" 'end-of-defun`
;;   You can either press `,ef` or `M-x end-of-defun` to execute it
(require-package 'general)
(require 'general)
(general-evil-setup t)

;; {{ use `,` as leader key
(nvmap :prefix ","
       "="  'align
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
       "o"  'helm-occur
       "q"  'quickrun
       "Q"  'quickrun-region
       "/"  'evilnc-comment-or-uncomment-lines
       "k"  'kill-buffer-and-window
       "K"  'kill-other-buffers
       "P"  'list-packages
       "g"  'magit-status
       "r"  'helm-show-kill-ring
       ","  'git-gutter-map
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
;; }}

;; {{ Use `SPC` as leader key
;; all keywords arguments are still supported
(nvmap :prefix "SPC"
       "SPC" 'avy-goto-word-1
       "j" 'avy-goto-char
       "l" 'avy-goto-line
       "e" 'er/expand-region
       "y" 'youdao-dictionary-search-at-point+
       "t" 'multi-term
       )

;; {{ Use `;` as leader key, for searching something
(nvmap :prefix ";"
       ";" 'avy-goto-char
       )
;; }}

;; {{ remember what we searched
;; http://emacs.stackexchange.com/questions/24099/how-to-yank-text-to-search-command-after-in-evil-mode/
(defvar my-search-text-history nil "List of text I searched.")
(defun my-select-from-search-text-history ()
  (interactive)
  (ivy-read "Search text history:" my-search-text-history
            :action (lambda (item)
                      (copy-yank-str item)
                      (message "%s => clipboard & yank ring" item))))
(defun my-cc-isearch-string ()
  (interactive)
  (if (and isearch-string (> (length isearch-string) 0))
      ;; NOT pollute clipboard who has things to paste into Emacs
      (add-to-list 'my-search-text-history isearch-string)))

(defadvice evil-search-incrementally (after evil-search-incrementally-after-hack activate)
  (my-cc-isearch-string))

(defadvice evil-search-word (after evil-search-word-after-hack activate)
  (my-cc-isearch-string))

(defadvice evil-visualstar/begin-search (after evil-visualstar/begin-search-after-hack activate)
  (my-cc-isearch-string))
;; }}

;; change mode-line color by evil state
(lexical-let ((default-color (cons (face-background 'mode-line)
                                   (face-foreground 'mode-line))))
  (add-hook 'post-command-hook
            (lambda ()
              (let ((color (cond ((minibufferp) default-color)
                                 ((evil-insert-state-p) '("red" . "white"))
                                 ((evil-emacs-state-p)  '("#444488" . "white"))
                                 ((buffer-modified-p)   '("blue" . "white"))
                                 (t default-color))))
                (set-face-foreground 'mode-line-buffer-id (cdr color))
                (set-face-background 'mode-line (car color))
                (set-face-foreground 'mode-line (cdr color))))))

(require-package 'evil-nerd-commenter)
(require 'evil-nerd-commenter)

;; {{ evil-exchange
;; press gx twice to exchange, gX to cancel
(require-package 'evil-exchange)
(require 'evil-exchange)
;; change default key bindings (if you want) HERE
;; (setq evil-exchange-key (kbd "zx"))
(evil-exchange-install)
;; }}

(require-package 'evil-search-highlight-persist)
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(setq evil-search-highlight-string-min-len 3)
(set-face-foreground 'evil-search-highlight-persist-highlight-face "black")
(set-face-background 'evil-search-highlight-persist-highlight-face "#66cccc")

(require-package 'evil-ediff)

(require-package 'evil-anzu)
(require 'evil-anzu)

(provide 'init-evil)
