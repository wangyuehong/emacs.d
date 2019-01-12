;;; init-evil.el --- evil configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :ensure t
  :init ;; tweak evil's configuration before loading it
  (evil-mode t)
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
  :config ;; tweak evil after loading it
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)

  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-normal-state-map (kbd "C-d") 'delete-char)

  (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
  (define-key evil-visual-state-map (kbd "TAB") 'evil-indent)

  ;; (key-chord-define evil-insert-state-map ";;" "\C-e;")
  ;; (key-chord-define evil-insert-state-map ",," "\C-e,")
  ;; (key-chord-define evil-insert-state-map "jj" 'evil-normal-state)
  (loop for (mode . state) in
        '((minibuffer-inactive-mode . emacs)
          (ag-mode . normal)
          (ggtags-global-mode . emacs)
          (Info-mode . emacs)
          (term-mode . emacs)
          (sdcv-mode . emacs)
          (anaconda-nav-mode . emacs)
          (log-edit-mode . emacs)
          (vc-log-edit-mode . emacs)
          (inf-ruby-mode . normal)
          (quickrun--mode . emacs)
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
  ) ;; M-x pp-macroexpand-last-sexp here to test use-package

(use-package evil-surround
  :ensure t
  :init
  (global-evil-surround-mode 1))

(use-package evil-anzu
  :ensure t
  :init
  (global-anzu-mode t))

(use-package evil-visualstar
  :init
  (setq evil-visualstar/persistent t)
  (global-evil-visualstar-mode t)
  )

(use-package evil-nerd-commenter
  :ensure t
  )

(provide 'init-evil)
;;; init-evil.el ends here