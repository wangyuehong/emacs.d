;;; init-evil.el --- evil configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq-default
   evil-auto-indent t
   evil-want-keybinding nil
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
  (evil-mode t)
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
   ;; modes to map to different default states

  (dolist (mode-map '((comint-mode . emacs)
                      (term-mode . emacs)
                      (eshell-mode . emacs)
                      (help-mode . emacs)
                      (fundamental-mode . emacs)))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

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

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init)
  )

(use-package evil-surround
  :after evil
  :config
  (global-evil-surround-mode)
  )

(use-package evil-anzu
  :after evil
  :config
  (global-anzu-mode t))

(use-package evil-visualstar
  :after evil
  :init
  (setq evil-visualstar/persistent t)
  :config
  (global-evil-visualstar-mode)
  )

(use-package evil-nerd-commenter
  :after evil
  )

(provide 'init-evil)
;;; init-evil.el ends here
