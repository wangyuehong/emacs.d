;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package evil
  :hook (after-init . evil-mode)
  :bind
  (("C-x o" . evil-window-next)
   ("C-x -" . evil-window-split)
   ("C-x |" . evil-window-vsplit)
   ("M-]" . xref-find-references)
   ([remap evil-quit] . kill-this-buffer)
   :map evil-normal-state-map
   ([remap evil-jump-to-tag] . xref-find-definitions))

  :init
  (setq evil-want-Y-yank-to-eol t
        evil-want-keybinding nil
        evil-want-fine-undo t
        evil-want-abbrev-expand-on-insert-exit nil)

  :custom
  (evil-auto-indent t)
  (evil-cross-lines t)
  (evil-default-cursor t)
  (evil-disable-insert-state-bindings t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-ex-search-vim-style-regexp t)
  (evil-find-skip-newlines nil)
  (evil-insert-skip-empty-lines t)
  (evil-kill-on-visual-paste nil)
  (evil-move-cursor-back nil)
  (evil-respect-visual-line-mode t)
  (evil-search-module 'evil-search)
  (evil-shift-width 4)
  (evil-split-window-below t)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-vsplit-window-right t)

  :config
  (with-eval-after-load 'evil-maps ; avoid conflict with company
    (define-key evil-insert-state-map (kbd "C-k") nil))

  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  ;; (define-key evil-normal-state-map (kbd "q") 'quit-window)
  (define-key evil-normal-state-map (kbd "f") 'evil-avy-goto-char-timer)
  (define-key evil-visual-state-map (kbd "f") 'evil-avy-goto-char-timer)
  (define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
  (define-key evil-visual-state-map (kbd "TAB") 'evil-indent)

  (with-no-warnings
    ;; modes to map to different default states
    (dolist (p '((comint-mode . emacs)
                 (term-mode . emacs)
                 (eshell-mode . emacs)
                 (calculator-mode . emacs)
                 (help-mode . motion)
                 (minibuffer-inactive-mode . emacs)
                 (special-mode . emacs)
                 (Info-mode . emacs)
                 (term-mode . emacs)
                 (shell-mode . emacs)
                 (xref--xref-buffer-mode . emacs)
                 (fundamental-mode . emacs)))
      (evil-set-initial-state (car p) (cdr p)))


    (defconst my-default-color (cons (face-background 'mode-line)
                                     (face-foreground 'mode-line)))
    (defun my-show-evil-state ()
      "Change mode line color to notify user evil current state."
      (let* ((color (cond ((minibufferp) my-default-color)
                          ((evil-insert-state-p)  '("#ff6347" . "#ffe7ba"))
                          ((evil-replace-state-p) '("#b22222" . "#ffe7ba"))
                          ((evil-emacs-state-p)   '("#444488" . "#ffe7ba"))
                          ((buffer-modified-p)    '("#4f94cd" . "#ffe7ba"))
                          (t my-default-color))))
        (set-face-background 'mode-line (car color))
        (set-face-foreground 'mode-line (cdr color))))
    (add-hook 'post-command-hook #'my-show-evil-state)))

(use-package evil-collection :hook (evil-mode . evil-collection-init))
(use-package evil-surround :hook (after-init . global-evil-surround-mode))
(use-package evil-nerd-commenter)

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(provide 'init-evil)
