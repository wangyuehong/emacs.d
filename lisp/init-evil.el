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
        evil-want-C-i-jump nil
        evil-want-abbrev-expand-on-insert-exit nil)

  :custom
  (evil-auto-indent t)
  (evil-default-cursor t)
  (evil-cross-lines t)
  (evil-search-module 'evil-search)
  (evil-symbol-word-search t)
  (evil-ex-search-vim-style-regexp t)
  (evil-shift-width 4)
  (evil-find-skip-newlines nil)
  (evil-move-cursor-back nil)
  (evil-kill-on-visual-paste nil)
  (evil-split-window-below t)
  (evil-vsplit-window-right t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-disable-insert-state-bindings t)
  (evil-insert-skip-empty-lines t)
  (evil-undo-system 'undo-redo)

  :config
  (defalias #'forward-evil-word #'forward-evil-symbol)

  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-normal-state-map (kbd "C-d") 'delete-char)
  (define-key evil-visual-state-map (kbd "C-y") 'copy-to-clipboard)
  (define-key evil-normal-state-map (kbd "q") 'quit-window)
  (define-key evil-normal-state-map (kbd "f") 'evil-avy-goto-char)
  (define-key evil-visual-state-map (kbd "f") 'evil-avy-goto-char)
  (define-key evil-normal-state-map (kbd "s") 'evil-avy-goto-word-or-subword-1)
  (define-key evil-visual-state-map (kbd "s") 'evil-avy-goto-word-or-subword-1)
  (define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
  (define-key evil-visual-state-map (kbd "TAB") 'evil-indent)

  ;; modes to map to different default states
  (dolist (mode-map '((comint-mode . emacs)
                      (term-mode . emacs)
                      (eshell-mode . emacs)
                      (calculator-mode . emacs)
                      (help-mode . motion)))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

  (let* ((default-color (cons (face-background 'mode-line)
                              (face-foreground 'mode-line))))
    (add-hook 'post-command-hook
              (lambda ()
                (let* ((color (cond ((minibufferp) default-color)
                                    (buffer-read-only default-color)
                                    ((evil-insert-state-p)  '("#ff6347" . "#ffe7ba"))
                                    ((evil-replace-state-p) '("#b22222" . "#ffe7ba"))
                                    ((evil-emacs-state-p)   '("#444488" . "#ffe7ba"))
                                    ((buffer-modified-p)    '("#4f94cd" . "#ffe7ba"))
                                    (t default-color))))
                  (set-face-background 'mode-line (car color))
                  (set-face-foreground 'mode-line (cdr color))))))
 )

(use-package evil-collection
  :demand t
  :custom (evil-collection-company-use-tng  nil)
  :init (evil-collection-init))

(use-package evil-surround :hook (after-init . global-evil-surround-mode))
(use-package evil-nerd-commenter :demand t)


(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :custom
  (evil-iedit-state-tag        (propertize "<E>"  'face '((:background "#228b22" ))))
  (evil-iedit-insert-state-tag (propertize "<Ei>" 'face '((:background "#ff6347" )))))

(provide 'init-evil)
