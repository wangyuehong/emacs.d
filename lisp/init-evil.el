;;; init-evil.el --- evil config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
  :preface
  (defun my/replace-at-point-or-region ()
    "Setup buffer replace string for symbol at point
or active region using evil ex mode."
    (interactive)
    (let ((text (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                  (thing-at-point 'symbol t))))
      (evil-ex (concat "%s/" (regexp-quote text) "/"))))
  :functions
  (evil-ex evil-set-initial-state)
  :defines
  (evil-window-map evil-normal-state-map evil-motion-state-map evil-visual-state-map
    evil-operator-state-map evil-disable-insert-state-bindings evil-want-Y-yank-to-eol)
  :hook (after-init . evil-mode)
  :bind
  (("C-x -" . evil-window-split)
    ("C-x |" . evil-window-vsplit)
    ("C-x \\" . evil-window-vsplit)
    :map evil-window-map
    ("-" . evil-window-split)
    ("|" . evil-window-vsplit)
    ("\\" . evil-window-vsplit)
    ("0" . delete-window)
    ("1" . delete-other-windows)
    ("o" . other-window)
    ("u" . winner-undo)
    ("C-r" . winner-redo)
    ("C-]" . xref-find-definitions-other-window)
    ("C-h" . nil)
    :map evil-normal-state-map
    ("q" . quit-window)
    ("s" . evil-avy-goto-word-or-subword-1)
    ("f" . evil-avy-goto-char-in-line)
    ("u" . undo-fu-only-undo)
    ("C-r" . undo-fu-only-redo)
    ("M-r" . my/replace-at-point-or-region)
    :map evil-operator-state-map
    ("s" . evil-avy-goto-word-or-subword-1)
    ("f" . evil-avy-goto-char-in-line)
    :map evil-motion-state-map
    ("q" . quit-window)
    ("s" . evil-avy-goto-word-or-subword-1)
    ("f" . evil-avy-goto-char-in-line)
    :map evil-visual-state-map
    ("v" . er/expand-region)
    ("s" . evil-avy-goto-word-or-subword-1)
    ("f" . evil-avy-goto-char-in-line)
    ("TAB" . evil-indent))
  :init
  ;; https://github.com/emacs-evil/evil/issues/1486
  (setq evil-disable-insert-state-bindings t
    evil-want-Y-yank-to-eol t)

  :custom
  (evil-auto-indent t)
  (evil-cross-lines t)
  (evil-default-cursor t)
  (evil-ex-complete-emacs-commands nil)
  (evil-ex-interactive-search-highlight 'selected-window)
  (evil-ex-search-vim-style-regexp t)
  (evil-find-skip-newlines nil)
  (evil-insert-skip-empty-lines t)
  (evil-kill-on-visual-paste nil)
  (evil-move-cursor-back nil)
  (evil-respect-visual-line-mode t)
  (evil-search-module 'evil-search)
  (evil-split-window-below t)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-vsplit-window-right t)
  (evil-want-C-i-jump nil)
  (evil-want-C-u-scroll t)
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-fine-undo t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
  ;; modes to map to different default states
  (dolist (p '((Info-mode . motion)
                (calculator-mode . emacs)
                (dashboard-mode . motion)
                (help-mode . motion)
                (messages-buffer-mode . motion)))
    (evil-set-initial-state (car p) (cdr p))))

(use-package evil-collection
  :after evil
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-want-unimpaired-p nil))

(use-package evil-surround
  :after evil
  :hook (evil-mode . global-evil-surround-mode))

(use-package evil-nerd-commenter :after evil)

(use-package evil-iedit-state
  :after evil
  :commands (evil-iedit-state evil-iedit-state/iedit-mode))

(use-package evil-matchit
  :after evil
  :hook (evil-mode . global-evil-matchit-mode))

(use-package evil-terminal-cursor-changer
  :after evil
  :unless (display-graphic-p)
  :hook (evil-mode . evil-terminal-cursor-changer-activate))

(provide 'init-evil)
;;; init-evil.el ends here
