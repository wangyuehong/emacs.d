;;; init-evil.el --- evil config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
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
    ("C-h" . nil)
    :map evil-normal-state-map
    ("q" . quit-window)
    ("s" . evil-avy-goto-char-timer)
    ("u" . undo-fu-only-undo)
    ("C-r" . undo-fu-only-redo)
    ("M-r" . my/replace-at-point-or-region)
    :map evil-operator-state-map
    ("s" . evil-avy-goto-char-timer)
    :map evil-motion-state-map
    ("q" . quit-window)
    ("s" . evil-avy-goto-char-timer)
    :map evil-visual-state-map
    ("v" . er/expand-region)
    ("s" . evil-avy-goto-char-timer)
    ("TAB" . evil-indent))
  :init
  ;; https://github.com/emacs-evil/evil/issues/1486
  (setq evil-disable-insert-state-bindings t
    evil-want-Y-yank-to-eol t)

  (make-variable-buffer-local
    (defvar my/last-input-method-in-buffer nil "input method to restore."))

  (defun my/get-current-input-method ()
    (string-trim-right (shell-command-to-string "im-select")))

  (defun my/save-current-input-method ()
    (setq my/last-input-method-in-buffer (my/get-current-input-method)))

  (defun my/restore-input-method ()
    (let ((current-input-method (my/get-current-input-method)))
      (if (and my/last-input-method-in-buffer
            (not (string= current-input-method my/last-input-method-in-buffer)))
        (start-process "set-input-source" nil "im-select" my/last-input-method-in-buffer))))

  (defun my/set-english-input-method ()
    (let ((current-input-method (my/get-current-input-method)))
      (unless (string= current-input-method "com.apple.keylayout.ABC")
        (start-process "set-input-source" nil "im-select" "com.apple.keylayout.ABC"))))

  (with-eval-after-load 'evil
    (if (executable-find "im-select")
      (progn
        (add-hook 'evil-insert-state-exit-hook
          (lambda ()
            (my/save-current-input-method)
            (my/set-english-input-method)))

        (add-hook 'evil-insert-state-entry-hook
          #'my/restore-input-method))))

  (defun my/replace-at-point-or-region ()
    "Setup buffer replace string for word at point or active region using evil ex mode."
    (interactive)
    (let ((text (if (region-active-p)
                  (buffer-substring-no-properties (region-beginning) (region-end))
                  (word-at-point))))
      (evil-ex (concat "%s/" text "/"))))

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
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-want-unimpaired-p nil))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))

(use-package evil-nerd-commenter)

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode))

(use-package evil-matchit
  :after evil
  :init
  (global-evil-matchit-mode 1))

(use-package evil-terminal-cursor-changer
  :init
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

(provide 'init-evil)
;;; init-evil.el ends here
