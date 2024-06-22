;;; init-evil.el --- evil config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
  :hook (after-init . evil-mode)
  :bind
  (("C-x -" . evil-window-split)
    ("C-x |" . evil-window-vsplit)
    ("M-]" . xref-find-references))

  :init
  (setq evil-want-abbrev-expand-on-insert-exit nil
    evil-disable-insert-state-bindings t
    evil-want-fine-undo t
    evil-want-integration t
    evil-want-keybinding nil
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
  (evil-shift-width 4)
  (evil-split-window-below t)
  (evil-symbol-word-search t)
  (evil-undo-system 'undo-redo)
  (evil-vsplit-window-right t)
  :config
  (define-key evil-normal-state-map (kbd "q") 'quit-window)
  (define-key evil-motion-state-map (kbd "q") 'quit-window)
  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-motion-state-map (kbd "s") 'evil-avy-goto-word-or-subword-1)
  (define-key evil-normal-state-map (kbd "s") 'evil-avy-goto-word-or-subword-1)
  (define-key evil-visual-state-map (kbd "s") 'evil-avy-goto-word-or-subword-1)
  (define-key evil-normal-state-map (kbd "f") 'evil-avy-goto-char-in-line)
  (define-key evil-visual-state-map (kbd "f") 'evil-avy-goto-char-in-line)
  (define-key evil-visual-state-map (kbd "TAB") 'evil-indent)
  (define-key evil-normal-state-map "u" 'undo-fu-only-undo)
  (define-key evil-normal-state-map (kbd "C-r") 'undo-fu-only-redo)

  ;; modes to map to different default states
  (dolist (p '((Info-mode . emacs)
                (calculator-mode . emacs)
                (comint-mode . emacs)
                (dashboard-mode . motion)
                (eshell-mode . emacs)
                (fundamental-mode . emacs)
                (help-mode . motion)
                (messages-buffer-mode . motion)
                (minibuffer-inactive-mode . emacs)
                (shell-mode . emacs)
                (special-mode . emacs)
                (term-mode . emacs)
                (xref--xref-buffer-mode . emacs)))
    (evil-set-initial-state (car p) (cdr p))))

(use-package evil-collection
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-want-unimpaired-p nil))

(use-package evil-args
  :after evil
  :bind (:map evil-inner-text-objects-map
          ("a" . evil-inner-arg)
          :map evil-outer-text-objects-map
          ("a" . evil-outer-arg)))

(use-package evil-surround
  :hook (after-init . global-evil-surround-mode))

(use-package evil-nerd-commenter)

(use-package evil-iedit-state
  :commands (evil-iedit-state evil-iedit-state/iedit-mode))

(use-package evil-matchit
  :init
  (global-evil-matchit-mode 1))

(use-package evil-terminal-cursor-changer
  :init
  (unless (display-graphic-p)
    (evil-terminal-cursor-changer-activate)))

(provide 'init-evil)
;;; init-evil.el ends here
