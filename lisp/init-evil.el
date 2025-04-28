;;; init-evil.el --- evil config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package evil
  :preface
  (defconst my/default-english-system-im "com.apple.keylayout.ABC"
    "Default English input method ID for the system (macOS).")

  (defvar-local my/last-system-im-in-buffer nil
    "Saved non-English system input method id from last insert exit.")
  (defvar-local my/last-emacs-im-in-buffer nil
    "Saved Emacs input method name from last insert exit.")

  (defun my/get-current-system-input-method ()
    "Get the current system input method using `im-select'."
    (let ((result (string-trim-right (shell-command-to-string "im-select"))))
      (if (string-empty-p result)
        (error "Failed to get current input method")
        result)))

  (defun my/set-english-input-method ()
    "Save non-English system-im and active emacs-im, then switch/disable them.
1. Save non-English system-im to `my/last-system-im-in-buffer' and switch system to English.
2. Save active emacs-im to `my/last-emacs-im-in-buffer' and disable it."
    (let ((system-im (my/get-current-system-input-method)))
      (unless (string= system-im my/default-english-system-im)
        (setq my/last-system-im-in-buffer system-im)
        (call-process "im-select" nil nil nil my/default-english-system-im)))

    (when current-input-method
      (setq my/last-emacs-im-in-buffer current-input-method)
      (set-input-method nil)))

  (defun my/restore-input-method ()
    "Restore input methods upon entering insert state.
Prioritizes restoring emacs-im. If restored, ensures system-im is English.
Otherwise, restores the saved system-im if applicable."
    (let ((emacs-im-restored nil))
      (when my/last-emacs-im-in-buffer
        (set-input-method my/last-emacs-im-in-buffer)
        (setq emacs-im-restored t)
        (setq my/last-emacs-im-in-buffer nil)) ; Reset Emacs input method state

      (let ((current-system-im (my/get-current-system-input-method)))
        (if emacs-im-restored
          (unless (string= current-system-im my/default-english-system-im)
            (call-process "im-select" nil nil nil my/default-english-system-im))
          (when my/last-system-im-in-buffer
            (unless (string= current-system-im my/last-system-im-in-buffer)
              (call-process "im-select" nil nil nil my/last-system-im-in-buffer))))
      (setq my/last-system-im-in-buffer nil))))

  (defun my/replace-at-point-or-region ()
    "Setup buffer replace string for symbol at point or active region using evil ex mode."
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
  :hook ((after-init . evil-mode)
          (evil-insert-state-exit . my/set-english-input-method)
          (evil-insert-state-entry . my/restore-input-method))
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
    ("q" . my/quit-window-dwim)
    ("s" . evil-avy-goto-word-or-subword-1)
    ("f" . evil-avy-goto-char-in-line)
    ("u" . undo-fu-only-undo)
    ("C-r" . undo-fu-only-redo)
    ("M-r" . my/replace-at-point-or-region)
    :map evil-operator-state-map
    ("s" . evil-avy-goto-word-or-subword-1)
    ("f" . evil-avy-goto-char-in-line)
    :map evil-motion-state-map
    ("q" . my/quit-window-dwim)
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
  (evil-want-abbrev-expand-on-insert-exit nil)
  (evil-want-fine-undo t)
  (evil-want-integration t)
  (evil-want-keybinding nil)
  :config
  ;; modes to map to different default states
  (dolist (p '((Info-mode . motion)
                (calculator-mode . emacs)
                (comint-mode . emacs)
                (dashboard-mode . motion)
                (help-mode . motion)
                (messages-buffer-mode . motion)
                (vterm-mode . emacs)))
    (evil-set-initial-state (car p) (cdr p))))

(use-package evil-collection
  :after evil
  :hook (evil-mode . evil-collection-init)
  :custom
  (evil-collection-want-unimpaired-p nil)
  :config
  (setq evil-collection-mode-list (remove 'vterm evil-collection-mode-list)))

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
