;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package counsel
  :diminish ivy-mode counsel-mode
  :bind (("C-x j j" . counsel-bookmark)

         :map ivy-minibuffer-map
         ("<return>" . ivy-alt-done)
         ("C-k" . ivy-previous-line)
         ("C-j" . ivy-next-line)
         ("C-b" . ivy-scroll-down-command)
         ("C-f" . ivy-scroll-up-command)

         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         )
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq-default counsel-mode-override-describe-bindings t)
  (setq enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (setq ivy-use-selectable-prompt t
        ivy-virtual-abbreviate 'full
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))
        ivy-use-virtual-buffers t    ; Enable bookmarks and recentf
        ivy-height 10
        ivy-fixed-height-minibuffer t
        ivy-count-format "(%d/%d) "
        ivy-on-del-error-function nil
        ivy-initial-inputs-alist nil)
  )

(use-package amx
  :init
  (setq amx-save-file (expand-file-name ".amx-items" user-emacs-directory))
  (setq amx-history-length 20)
  :bind (("M-x" . amx)))

(use-package ivy-rich
  :demand t
  :init
  (setq ivy-rich-path-style 'abbrev)
  (setq ivy-rich-parse-remote-buffer nil)
  :config (ivy-rich-mode 1))

(provide 'init-ivy)
