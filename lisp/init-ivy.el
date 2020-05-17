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
         ("M-j" . ivy-immediate-done)
         :map ivy-switch-buffer-map
         ("C-k" . ivy-previous-line)
         )
  :hook ((after-init . ivy-mode)
         (ivy-mode . counsel-mode))
  :init
  (setq-default counsel-mode-override-describe-bindings t)
  :custom
  (enable-recursive-minibuffers t) ; Allow commands in minibuffers
  (ivy-use-selectable-prompt t)
  (ivy-virtual-abbreviate 'full)
  (ivy-re-builders-alist '((t . ivy--regex-ignore-order)))
  (ivy-use-virtual-buffers t)
  (ivy-height 10)
  (ivy-fixed-height-minibuffer t)
  (ivy-count-format "(%d/%d) ")
  (ivy-on-del-error-function nil)
  (ivy-initial-inputs-alist nil)
  :config
  (defun d/ignore-dired-buffers (str)
    "Return non-nil if STR names a Dired buffer. This function is intended for use with `ivy-ignore-buffers'."
    (let ((buf (get-buffer str)))
      (and buf (eq (buffer-local-value 'major-mode buf) 'dired-mode))))
  (add-to-list 'ivy-ignore-buffers #'d/ignore-dired-buffers)
  (add-to-list 'ivy-ignore-buffers "\\*Messages\\*")
  (add-to-list 'ivy-ignore-buffers "\\*Help\\*")
  (add-to-list 'ivy-ignore-buffers "\\*Ibuffer\\*")
  (add-to-list 'ivy-ignore-buffers "\\*helm lsgit\\*")
  )

(use-package amx
  :bind (("M-x" . amx))
  :custom
  (amx-save-file (expand-file-name ".amx-items" user-emacs-directory))
  (amx-history-length 20)
  )

(use-package ivy-rich
  :demand t
  :custom
  (ivy-rich-path-style 'abbrev)
  (ivy-rich-parse-remote-buffer nil)
  :config
  (ivy-rich-mode 1))

(provide 'init-ivy)
