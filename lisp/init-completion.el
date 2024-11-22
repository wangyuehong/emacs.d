;;; init-completion.el --- code completion. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :init
  (setq completion-cycle-threshold 3)
  (when (boundp 'read-extended-command-predicate)
    (setq read-extended-command-predicate
      #'command-completion-default-include-p))
  (setq tab-always-indent 'complete))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-preselect 'prompt)
  (corfu-on-exact-match nil)
  (corfu-preselect-first nil)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  :bind
  (:map corfu-map
    ("C-s" . corfu-insert-separator)
    ("TAB" . corfu-next)
    ([tab] . corfu-next)
    ("S-TAB" . corfu-previous)
    ([backtab] . corfu-previous))
  :hook ((after-init . global-corfu-mode)))

(use-package corfu-prescient
  :after corfu)

(use-package corfu-terminal
  :unless (display-graphic-p)
  :hook (global-corfu-mode . corfu-terminal-mode))

(use-package cape
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dabbrev-check-other-buffers #'cape--buffers-major-mode)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)

  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico
  :hook ((after-init . vertico-mode)
         (minibuffer-setup . vertico-repeat-save))
  :bind (:map vertico-map
          ("RET" . vertico-directory-enter)
          ("DEL" . vertico-directory-delete-char)
          ("C-f" . vertico-scroll-up)
          ("C-b" . vertico-scroll-down)
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous))
  :custom
  (vertico-count 15)
  (vertico-cycle t))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
          ([remap bookmark-jump]      . consult-bookmark)
          ([remap goto-line]          . consult-goto-line)
          ([remap imenu]              . consult-imenu)
          ([remap recentf-open-files] . consult-recent-file)
          :map minibuffer-local-map
          ("C-r" . consult-history))
  :init
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref))
  :config
  (setq consult-narrow-key "C-s")
  (setq consult-preview-key (list :debounce 0.5 'any)))

(use-package embark
  :bind (("C-;" . embark-act)
          ([remap describe-bindings] . embark-bindings))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(use-package consult-ls-git
  :after (consult))

(provide 'init-completion)
;;; init-completion.el ends here
