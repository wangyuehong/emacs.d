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
  (corfu-auto-delay 0.1)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-on-exact-match nil)
  :bind
  (:map corfu-map
    ("C-j" . corfu-next)
    ("C-k" . corfu-previous)
    ("C-s" . corfu-insert-separator)
    ("TAB" . corfu-next))
  :init
  (corfu-history-mode)
  :hook ((after-init . global-corfu-mode)))

(use-package corfu-prescient
  :after corfu)

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :hook (global-corfu-mode . corfu-terminal-mode))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-file)

  (advice-add #'eglot-completion-at-point :around #'cape-wrap-buster))

(use-package orderless
  :custom
  (completion-styles '(orderless flex))
  (completion-category-overrides '((eglot (styles . (orderless flex)))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico
  :hook (after-init . vertico-mode)
  :bind (:map vertico-map
          ("C-f" . vertico-next-group)
          ("C-b" . vertico-previous-group)
          ("C-j" . vertico-next)
          ("C-k" . vertico-previous))
  :custom
  (vertico-count 15)
  (vertico-cycle t))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :bind (("C-x b" . consult-buffer)
          ([remap imenu] . consult-imenu))
  :init
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
      xref-show-definitions-function #'consult-xref))
  :config
  (setq consult-preview-key (list :debounce 0.5 'any)))

(use-package embark
  :bind
  (("C-;" . embark-act)))

(use-package embark-consult
  :after (embark consult))

(use-package consult-ls-git
  :after (consult))

(use-package consult-dir
  :after (consult)
  :bind (("C-x C-d" . consult-dir)
          :map vertico-map
          ("C-x C-d" . consult-dir)
          ("C-x C-j" . consult-dir-jump-file)))

(use-package consult-yasnippet
  :after (consult yasnippet)
  :bind (("S-TAB" . consult-yasnippet)
          ([backtab] . consult-yasnippet)))

(provide 'init-completion)
