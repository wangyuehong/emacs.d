;;; init-completion.el --- code completion. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :custom
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-auto-delay 0.3)
  (corfu-auto-prefix 1)
  (corfu-cycle t)
  (corfu-max-width 60)
  (corfu-on-exact-match nil)
  (corfu-preselect 'prompt)
  (corfu-preselect-first nil)
  (corfu-preview-current 'insert)
  (corfu-quit-at-boundary 'separator)
  (corfu-quit-no-match 'separator)
  (global-corfu-minibuffer nil)
  :bind
  (:map corfu-map
    ("C-s" . corfu-insert-separator)
    ("RET" . corfu-complete)
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
  :functions (cape-dabbrev cape-keyword cape-wrap-buster)
  :custom
  (cape-dabbrev-min-length 3)
  (cape-dabbrev-check-other-buffers #'cape--buffers-major-mode)
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

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
  :functions (consult-line consult-xref)
  :preface
  (defun my/consult-line ()
    "Call `consult-line` with the current symbol at point or
selected region as initial input."
    (interactive)
    (let ((initial-input
            (if (use-region-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
              (thing-at-point 'symbol t))))
      (consult-line initial-input)))

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
  :custom
  (consult-narrow-key "C-s")
  (consult-preview-key (list :debounce 0.5 'any)))

(use-package embark
  :functions embark-prefix-help-command
  :bind (("C-;" . embark-act)
          ([remap describe-bindings] . embark-bindings)
          :map minibuffer-local-map
          ("C-c C-c" . embark-collect)
          ("C-c C-e" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(use-package consult-ls-git
  :after (consult))

(use-package minibuffer
  :ensure nil
  :bind(:map completion-in-region-mode-map
         ("TAB" . minibuffer-next-completion)
         ([backtab] . minibuffer-previous-completion)
         ("RET" . minibuffer-choose-completion)))

(provide 'init-completion)
;;; init-completion.el ends here
