;;; init-completion.el --- code completion. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :custom
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent 'complete)
  (text-mode-ispell-word-completion nil))

(use-package company
  :ensure t
  :hook ((prog-mode yaml-mode protobuf-mode) . company-mode)
  :bind (:map company-mode-map
          ([remap completion-at-point] . company-complete)
          ("C-c y" . company-yasnippet)
          :map company-active-map
          ("C-s"     . company-filter-candidates)
          ([backtab] . company-select-previous-or-abort))
  :init
  (add-variable-watcher 'company-backends
    (lambda (symbol new-value operation where)
      (message "%s changed: where=%s, operation=%s, new-value=%s"
        symbol where operation new-value)))
  :custom
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-ignore-case t)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case t)
  (company-dabbrev-other-buffers t)
  (company-files-exclusions '(".git/" ".DS_Store"))
  (company-format-margin-function #'company-text-icons-margin)
  (company-idle-delay 0.2)
  (company-minimum-prefix-length 2)
  (company-tooltip-minimum-width 20)
  (company-frontends '(company-pseudo-tooltip-frontend
                        company-echo-metadata-frontend))
  (company-show-quick-access t)
  (company-tooltip-align-annotations t)
  (company-tooltip-maximum-width 70)
  (company-tooltip-width-grow-only t)
  (company-backends '((company-capf :with company-yasnippet company-dabbrev)
                       (company-dabbrev-code company-keywords))))

(use-package company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

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
