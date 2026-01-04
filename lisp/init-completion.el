;;; init-completion.el --- code completion. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package emacs
  :ensure nil
  :custom
  (completion-cycle-threshold 3)
  (read-extended-command-predicate #'command-completion-default-include-p)
  (tab-always-indent t)
  (text-mode-ispell-word-completion nil))

(use-package company
  :defines (company-mode-map company-active-map)
  :ensure t
  :hook ((prog-mode yaml-mode) . company-mode)
  :bind (:map company-mode-map
          ([remap completion-at-point] . company-complete)
          ("C-c y" . company-yasnippet)
          :map company-active-map
          ([escape]  . company-abort)
          ("C-b"     . company-previous-page)
          ("C-f"     . company-next-page)
          ("C-s"     . company-filter-candidates)
          ("M-s"     . company-search-candidates)
          ([backtab] . company-select-previous-or-abort))
  ;; :init
  ;; (add-variable-watcher 'company-backends
  ;;   (lambda (symbol new-value operation where)
  ;;     (message "%s changed: where=%s, operation=%s, new-value=%s"
  ;;       symbol where operation new-value)))
  :config
  (add-to-list 'company-transformers #'delete-dups)
  :custom-face
  (company-tooltip-selection ((t (:inherit shadow :weight bold :foreground "dodgerblue" :background "#3b3b3b"))))
  :custom
  (company-backends '((company-capf company-yasnippet company-dabbrev-code :separate)
                       (company-keywords)))
  (company-dabbrev-code-everywhere t)
  (company-dabbrev-code-ignore-case nil)
  (company-files-exclusions '(".git/" ".DS_Store"))
  (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))
  (company-idle-delay 0.1)
  (company-minimum-prefix-length 2)
  (company-show-quick-access t)
  (company-tooltip-align-annotations t)
  (company-tooltip-maximum-width 70)
  (company-tooltip-minimum-width 20)
  (company-text-icons-mapping
    '((array "" font-lock-type-face)
       (boolean "" font-lock-builtin-face)
       (class "" font-lock-type-face)
       (color "" success)
       (constant "" font-lock-constant-face)
       (constructor "" font-lock-function-name-face)
       (enum-member "" font-lock-builtin-face)
       (enum "" font-lock-builtin-face)
       (field "" font-lock-variable-name-face)
       (file "" font-lock-string-face)
       (folder "" font-lock-doc-face)
       (interface "" font-lock-type-face)
       (keyword "" font-lock-keyword-face)
       (method "" font-lock-function-name-face)
       (function "󰡱" font-lock-function-name-face)
       (module "" font-lock-type-face)
       (numeric "󰎠" font-lock-builtin-face)
       (operator "" font-lock-comment-delimiter-face)
       (property "" font-lock-variable-name-face)
       (reference "" font-lock-doc-face)
       (snippet "" font-lock-string-face)
       (string "" font-lock-string-face)
       (struct "" font-lock-variable-name-face)
       (text "󱀍" shadow)
       (type-parameter "" font-lock-type-face)
       (unit "󰻐" shadow)
       (value "" font-lock-builtin-face)
       (variable "󰫧" font-lock-variable-name-face)
       (t "." shadow))))

(use-package company-prescient
  :after company
  :hook (company-mode . company-prescient-mode))

(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles basic partial-completion))
                                    (eglot-capf (styles orderless))))
  (orderless-component-separator #'orderless-escapable-split-on-space))

(use-package vertico
  :defines (vertico-map)
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
  :hook (vertico-mode . marginalia-mode))


(use-package consult
  :functions (consult-line consult-xref)
  :preface
  (defun my/consult-line ()
    "Call `consult-line' with symbol at point or region as initial input."
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
  :custom
  (xref-show-xrefs-function #'consult-xref)
  (xref-show-definitions-function #'consult-xref)
  (consult-narrow-key "C-s")
  (consult-preview-key (list :debounce 0.5 'any)))

(use-package embark
  :functions embark-prefix-help-command
  :bind (("C-;" . embark-act)
          ([remap describe-bindings] . embark-bindings)
          :map minibuffer-local-map
          ("C-c C-c" . embark-collect)
          ("C-c C-o" . embark-export))
  :init
  (setq prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult))

(use-package consult-ls-git
  :after (consult))

(use-package minibuffer
  :ensure nil
  :preface
  (defun my/minibuffer-free-input-p ()
    "Return non-nil if current minibuffer is free-input (no completion table)."
    (and (minibufferp)
      (null minibuffer-completion-table)))
  :bind (:map minibuffer-local-map
          ("C-k" . kill-line)
          ("C-j" . newline)
          ("S-<return>" . newline)
          :map completion-in-region-mode-map
          ([backtab] . minibuffer-previous-completion)
          ("RET" . minibuffer-choose-completion)))

(use-package completion-preview
  :ensure nil
  :custom
  (completion-preview-minimum-symbol-length 2))

(use-package cape
  :preface
  (defun my/minibuffer-autosuggest-setup ()
    "Enable autosuggest in free-input minibuffer."
    (when (my/minibuffer-free-input-p)
      (setq-local completion-at-point-functions
        (append (list #'cape-file #'cape-dabbrev)
          completion-at-point-functions))
      (completion-preview-mode 1)))
  :hook (minibuffer-setup . my/minibuffer-autosuggest-setup)
  :custom
  (cape-dabbrev-buffer-function #'cape-text-buffers))

(use-package nerd-icons-completion
  :hook (marginalia-mode . nerd-icons-completion-mode))

(provide 'init-completion)
;;; init-completion.el ends here
