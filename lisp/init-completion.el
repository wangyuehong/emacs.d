;;; init-completion.el --- auto complete by corfu and copilot.    -*- lexical-binding: t; -*-

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
  (corfu-auto-prefix 2)
  (corfu-cycle t)
  :bind
  (:map corfu-map
        ("C-j" . corfu-next)
        ("C-k" . corfu-previous)
        ("TAB" . corfu-next)
        ([tab] . corfu-next))
  :hook ((after-init . global-corfu-mode)))

(use-package corfu-terminal
  :if (not (display-graphic-p))
  :hook (global-corfu-mode . corfu-terminal-mode))

(use-package kind-icon
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (add-to-list 'completion-at-point-functions #'cape-file))

(use-package tabnine
  :disabled
  :commands (tabnine-start-process)
  :hook ((prog-mode . tabnine-mode)
         (kill-emacs . tabnine-kill-process))
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  ;; (add-to-list 'tabnine-disable-display-predicates #'(lambda () t))
  (tabnine-start-process)
  :custom
  (tabnine-max-num-results 3)
  :custom-face
  (tabnine-overlay-face ((t (:inherit shadow :foreground "#cd5c5c"))))
  :bind
  (:map tabnine-completion-map
        ("C-f" . tabnine-accept-completion)
        ("C-w" . tabnine-accept-completion-by-word)))

(use-package s)
(use-package dash)
(use-package copilot
  ;; :disabled
  :load-path "site-lisp/copilot.el"
  :hook
  ((prog-mode git-commit-setup) . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-f" . 'copilot-accept-completion)
        ("C-w" . 'copilot-accept-completion-by-word))
  :custom-face
  (copilot-overlay-face ((t (:inherit shadow :foreground "#7ec0ee"))))
  :custom
  (copilot-log-max 0)
  :config
  (defun my/copilot-inhibited-p ()
    "Return t if the corfu completion menu is active, nil otherwise."
    (if (display-graphic-p)
        (frame-live-p corfu--frame)
      (bound-and-true-p corfu-terminal--popon)))

  (add-to-list 'copilot-disable-display-predicates #'my/copilot-inhibited-p))

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-overrides '((file (styles basic partial-completion)))))

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
  :bind (("C-x b" . consult-buffer))
  :init
  (with-eval-after-load 'xref
    (setq xref-show-xrefs-function #'consult-xref
          xref-show-definitions-function #'consult-xref))
  :config
  (setq consult-preview-key (list :debounce 0.5 'any)))

(use-package embark
  :bind
  (("C-x c" . embark-act)))

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
