;;; init-completion.el --- auto complete by corfu and copilot.    -*- lexical-binding: t; -*-

;;; Commentary:

;;; Code:

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

(use-package cape
  :init
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-keyword)
  (add-to-list 'completion-at-point-functions #'cape-abbrev)
  (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster))

;; init copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook
  ((prog-mode git-commit-setup) . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-f" . 'copilot-accept-completion)
        ("C-w" . 'copilot-accept-completion-by-word)))

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
  (setq consult-narrow-key "<")
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help))

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
