;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package company
  :hook ((prog-mode yaml-mode) . company-mode)
  :commands company-cancel
  :bind (("M-/" . company-complete)
         ("C-M-i" . company-complete)
         :map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next)
         ("C-b" . company-previous-page)
         ("C-f" . company-next-page)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         ("<return>" . company-complete-selection)
         ("RET" . company-complete-selection)
         :map company-search-map
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next))
  :custom
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 12)
  (company-echo-delay 0)
  (company-idle-delay 0)
  (company-require-match nil)
  (company-dabbrev-ignore-invisible t)
  (company-dabbrev-other-buffers t)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-minimum-prefix-length 1)
  (company-transformers '(company-sort-by-backend-importance))

  :init
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))

  :config
  (setq company-backends (delete 'company-xcode company-backends))
  (setq company-backends (delete 'company-bbdb company-backends))
  (setq company-backends (delete 'company-eclim company-backends))
  (setq company-backends (delete 'company-oddmuse company-backends))
  )

(use-package company-prescient :init (company-prescient-mode 1))

(provide 'init-company)
