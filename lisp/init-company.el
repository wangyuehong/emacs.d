;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package company
  :diminish
  :hook ((prog-mode yaml-mode) . company-mode)
  :commands company-cancel
  :bind (:map company-mode-map
         ([remap completion-at-point] . company-complete)
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next)
         ("C-b" . company-previous-page)
         ("C-f" . company-next-page)
         ("<tab>" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         )
  :custom
  (company-echo-delay 0.1)
  (company-idle-delay 0.1)
  (company-tooltip-limit 15)
  (company-require-match t)
  (company-minimum-prefix-length 1)
  (company-tooltip-align-annotations t)

  (company-dabbrev-other-buffers t)
  (company-dabbrev-ignore-case nil)
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-invisible t)
  (company-dabbrev-code-everywhere t)
  (company-transformers '(company-sort-by-backend-importance))
  (company-backends '((company-capf :separate company-yasnippet :with company-dabbrev)
                      (company-dabbrev-code company-keywords company-files)
                      ))
  (company-frontends '(company-pseudo-tooltip-frontend company-echo-metadata-frontend))

  :init
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))
  )

(use-package company-prescient :init (company-prescient-mode 1))

(provide 'init-company)
