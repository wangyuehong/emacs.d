;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package company
  :diminish
  :hook ((prog-mode yaml-mode protobuf-mode) . company-mode)
  :bind (:map company-mode-map
        ([remap completion-at-point] . company-complete)
        ("<backtab>" . company-yasnippet)
        :map company-active-map
        ("C-s" . company-filter-candidates)
        ("TAB" . company-complete-common-or-cycle)
        ([backtab] . company-select-previous-or-abort))
  :custom
  (company-dabbrev-downcase nil)
  (company-dabbrev-ignore-case nil)
  (company-files-exclusions '(".git/" ".DS_Store"))
  (company-format-margin-function nil)
  (company-idle-delay 0)
  (company-minimum-prefix-length 2)
  (company-require-match nil)
  (company-show-quick-access t)
  (company-tooltip-align-annotations t)
  (company-tooltip-limit 10)
  (company-tooltip-width-grow-only t)
  (company-transformers '(company-sort-by-backend-importance))
  (company-backends '((company-capf :with company-yasnippet :separate)
                      (company-dabbrev-code company-keywords company-files))))

(use-package company-tabnine
  :defer 1
  :commands (lsp-after-open-tabnine company-tabnine-toggle)
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :config
  (company-tabnine-toggle t)
  :custom
  (company-tabnine-max-num-results 9)
  :init
  (defun company//sort-by-tabnine (candidates)
    "Integrate company-tabnine with lsp-mode"
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-lsp
            candidates-tabnine)
        (dolist (candidate candidates)
          (if (eq (get-text-property 0 'company-backend candidate)
                  'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine))
            (push candidate candidates-lsp)
            (puthash candidate t candidates-table)))
        (setq candidates-lsp (nreverse candidates-lsp))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (nconc (seq-take candidates-tabnine 5)
               (seq-take candidates-lsp 10)))))
  (defun lsp-after-open-tabnine ()
    "Hook to attach to `lsp-after-open'."
    (setq-local company-tabnine-max-num-results 5)
    (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    (add-to-list 'company-backends '(company-tabnine :with company-yasnippet company-capf :separate)))
  (defun company-tabnine-toggle (&optional enable)
    "Enable/Disable TabNine. If ENABLE is non-nil, definitely enable it."
    (interactive)
    (if (or enable (not (memq 'company-tabnine company-backends)))
        (progn
          (add-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
          (add-to-list 'company-backends #'company-tabnine)
          (when (bound-and-true-p lsp-mode) (lsp-after-open-tabnine))
          (message "TabNine enabled."))
      (setq company-backends (delete 'company-tabnine company-backends))
      (setq company-backends (delete '(company-tabnine :with company-yasnippet company-capf :separate) company-backends))
      (remove-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
      (company-tabnine-kill-process)
      (message "TabNine disabled."))))

;; init copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook
  ((prog-mode git-commit-setup) . copilot-mode)
  :bind
  (:map copilot-mode-map
        ("C-f" . 'copilot-accept-completion)
        ("TAB" . 'copilot-accept-completion))
  (:map copilot-completion-map
        ("C-f" . 'copilot-accept-completion)
        ("C-w" . 'copilot-accept-completion-by-word)))

(provide 'init-completion)