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
  (company-frontends '(company-pseudo-tooltip-frontend ; show tooltip even for single candidate
                       company-echo-metadata-frontend))
  (company-backends '((company-capf :with company-yasnippet :separate)
                      (company-dabbrev-code company-keywords company-files))))

(use-package prescient
  :commands prescient-persist-mode
  :init (prescient-persist-mode 1))

(use-package company-prescient
  :init (company-prescient-mode 1))

(use-package company-tabnine
  :defer 1
  :commands (lsp-after-open-tabnine enable-company-tabnine)
  :hook
  (kill-emacs . company-tabnine-kill-process)
  :config
  (enable-company-tabnine)
  :custom
  (company-tabnine-max-num-results 3)
  :init
  (defun company//sort-by-tabnine (candidates)
    "Integrate company-tabnine with lsp-mode and prioritize yasnippet"
    (if (or (functionp company-backend)
            (not (and (listp company-backend) (memq 'company-tabnine company-backends))))
        candidates
      (let ((candidates-table (make-hash-table :test #'equal))
            candidates-other
            candidates-tabnine
            candidates-yasnippet)
        (dolist (candidate candidates)
          (let ((backend (get-text-property 0 'company-backend candidate)))
            (cond
             ((eq backend 'company-tabnine)
              (unless (gethash candidate candidates-table)
                (push candidate candidates-tabnine)))
             ((eq backend 'company-yasnippet)
              (push candidate candidates-yasnippet))
             (t
              (push candidate candidates-other)
              (puthash candidate t candidates-table)))))
        (setq candidates-other (nreverse candidates-other))
        (setq candidates-tabnine (nreverse candidates-tabnine))
        (setq candidates-yasnippet (nreverse candidates-yasnippet))
        (nconc candidates-yasnippet
               (seq-take candidates-tabnine 2)
               (seq-take candidates-other 7)))))

  (defun lsp-after-open-tabnine ()
    "Hook to attach to `lsp-after-open'."
    (add-to-list 'company-transformers 'company//sort-by-tabnine t)
    (add-to-list 'company-backends '(company-capf :with company-tabnine company-yasnippet :separate)))
  (defun enable-company-tabnine ()
    "Enable TabNine."
    (interactive)
    (if (not (memq 'company-tabnine company-backends))
        (progn
          (add-hook 'lsp-after-open-hook #'lsp-after-open-tabnine)
          (add-to-list 'company-backends #'company-tabnine)
          (when (bound-and-true-p lsp-mode) (lsp-after-open-tabnine))
          (message "TabNine enabled.")))))

;; init copilot
(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :hook
  ((prog-mode git-commit-setup) . copilot-mode)
  :bind
  (:map copilot-completion-map
        ("C-f" . 'copilot-accept-completion)
        ("C-w" . 'copilot-accept-completion-by-word)))

(provide 'init-completion)
