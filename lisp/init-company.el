;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package company
  :diminish
  :hook ((prog-mode yaml-mode) . company-mode)
  :commands company-cancel
  :bind (:map company-mode-map
         ("<backtab>" . company-yasnippet)
         :map company-active-map
         ("C-k" . company-select-previous)
         ("C-j" . company-select-next)
         ("C-b" . company-previous-page)
         ("C-f" . company-next-page)
         ("TAB" . company-complete-common-or-cycle)
         ("<backtab>" . my-company-yasnippet)
         )
  :custom
  (company-echo-delay 0.1)
  (company-idle-delay 0.1)
  (company-tooltip-limit 15)
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

  :init
  (defun my-company-yasnippet ()
    "Hide the current completeions and show snippets."
    (interactive)
    (company-cancel)
    (call-interactively 'company-yasnippet))
  :config
  ;; `yasnippet' integration
  (with-no-warnings
    (with-eval-after-load 'yasnippet
      (defun company-backend-with-yas (backend)
        "Add `yasnippet' to company backend."
        (if (and (listp backend) (member 'company-yasnippet backend))
            backend
          (append (if (consp backend) backend (list backend))
                  '(:with company-yasnippet))))

      (defun my-company-enbale-yas (&rest _)
        "Enable `yasnippet' in `company'."
        (setq company-backends (mapcar #'company-backend-with-yas company-backends)))

      (defun my-lsp-fix-company-capf ()
        "Remove redundant `comapny-capf'."
        (setq company-backends
              (remove 'company-backends (remq 'company-capf company-backends))))
      (advice-add #'lsp-completion--enable :after #'my-lsp-fix-company-capf)

      (defun my-company-yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
        "Enable yasnippet but disable it inline."
        (if (eq cmd  'prefix)
            (when-let ((prefix (funcall fn 'prefix)))
              (unless (memq (char-before (- (point) (length prefix)))
                            '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
                prefix))
          (progn
            (when (and (bound-and-true-p lsp-mode)
                       arg (not (get-text-property 0 'yas-annotation-patch arg)))
              (let* ((name (get-text-property 0 'yas-annotation arg))
                     (snip (format "%s (Snippet)" name))
                     (len (length arg)))
                (put-text-property 0 len 'yas-annotation snip arg)
                (put-text-property 0 len 'yas-annotation-patch t arg)))
            (funcall fn cmd  arg))))
      (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline)))
  )

(use-package company-prescient :init (company-prescient-mode 1))

(provide 'init-company)
