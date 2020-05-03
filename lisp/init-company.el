;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package company
  :diminish
  :defines (company-dabbrev-ignore-case
            company-dabbrev-downcase
            company-dabbrev-ignore-invisible
            company-dabbrev-other-buffers)
  :commands company-cancel
  :hook (after-init . global-company-mode)
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
  :init
  (setq company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-echo-delay 0                ; remove annoying blinking
        company-dabbrev-ignore-invisible t
        company-dabbrev-other-buffers t
        company-dabbrev-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0
        company-minimum-prefix-length 0
        company-transformers '(company-sort-by-backend-importance)
        company-require-match nil
        company-global-modes '(not erc-mode message-mode help-mode
                                   gud-mode eshell-mode shell-mode)
        )
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
    ;; Enable in current backends
    (my-company-enbale-yas)

    (defun my-company-yasnippet-disable-inline (fun command &optional arg &rest _ignore)
      "Enable yasnippet but disable it inline."
      (if (eq command 'prefix)
          (when-let ((prefix (funcall fun 'prefix)))
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
          (funcall fun command arg))))
    (advice-add #'company-yasnippet :around #'my-company-yasnippet-disable-inline))
  )

(use-package company-prescient :init (company-prescient-mode 1))

(provide 'init-company)
