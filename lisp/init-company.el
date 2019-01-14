;; init-company.el --- company configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(setq tab-always-indent 'complete)
(add-to-list 'completion-styles 'initials t)

(use-package company
  :diminish company-mode
  :defines (company-dabbrev-ignore-case company-dabbrev-downcase)
  :commands company-abort
  :hook (after-init . global-company-mode)
  :init
  (setq company-dabbrev-other-buffers 'all
        company-tooltip-align-annotations t ; aligns annotation to the right
        company-tooltip-limit 12            ; bigger popup window
        company-idle-delay .2               ; decrease delay before autocompletion popup shows
        company-echo-delay 0                ; remove annoying blinking
        company-dabbrev-downcase nil
        company-selection-wrap-around t
        company-dabbrev-ignore-case nil
        company-idle-delay 0.2
        company-minimum-prefix-length 2
        company-require-match nil
        )
  :config

  (defun my-company-yasnippet ()
    (interactive)
    (company-abort)
    (call-interactively 'company-yasnippet))

  (defun sanityinc/local-push-company-backend (backend)
    "Add BACKEND to a buffer-local version of `company-backends'."
    (make-local-variable 'company-backends)
    (push backend company-backends))

  (dolist (backend '(company-eclim company-semantic))
    (delq backend company-backends))

    (define-key company-mode-map (kbd "M-/") 'company-complete)
    (define-key company-active-map [tab] 'company-complete-common-or-cycle)
    (define-key company-active-map [backtab] 'my-company-yasnippet)
    (define-key company-active-map (kbd "TAB") 'company-complete-common-or-cycle)
    (define-key company-active-map (kbd "C-k") 'company-select-previous)
    (define-key company-active-map (kbd "C-j") 'company-select-next)
    (define-key company-active-map (kbd "C-b") 'company-previous-page)
    (define-key company-active-map (kbd "C-f") 'company-next-page)
  )

(after-load 'company
  (after-load 'yasnippet
    ;; Add yasnippet support for all company backends
    ;; https://github.com/syl20bnr/spacemacs/pull/179
    (defvar company-mode/enable-yas t
      "Enable yasnippet for all backends.")

    (defun company-mode/backend-with-yas (backend)
      (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
          backend
        (append (if (consp backend) backend (list backend))
                '(:with company-yasnippet))))

    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))))

(provide 'init-company)
;;; init-company.el ends here
