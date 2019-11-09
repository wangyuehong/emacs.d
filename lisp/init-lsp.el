;;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :diminish
  :commands lsp
  :init
  (setq
   lsp-auto-guess-root t
   lsp-keep-workspace-alive nil
   lsp-restart 'auto-restart
   lsp-prefer-flymake nil
   )
  )

(use-package lsp-ui
  :commands lsp-ui-mode
  :custom-face
  (lsp-ui-doc-background ((t (:background "brightblack"))))
  :init
  (setq lsp-ui-flycheck-enable t
        lsp-ui-doc-enable t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        )
  )

(use-package company-lsp
  :commands company-lsp
  :init
  (setq company-lsp-cache-candidates 'auto)
  :config
  (push '(company-yasnippet company-dabbrev company-lsp :separate) company-backends)
  ;; (push '(company-lsp :with company-yasnippet) company-backends)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
