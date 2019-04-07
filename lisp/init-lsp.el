;;; init-lsp.el --- lsp configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package lsp-mode
  :diminish
  :commands lsp
  :init
  (setq
   lsp-auto-guess-root t
   lsp-prefer-flymake nil
   lsp-trace t
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
        lsp-ui-doc-enable nil
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t

        lsp-ui-sideline-enable t
        lsp-ui-sideline-ignore-duplicate t
        )
  )

(use-package company-lsp
  :commands company-lsp
  :bind (("<backtab>" . company-lsp))
  :init
  (setq company-lsp-cache-candidates 'auto)
  :config
  (push '(company-lsp company-dabbrev :separate) company-backends)
  )

(provide 'init-lsp)
;;; init-lsp.el ends here
