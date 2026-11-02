;;; init-custom.el --- User customizable variables. -*- lexical-binding: t -*-
;;; Commentary:
;; Define variables with defcustom for personal/work environment customization.
;;
;; Override methods:
;;   A. M-x customize-group RET my RET (UI, auto-saves to custom.el)
;;   B. Edit custom.el directly
;;   C. In init-local.el (recommended for code-based config):
;;      (setopt my/copilot-commit-model "claude-sonnet-4.5")
;;; Code:

(defgroup my nil
  "Personal Emacs configuration."
  :prefix "my/"
  :group 'convenience)

;;; AI

(defcustom my/copilot-commit-model "auto"
  "Model for copilot-commit message generation."
  :type 'string
  :group 'my)

;;; Markdown Preview

(defcustom my/md-preview-font-family "Maple Mono Normal NL NF CN"
  "Font family for embedded Markdown previews."
  :type 'string
  :group 'my)

(defcustom my/md-preview-font-size 12
  "Font size in points for embedded Markdown previews."
  :type 'number
  :group 'my)

(provide 'init-custom)
;;; init-custom.el ends here
