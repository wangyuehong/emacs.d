;;; init-custom.el --- User customizable variables. -*- lexical-binding: t -*-
;;; Commentary:
;; Define variables with defcustom for personal/work environment customization.
;;
;; Override methods:
;;   A. M-x customize-group RET my RET (UI, auto-saves to custom.el)
;;   B. Edit custom.el directly
;;   C. In init-local.el (recommended for code-based config):
;;      (setopt my/copilot-chat-commit-model "claude-sonnet-4.5")
;;; Code:

(defgroup my nil
  "Personal Emacs configuration."
  :prefix "my/"
  :group 'convenience)

;;; AI

(defcustom my/copilot-chat-commit-model "claude-opus-4.5"
  "Model for copilot-chat commit message generation."
  :type 'string
  :group 'my)

(provide 'init-custom)
;;; init-custom.el ends here
