;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  ;; :requires (dash s editorconfig jsonrpc)
  :load-path (lambda () (expand-file-name "site-lisp/copilot.el" user-emacs-directory))
  :hook
  ((prog-mode git-commit-setup yaml-mode protobuf-mode markdown-mode) . copilot-mode)
  :bind
  (:map copilot-completion-map
    ("C-j" . copilot-next-completion)
    ("C-k" . copilot-previous-completion)
    ("C-g" . copilot-clear-overlay)
    ("C-f" . copilot-accept-completion)
    ("C-<return>" . copilot-accept-completion)
    ("C-w" . copilot-accept-completion-by-word))
  :custom-face
  (copilot-overlay-face ((t (:inherit shadow :foreground "#7ec0ee"))))
  :custom
  (copilot-log-max 0))

(provide 'init-ai)
