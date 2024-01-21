;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  ;; :requires (dash s editorconfig)
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
    ("SPC" . copilot-accept-completion)
    ("C-w" . copilot-accept-completion-by-word))
  :custom-face
  (copilot-overlay-face ((t (:inherit shadow :foreground "#7ec0ee"))))
  :custom
  (copilot-log-max 0))

(use-package tabnine
  ;; :commands (tabnine-start-process)
  :hook ((kill-emacs . tabnine-kill-process))
  :config
  (add-to-list 'completion-at-point-functions #'tabnine-completion-at-point)
  ;; (tabnine-start-process)
  :custom
  (tabnine-max-num-results 5)
  :custom-face
  (tabnine-overlay-face ((t (:inherit shadow :foreground "#cd5c5c")))))

(provide 'init-ai)
