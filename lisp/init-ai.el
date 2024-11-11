;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
        :branch "main"
        :rev :newest)
  :hook
  ((prog-mode git-commit-setup yaml-mode protobuf-mode markdown-mode) . copilot-mode)
  :bind
  (:map copilot-completion-map
    ("C-j" . copilot-next-completion)
    ("C-k" . copilot-previous-completion)
    ("C-g" . copilot-clear-overlay)
    ("C-f" . copilot-accept-completion)
    ("M-f" . copilot-accept-completion-by-word))
  :custom-face
  (copilot-overlay-face ((t (:inherit shadow :foreground "#7ec0ee"))))
  :custom
  (copilot-log-max 0))

(use-package copilot-chat
  :after copilot
  :bind (:map git-commit-mode
         ("C-c i" . copilot-chat-insert-commit-message))
  :config
  (add-hook 'git-commit-setup-hook 'copilot-chat-insert-commit-message))

(use-package gptel
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(provide 'init-ai)
;;; init-ai.el ends here
