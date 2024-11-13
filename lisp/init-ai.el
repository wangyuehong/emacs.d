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
  (copilot-idle-delay 0.1)
  (copilot-log-max 0))

(use-package copilot-chat
  :bind (("C-x c r" . copilot-chat-review)
          ("C-x c d" . copilot-chat-doc)
          ("C-x c e" . copilot-chat-explain)
          ("C-x c f" . copilot-chat-fix)
          ("C-x c o" . copilot-chat-optimize)
          ("C-x c t" . copilot-chat-test)
          :map git-commit-mode-map
          ("C-c i" . copilot-chat-insert-commit-message))
  :hook
  (git-commit-setup-hook . copilot-chat-insert-commit-message))

(use-package gptel
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response))

(provide 'init-ai)
;;; init-ai.el ends here
