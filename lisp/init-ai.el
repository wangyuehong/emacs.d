;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :defines (copilot-completion-map copilot-disable-display-predicates)
  :hook
  ((prog-mode git-commit-setup yaml-mode markdown-mode) . copilot-mode)
  :bind
  (("C-x c g" . copilot-diagnose)
    :map copilot-completion-map
    ("TAB" . copilot-accept-completion)
    ([tab] . copilot-accept-completion)
    ("C-f" . copilot-accept-completion-by-line)
    ("C-n" . copilot-next-completion)
    ("C-p" . copilot-previous-completion)
    ("C-g" . copilot-clear-overlay))
  :custom-face
  (copilot-overlay-face ((t (:inherit shadow :foreground "#7ec0ee"))))
  :config
  (with-eval-after-load 'company
    (add-to-list 'copilot-disable-display-predicates 'company-tooltip-visible-p))
  :custom
  (copilot-idle-delay 0.2)
  (copilot-log-max 0))

(use-package copilot-chat
  :bind (("C-c c" . copilot-chat-transient))
  :custom
  (copilot-chat-frontend 'markdown)
  (copilot-chat-follow t)
  (copilot-chat-markdown-prompt "Respone in 中文:\n")
  (copilot-chat-prompt-test "Write unit tests for the following code:\n")
  (copilot-chat-prompt-optimize "Optimize and refactor the following code:\n")
  (copilot-chat-prompt-explain "Explain in detail for the following code without including
the code itself to shorten the response length:\n"))

(use-package gptel
  :defines (gptel-mode-map gptel-backend)
  :functions (gptel-make-anthropic gptel-make-azure gptel-make-ollama gptel-api-key)
  :bind (("C-c C-g" . gptel)
          ("C-c C-r" . gptel-rewrite)
          :map gptel-mode-map
          ("C-c C-m" . gptel-menu))
  :custom
  (gptel-log-level 'debug)
  (gptel-post-stream-hook #'gptel-auto-scroll)
  (gptel-post-response-functions #'gptel-end-of-response)
  (gptel-rewrite-default-action #'gptel--rewrite-diff)
  :config
  (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key))

(use-package aider
  :bind (("C-c a" . aider-transient-menu)))

(provide 'init-ai)
;;; init-ai.el ends here
