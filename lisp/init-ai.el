;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :vc (:url "https://github.com/wangyuehong/copilot.el"
        :branch "main"
        :rev :newest)
  :hook
  ((prog-mode git-commit-setup yaml-mode protobuf-mode markdown-mode) . copilot-mode)
  :bind
  (("C-x c g" . copilot-diagnose)
    :map copilot-completion-map
    ("C-n" . copilot-next-completion)
    ("C-p" . copilot-previous-completion)
    ("C-g" . copilot-clear-overlay)
    ("C-f" . copilot-accept-completion)
    ("M-f" . copilot-accept-completion-by-word))
  :custom-face
  (copilot-overlay-face ((t (:inherit shadow :foreground "#7ec0ee"))))
  :custom
  (copilot-version "1.41.0")
  (copilot-idle-delay 0)
  (copilot-log-max 0))

(use-package copilot-chat
  :bind (("C-x c a"  . copilot-chat-add-current-buffer)
          ("C-x c c" . copilot-chat-custom-prompt-selection)
          ("C-x c d" . copilot-chat-doc)
          ("C-x c e" . copilot-chat-explain)
          ("C-x c f" . copilot-chat-fix)
          ("C-x c l" . copilot-chat-list)
          ("C-x c o" . copilot-chat-optimize)
          ("C-x c p" . copilot-chat-display)
          ("C-x c r" . copilot-chat-review)
          ("C-x c t" . copilot-chat-test)
          ("C-x c x" . copilot-chat-reset))
  :custom
  (copilot-chat-prompt-optimize "Please optimize and refactor the following code:\n")
  (copilot-chat-prompt-explain "Please write an explanation in detail for the following code in Chinese without including the code itself to shorten the response length:\n"))

(use-package gptel
  :bind
  (:map gptel-mode-map
        ("C-c m" . gptel-menu))
  :custom
  (gptel-post-stream-hook #'gptel-auto-scroll)
  (gptel-post-response-functions #'gptel-end-of-response)
  (gptel-rewrite-default-action #'gptel--rewrite-merge)
  :config
  (gptel-make-anthropic "Claude" :stream t)
  (setq gptel-backend (gptel-make-ollama "Ollama" :stream t :models '(llama3.2:3b qwen2.5-coder:7b)))
  (setq gptel-model 'qwen2.5-coder:7b))

(provide 'init-ai)
;;; init-ai.el ends here
