;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
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
    (add-to-list 'copilot-disable-display-predicates #'company-tooltip-visible-p))
  (with-eval-after-load 'popup
    (add-to-list 'copilot-disable-display-predicates #'my/popup-visible-p))
  :custom
  (copilot-idle-delay 0.2)
  (copilot-log-max 0))

(use-package copilot-chat
  :config
  (let ((my/original-commit-prompt copilot-chat-commit-prompt))
    (setopt copilot-chat-commit-prompt (concat my/original-commit-prompt
                                         "\n\n### LANGUAGE RULE\n
Determine the commit message language based on the diff content.\n
**You must** use 日本語 or 中文 to write commit message if the diff contains 日本語 or 中文 characters.")))
  :custom
  (copilot-chat-frontend 'markdown)
  (copilot-chat-markdown-prompt "Respone in 中文:\n")
  (copilot-chat-prompt-test "Write unit tests for the following code:\n")
  (copilot-chat-prompt-optimize "Optimize and refactor the following code:\n")
  (copilot-chat-prompt-explain "Explain the following code:\n"))

(use-package gptel
  :defines (gptel-mode-map gptel-backend)
  :custom
  (gptel-log-level 'info)
  (gptel-post-stream-hook #'gptel-auto-scroll)
  (gptel-post-response-functions #'gptel-end-of-response)
  (gptel-rewrite-default-action #'gptel--rewrite-diff)
  :config
  (setq gptel-model 'gpt-4o
    gptel-backend (gptel-make-gh-copilot "GHCopilot"
                    :models '(gpt-4o)))
  (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :branch "main" :rev :newest))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :branch "main" :rev :newest)
  :bind
  ("C-c c" . claude-code-transient)
  :init
  (add-to-list 'display-buffer-alist
    '("^\\*claude"
       (display-buffer-in-side-window)
       (window-width . 0.4)
       (side . right)))
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :custom
  (claude-code-no-delete-other-windows t)
  (claude-code-terminal-backend 'eat))

(provide 'init-ai)
;;; init-ai.el ends here
