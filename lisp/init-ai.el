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
  :bind (("C-c c" . copilot-chat-transient))
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

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick" :branch "main" :rev :newest)
  :after gptel
  :init
  (setq gptel-quick-word-count 48)
  :config
  (setq gptel-quick-system-message
    (lambda (count)
      (format "Provide a most detailed explanation using fewer than %d Chinese characters. \
Prioritize completeness within the character limit." count))))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (aidermacs-setup-minor-mode)
  (unless (display-graphic-p)
    (setq aidermacs-extra-args '("--editor" "emacsclient")))
  :custom
  (aidermacs-vterm-use-theme-colors nil)
  (aidermacs-backend 'vterm))

(provide 'init-ai)
;;; init-ai.el ends here
