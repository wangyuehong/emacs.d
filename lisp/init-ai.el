;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :vc (:url "https://github.com/copilot-emacs/copilot.el"
        :branch "main"
        :rev "952fc7a8ae06091a46995d32ebde4380e0c71142")
  :hook
  ((prog-mode git-commit-setup yaml-mode markdown-mode) . copilot-mode)
  :bind
  (("C-c x" . copilot-diagnose)
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
  :bind
  ("C-c C-c" . copilot-chat-transient)
  :config
  (let ((my/original-commit-prompt copilot-chat-commit-prompt))
    (setopt copilot-chat-commit-prompt (concat my/original-commit-prompt
                                         "\n\n### LANGUAGE RULE\n
Determine commit message language by analyzing diff content:\n
- Japanese text (with kana): Write in Japanese\n
- Simplified Chinese text: Write in Simplified Chinese\n
- Both present: Use Simplified Chinese\n
- Otherwise: Use English\n
Note: Analyze beyond individual characters - consider vocabulary, grammar, and context.")))
  :custom
  (copilot-chat-frontend 'markdown)
  (copilot-chat-markdown-prompt "Respone in 简体中文:\n")
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
    gptel-backend (gptel-make-gh-copilot "gh-copilot")))

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick" :branch "main" :rev :newest)
  :after gptel
  :init
  (setq gptel-quick-word-count 48)
  :config
  (setq gptel-quick-system-message
    (lambda (count)
      (format "If the selected text is a single English word, \
provide its Chinese translation, English IPA phonetic transcription, and meaning explanation in Chinese. \
For other content, provide a most detailed explanation. \
Use fewer than %d Chinese characters and prioritize completeness within the character limit." count))))

(use-package monet
  :vc (:url "https://github.com/stevemolitor/monet" :branch "main" :rev :newest))

(use-package claude-code
  :vc (:url "https://github.com/stevemolitor/claude-code.el" :branch "main" :rev :newest)
  :bind
  ("C-c c" . claude-code-transient)
  :preface
  (defun my/claude-display-right (buffer)
    "Display Claude buffer in right side window."
    (display-buffer buffer '((display-buffer-in-side-window)
                              (side . right)
                              (window-width . 0.4))))
  :config
  (add-hook 'claude-code-process-environment-functions #'monet-start-server-function)
  (monet-mode 1)
  (claude-code-mode)
  :custom
  (claude-code-display-window-fn #'my/claude-display-right)
  (claude-code-no-delete-other-windows t)
  (claude-code-terminal-backend 'vterm))

(provide 'init-ai)
;;; init-ai.el ends here
