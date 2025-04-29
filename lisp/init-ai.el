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
    gptel-backend (gptel-make-gh-copilot "Copilot"
                    :models '(gpt-4o)))
  (gptel-make-openai "GeminiPro"
    :stream t
    :protocol "http"
    :host "localhost:4000"
    :models '(gemini-2.5-pro))
  ;; (gptel-make-openai "Claude-3.7"
  ;;   :stream t
  ;;   :protocol "http"
  ;;   :host "localhost:4000"
  ;;   :models '(claude-3.7)
  ;;   :request-params '(:thinking (:type "enabled" :budget_tokens 4096) :max_tokens 64000))

  (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key)

  (use-package codel
    :vc (:url "https://github.com/skissue/llm-tool-collection"
          :branch "main"
          :rev :newest))

  (gptel-make-tool
    :name "run_command"
    :description "Run a command."
    :category "command"
    :function (lambda (command)
                (with-temp-message (format "Running command: %s" command)
                  (shell-command-to-string command)))
    :args (list
            '(:name "command"
               :type "string"
               :description "Command to run.")))

  (gptel-make-tool
    :name "append_to_buffer"
    :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
    :category "emacs"
    :function (lambda (buffer text)
                (with-current-buffer (get-buffer-create buffer)
                  (save-excursion
                    (goto-char (point-max))
                    (insert text)))
                (format "Appended text to buffer %s" buffer))
    :args (list '(:name "buffer"
                   :type string
                   :description "The name of the buffer to append text to.")
            '(:name "text"
               :type string
               :description "The text to append to the buffer.")))

  (mapcar (apply-partially #'apply #'gptel-make-tool)
    (llm-tool-collection-get-all)))

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
