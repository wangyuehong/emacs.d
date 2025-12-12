;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
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
  :preface
  ;; Project language setting, used for commit message generation, etc.
  ;; Configure per-project via .dir-locals.el in project root:
  ;;   ((nil . ((my/project-language . "ja"))))  ; Japanese
  ;;   ((nil . ((my/project-language . "zh"))))  ; Chinese
  (defvar-local my/project-language "en"
    "Project documentation language. Values: \"zh\", \"ja\", \"en\".")
  (put 'my/project-language 'safe-local-variable #'stringp)
  (defvar my/copilot-chat-original-commit-prompt nil
    "Original commit prompt from copilot-chat.")
  (defun my/copilot-chat-set-language-prompt (&rest _)
    "Set commit prompt language based on project setting."
    (unless my/copilot-chat-original-commit-prompt
      (setq my/copilot-chat-original-commit-prompt copilot-chat-commit-prompt))
    (let ((lang-rule (pcase my/project-language
                       ("ja" "Write commit message in Japanese.")
                       ("zh" "Write commit message in Simplified Chinese.")
                       (_ "Write commit message in English."))))
      (setq copilot-chat-commit-prompt
            (concat my/copilot-chat-original-commit-prompt
                    "\n\n### LANGUAGE RULE\n" lang-rule))))
  :config
  (advice-add 'copilot-chat-insert-commit-message :before
              #'my/copilot-chat-set-language-prompt)
  (advice-add 'copilot-chat-regenerate-commit-message :before
              #'my/copilot-chat-set-language-prompt)
  :custom
  (copilot-chat-commit-model "claude-sonnet-4.5")
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
