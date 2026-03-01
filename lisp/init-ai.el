;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :hook
  (((prog-mode git-commit-setup yaml-mode markdown-mode) . copilot-mode)
    (copilot-mode . copilot-nes-mode))
  :bind
  (("C-c x" . copilot-diagnose)
    :map copilot-completion-map
    ("TAB" . copilot-accept-completion)
    ([tab] . copilot-accept-completion)
    ("C-f" . copilot-accept-completion-by-line)
    ("M-f" . copilot-accept-completion-by-word)
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

;; Project language setting, used for commit message generation, etc.
;; Configure per-project via .dir-locals.el in project root:
;;   ((nil . ((my/project-language . "ja"))))  ; Japanese
;;   ((nil . ((my/project-language . "zh"))))  ; Chinese
(defvar-local my/project-language "en"
  "Project documentation language. Values: \"zh\", \"ja\", \"en\".")
(put 'my/project-language 'safe-local-variable #'stringp)

(use-package copilot-commit
  :ensure nil
  :after git-commit
  :bind (:map git-commit-mode-map
         ("C-c i" . copilot-commit-insert-message)
         ("C-c I" . copilot-commit-regenerate-message))
  :custom
  (copilot-commit-model my/copilot-commit-model)
  (copilot-commit-prompt-suffix
   (lambda ()
     (pcase my/project-language
       ("ja" "\n\nWrite commit message in Japanese.")
       ("zh" "\n\nWrite commit message in Simplified Chinese.")
       (_ "")))))

(use-package agentmux
  :ensure nil ;; site-lisp/agentmux
  :bind ("C-c a" . agentmux-transient))

(provide 'init-ai)
;;; init-ai.el ends here
