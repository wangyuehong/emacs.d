;;; init-general.el --- config general. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package general
  :demand t
  :config
  (general-evil-setup t)
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix ";"
    "c"  '(:wk "copilot")
    "cD" '(copilot-diagnose      :wk "copilot-diagnose")
    "cc" '(copilot-chat-insert-commit-message :wk "copilot-chat-insert-commit-message")
    "cr" '(copilot-chat-review   :wk "copilot-chat-review")
    "cd" '(copilot-chat-doc      :wk "copilot-chat-doc")
    "cf" '(copilot-chat-fix      :wk "copilot-chat-fix")
    "co" '(copilot-chat-optimize :wk "copilot-chat-optimize")
    "ct" '(copilot-chat-test     :wk "copilot-chat-test")
    "cr" '(copilot-chat-reset    :wk "copilot-chat-reset"))
  (general-define-key
    :states '(normal motion visual)
    :keymaps 'override
    :prefix "SPC"
    "SPC" '(:wk "+++")
    "SPC w" '(save-buffer    :wk "save-buffer")
    "SPC l" '(consult-ls-git :wk "ls-git")

    "a" 'embark-act

    "b"  '(:wk "buffer")
    "bi" '(ibuffer                :wk "ibuffer")
    "bb" '(consult-buffer         :wk "consult-buffer")
    "bk" '(kill-buffer-and-window :wk "kill-buffer")
    "bK" '(kill-other-buffers     :wk "kill-other-buffers")
    "br" '(rename-buffer          :wk "rename-buffer")
    "bw" '(save-buffer            :wk "save-buffer")
    "bW" '(save-some-buffers      :wk "save-all-buffers")
    "bj" '(consult-bookmark       :wk "bookmark-jump")
    "bd" '(bookmark-delete        :wk "bookmark-delete")

    "c"  '(:wk "code")
    "cc" '(evilnc-comment-or-uncomment-lines :wk "comment-or-uncomment")
    "ce" '(evil-iedit-state/iedit-mode       :wk "iedit")
    "ci" '(imenu                             :wk "imenu")
    "ch" '(symbol-overlay-put                :wk "symbol-overlay-put")
    "cq" '(quickrun                          :wk "quickrun")
    "cf" '(consult-flymake                   :wk "consult-flymake")
    "cF" '(flymake-show-project-diagnostics  :wk "flymake-show-project-diagnostics")

    "cl"  '(:wk "lsp")
    "cla" '(eglot-code-actions        :wk "eglot-code-action")
    "cli" '(eglot-find-implementation :wk "eglot-find-implementation")
    "clr" '(eglot-rename:wk           :wk "eglot-rename")
    "clk" '(eglot-reconnect           :wk "eglot-reconnect")

    "g"  '(:wk "git")
    "gg" '(magit-status :wk "magit-status")
    "gd" '(:keymap diff-hl-command-map :package diff-hl :wk "diff-hl")

    "s"  '(:wk "search")
    "ss" '(rg-project                  :wk "rg-project")
    "sS" '(rg                          :wk "rg")
    "sp" '(consult-ripgrep             :wk "ripgrep")
    "sr" '(consult-yank-from-kill-ring :wk "yank-from-kill-ring")
    "so" '(consult-line                :wk "occur")

    "t"  '(:wk "toggles")
    "tl" '(toggle-truncate-lines     :wk "toggle-truncate-lines")
    "tn" '(display-line-numbers-mode :wk "toggle-display-line-numbers")
    "tp" '(electric-pair-mode        :wk "electric-pair-mode")

    "x"  'execute-extended-command))

(provide 'init-general)
;;; init-general.el ends here
