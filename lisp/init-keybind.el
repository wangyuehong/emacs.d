;;; init-keybind.el --- Keybind configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c @" "hideshow")
  (which-key-add-key-based-replacements "C-c t" "gotest & tag")
  (which-key-add-key-based-replacements "C-c l" "lsp")
  (which-key-add-key-based-replacements "C-c s" "smerge")
  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x 8 e" "emoji")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x c" "copilot")
  (which-key-add-key-based-replacements "C-x c b" "copilot-chat-buffer")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x j" "bookmark")
  (which-key-add-key-based-replacements "C-x r" "register & bookmark")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x x" "buffer")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  :custom
  (which-key-dont-use-unicode nil)
  (which-key-idle-delay 0.3)
  (which-key-show-remaining-keys t)
  (which-key-side-window-max-height 0.35)
  (which-key-sort-order 'which-key-key-order-alpha))

;; https://protesilaos.com/codelog/2024-11-28-basic-emacs-configuration/
(defun my/keyboard-quit-dwim ()
  "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- When in `vterm-copy-mode', close it.
- When in `vterm-mode', call `vterm-reset-cursor-point'.
- When in `copilot-chat-prompt-mode', call `copilot-chat-goto-input'.
- When in `gptel-mode', call `gptel-end-of-response'.
- When in `comint-mode', call `comint-goto-process-mark'.
- In every other case use the regular `keyboard-quit'."
  (interactive)
  (cond
    ((region-active-p)
      (deactivate-mark))
    ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
    ((minibufferp)
      (abort-recursive-edit))
    ((bound-and-true-p vterm-copy-mode)
      (vterm-copy-mode -1))
    ((derived-mode-p 'vterm-mode)
      (vterm-reset-cursor-point))
    ((bound-and-true-p copilot-chat-prompt-mode)
      (copilot-chat-goto-input))
    ((and (bound-and-true-p copilot-chat-markdown-poly-mode)
       (eq major-mode 'markdown-view-mode))
      (copilot-chat-goto-input))
    ((and (boundp 'gptel-mode) gptel-mode)
      (gptel-end-of-response))
    ((derived-mode-p 'comint-mode)
      (comint-goto-process-mark))
    (t
      (keyboard-quit))))

(define-key global-map (kbd "C-g") #'my/keyboard-quit-dwim)

;; minibuffer multiline: Shift+Return inserts newline
(define-key minibuffer-local-map (kbd "C-j") #'newline)
(dolist (key '("S-<return>" "<S-return>" "S-RET"))
  (define-key minibuffer-local-map (kbd key) #'newline))

(provide 'init-keybind)
;;; init-keybind.el ends here
