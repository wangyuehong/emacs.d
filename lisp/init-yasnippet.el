;;; init-yasnippet.el --- yasnippet configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package yasnippet
  :diminish
  :hook (after-init . yas-global-mode)
  :init
  ;; http://stackoverflow.com/questions/7619640/emacs-latex-yasnippet-why-are-newlines-inserted-after-a-snippet
  (setq-default mode-require-final-newline nil)

  ;; default hotkey `C-c C-s` is still valid
  ;; give yas-dropdown-prompt in yas/prompt-functions a chance
  (setq yas-prompt-functions '(yas-dropdown-prompt
                               yas-ido-prompt
                               yas-completing-prompt))

  ;; use yas-completing-prompt when ONLY when `M-x yas-insert-snippet'
  ;; thanks to capitaomorte for providing the trick.
  (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let* ((yas-prompt-functions '(yas-completing-prompt)))
      ad-do-it))
  )

(add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
;;; init-yasnippet.el ends here
