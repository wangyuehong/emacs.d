;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook
  ((text-mode outline-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)

  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package osx-dictionary)

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (evil-normal-state-tag (propertize "[Normal]"))
  (evil-emacs-state-tag (propertize "[Emacs]"))
  (evil-insert-state-tag (propertize "[Insert]"))
  (evil-motion-state-tag (propertize "[Motion]"))
  (evil-visual-state-tag (propertize "[Visual]"))
  (evil-operator-state-tag (propertize "[Operator]"))
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t)
  (doom-modeline-project-detection 'auto))

(use-package xterm-color
  :defines (compilation-environment
            eshell-preoutput-filter-functions
            eshell-output-filter-functions)
  :functions (compilation-filter my-advice-compilation-filter)
  :init
  (setq compilation-environment '("TERM=xterm-256color"))
  (defun my-advice-compilation-filter (f proc string)
    (funcall f proc
             (if (eq major-mode 'rg-mode) ; compatible with `rg'
                 string
               (xterm-color-filter string))))
  (advice-add 'compilation-filter :around #'my-advice-compilation-filter)
  (advice-add 'gud-filter :around #'my-advice-compilation-filter))

(defun open-file-in-vscode ()
  "Open the current file in Visual Studio Code and jump to the current position."
  (interactive)
  (let ((file-path (buffer-file-name))
        (line-num (number-to-string (line-number-at-pos)))
        (col-num (number-to-string (current-column))))
    (shell-command (format "code --goto %s:%s:%s" (shell-quote-argument file-path) line-num col-num))))

(provide 'init-misc)
