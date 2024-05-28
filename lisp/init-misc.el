;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package csv-mode
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-invisibility-default nil))

(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :hook
  ((text-mode outline-mode) . flyspell-mode)
  (prog-mode . flyspell-prog-mode)
  :config
  (unbind-key "C-;" flyspell-mode-map)
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package nerd-icons
  :config
  (setf (cdr (assoc "nf-dev-go"   nerd-icons/devicon-alist)) "\xe627")
  (setf (cdr (assoc "nf-oct-ruby" nerd-icons/octicon-alist)) "\xe23e"))

(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-vcs-max-length 36)
  (doom-modeline-modal-icon nil)
  (evil-normal-state-tag (propertize "[Normal]"))
  (evil-emacs-state-tag (propertize "[Emacs]"))
  (evil-insert-state-tag (propertize "[Insert]"))
  (evil-motion-state-tag (propertize "[Motion]"))
  (evil-visual-state-tag (propertize "[Visual]"))
  (evil-operator-state-tag (propertize "[Operator]"))
  (evil-replace-state-tag (propertize "[Replace")))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  :config
  (nerd-icons-completion-mode))

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
  "Open the current file in Visual Studio Code and jump to the current position.
If the current buffer is not associated with a file, open a new Visual Studio Code window."
  (interactive)
  (if (buffer-file-name)
      (let ((file-path (buffer-file-name))
            (line-num (number-to-string (line-number-at-pos)))
            (col-num (number-to-string (current-column))))
        (shell-command (format "code --goto %s:%s:%s" (shell-quote-argument file-path) line-num col-num)))
    (shell-command "code")))

(use-package repeat-help
  :hook (repeat-mode . repeat-help-mode)
  :custom
  (repeat-help-auto t))

(use-package envrc
  :if (executable-find "direnv")
  :config
  (envrc-global-mode))

(use-package fanyi
  :commands fanyi-dwim fanyi-dwim2)

(use-package breadcrumb
  :hook
  (after-init . breadcrumb-mode))

(use-package open-junk-file
  :custom
  (open-junk-file-format "~/junk/%Y-%m-%dT%H-%M-%S.md"))

(use-package dashboard
  :hook ((after-init . dashboard-setup-startup-hook))
  :custom
  (dashboard-projects-backend 'project-el)
  (dashboard-display-icons-p t)
  (dashboard-icon-type 'nerd-icons)
  (dashboard-path-style 'truncate-middle)
  (dashboard-set-heading-icons t)
  (dashboard-set-file-icons t)
  (dashboard-set-init-info t)
  (dashboard-items '((recents  . 10)
                     (projects . 10))))

(provide 'init-misc)
