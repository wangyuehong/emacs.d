;;; init-misc.el --- Miscellaneous configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package csv-mode
  :hook (csv-mode . csv-align-mode)
  :custom
  (csv-invisibility-default nil))

(use-package flyspell
  :ensure nil
  :if (executable-find "aspell")
  :hook
  ((text-mode  . flyspell-mode)
    (prog-mode . flyspell-prog-mode))
  :config
  (unbind-key "C-;" flyspell-mode-map)
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together")))

(use-package nerd-icons
  :defines (nerd-icons/devicon-alist nerd-icons/octicon-alist)
  :config
  (setf (cdr (assoc "nf-dev-go"   nerd-icons/devicon-alist)) "\xe627")
  (setf (cdr (assoc "nf-oct-ruby" nerd-icons/octicon-alist)) "\xe23e"))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package nerd-icons-ibuffer
  :hook (ibuffer-mode . nerd-icons-ibuffer-mode))

(use-package nerd-icons-completion
  :after marginalia
  :hook ((marginalia-mode . nerd-icons-completion-marginalia-setup)
          (after-init . nerd-icons-completion-mode)))

(use-package doom-modeline
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-buffer-encoding 'nondefault)
  (doom-modeline-buffer-file-name-style 'relative-to-project)
  (doom-modeline-project-detection 'project)
  (doom-modeline-enable-word-count t)
  (doom-modeline-vcs-max-length 36)
  (doom-modeline-modal-icon t)
  (evil-normal-state-tag (propertize "[Normal]"))
  (evil-emacs-state-tag (propertize "[Emacs]"))
  (evil-insert-state-tag (propertize "[Insert]"))
  (evil-motion-state-tag (propertize "[Motion]"))
  (evil-visual-state-tag (propertize "[Visual]"))
  (evil-operator-state-tag (propertize "[Operator]"))
  (evil-replace-state-tag (propertize "[Replace")))

(use-package repeat-help
  :hook (repeat-mode . repeat-help-mode)
  :custom
  (repeat-help-auto t))

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
  (dashboard-items '((recents . 5))))

(use-package helpful
  :bind (("C-h f" . helpful-callable)
          ("C-h v" . helpful-variable)
          ("C-h k" . helpful-key)
          ("C-h x" . helpful-command)))

(defun my/open-file-in-vscode ()
  "Open the current file in Visual Studio Code and jump to the current position."
  (interactive)
  (if (buffer-file-name)
    (let ((file-path (buffer-file-name))
           (line-num (number-to-string (line-number-at-pos)))
           (col-num (number-to-string (current-column))))
      (shell-command (format "code --goto %s:%s:%s" (shell-quote-argument file-path) line-num col-num)))
    (message "Buffer is not visiting a file.")))

(defun my/open-file-in-typora ()
  "Open the current file in Typora."
  (interactive)
  (if-let* ((file-path (buffer-file-name)))
    (shell-command (format "open -a Typora %s" (shell-quote-argument file-path)))
    (message "Buffer is not visiting a file.")))

(defun my/open-in-finder ()
  "Open the current directory in Finder."
  (interactive)
  (when-let* ((dir (cond
                    ((eq major-mode 'dired-mode) (expand-file-name default-directory))
                    ((buffer-file-name) (file-name-directory (buffer-file-name)))
                    (t (expand-file-name default-directory)))))
    (shell-command (format "open %s" (shell-quote-argument dir)))))

(use-package vterm
  :after evil
  :preface
  (defun my/vterm-copy-mode-evil-setup ()
    "Set evil state based on vterm-copy-mode.
Switch to `evil-motion-state` when `vterm-copy-mode` is enabled,
and switch to `evil-emacs-state` otherwise."
    (if vterm-copy-mode
        (evil-motion-state)
      (evil-emacs-state)))
  :hook (vterm-copy-mode . my/vterm-copy-mode-evil-setup)
  :custom
  (vterm-max-scrollback 100000))

(use-package mozc
  :if (executable-find "mozc_emacs_helper")
  :custom
  (mozc-candidate-style 'echo-area)
  (default-input-method "japanese-mozc"))

(use-package popup)
(use-package pyim
  :after popup
  :bind (("M-i" . pyim-toggle-input-ascii)
          :map pyim-mode-map
          ("C-f" . pyim-next-page)
          ("C-b" . pyim-previous-page))
  :custom
  (default-input-method "pyim")
  (pyim-default-scheme 'quanpin)
  (pyim-page-length 6)
  (pyim-page-tooltip 'popup)
  :config
  (require 'popup)
  (pyim-basedict-enable))

(use-package pyim-basedict
  :after pyim)

(use-package im-bridge
  :ensure nil ;; site-lisp/im-bridge
  :hook
  ((after-init . imb-evil-mode)))

;; Input method switching functions and keybindings
(defun my/set-input-method-pyim ()
  "Set default input method to pyim and activate it."
  (interactive)
  (setq default-input-method "pyim")
  (set-input-method "pyim")
  (message "Input method set to: pyim"))

(defun my/set-input-method-mozc ()
  "Set default input method to japanese-mozc and activate it."
  (interactive)
  (setq default-input-method "japanese-mozc")
  (set-input-method "japanese-mozc")
  (message "Input method set to: japanese-mozc"))

(global-set-key (kbd "C-c i j") #'my/set-input-method-mozc)
(global-set-key (kbd "C-c i p") #'my/set-input-method-pyim)

(provide 'init-misc)
;;; init-misc.el ends here
