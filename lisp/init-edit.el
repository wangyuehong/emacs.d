;;; init-edit.el --- edit config . -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-interval 3))

(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'reverse)
  (uniquify-separator " • ")
  (uniquify-after-kill-buffer-p t)
  (uniquify-ignore-buffers-re "^\\*"))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package subword
  :ensure nil
  :hook ((prog-mode . subword-mode)
          (minibuffer-setup . subword-mode)))

(use-package expand-region
  :custom
  (expand-region-contract-fast-key "z"))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package iedit)

(use-package markdown-mode
  :functions (markdown--command-map-prompt markdown--style-map-prompt)
  :init
  (advice-add #'markdown--command-map-prompt :override #'ignore)
  (advice-add #'markdown--style-map-prompt   :override #'ignore)
  :mode ("README\\(?:\\.md\\)?\\'" . gfm-mode)
  :hook ((markdown-mode . visual-line-mode))
  :custom
  (markdown-enable-wiki-links t)
  (markdown-italic-underscore t)
  (markdown-asymmetric-header t)
  (markdown-fontify-code-blocks-natively t))

(use-package mermaid-mode)

(use-package editorconfig
  :hook (after-init . editorconfig-mode))

(use-package simple
  :ensure nil
  :custom
  (column-number-mode t)
  (line-number-mode t)
  (size-indication-mode t)
  (line-move-visual nil)
  (column-number-indicator-zero-based nil)
  (track-eol t)
  (save-interprogram-paste-before-kill t)
  (kill-do-not-save-duplicates t)
  (set-mark-command-repeat-pop t)
  :init
  (setq-default show-trailing-whitespace nil)); Don't show trailing whitespace by default

(use-package ediff
  :ensure nil
  :hook ((ediff-before-setup . ediff-save-window-conf)
          (ediff-quit         . ediff-restore-window-conf))
  :config
  (defvar local-ediff-saved-window-conf nil)
  (defun ediff-save-window-conf ()
    (setq local-ediff-saved-window-conf (current-window-configuration)))
  (defun ediff-restore-window-conf ()
    (when (window-configuration-p local-ediff-saved-window-conf)
      (set-window-configuration local-ediff-saved-window-conf)))
  :custom
  (ediff-highlight-all-diffs t)
  (ediff-window-setup-function 'ediff-setup-windows-plain)
  (ediff-split-window-function 'split-window-horizontally)
  (ediff-merge-split-window-function 'split-window-horizontally))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package undo-fu)

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

(provide 'init-edit)
;;; init-edit.el ends here
