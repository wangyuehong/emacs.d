;;; init-highlight.el --- Highlighting configurations -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package symbol-overlay
  :defines symbol-overlay-map
  :functions (turn-off-symbol-overlay turn-on-symbol-overlay)
  :bind (:map symbol-overlay-map
        ("<escape>" . symbol-overlay-remove-all))
  :hook (((prog-mode yaml-mode protobuf-mode) . symbol-overlay-mode)
         (iedit-mode . turn-off-symbol-overlay)
         (iedit-mode-end . turn-on-symbol-overlay))
  :config
  (unbind-key "w" symbol-overlay-map)
  (unbind-key "e" symbol-overlay-map)
  (with-no-warnings
    ;; Disable symbol highlighting while selecting
    (defun turn-off-symbol-overlay (&rest _)
      "Turn off symbol highlighting."
      (interactive)
      (symbol-overlay-mode -1))
    (advice-add #'set-mark :after #'turn-off-symbol-overlay)

    (defun turn-on-symbol-overlay (&rest _)
      "Turn on symbol highlighting."
      (interactive)
      (when (derived-mode-p 'prog-mode 'yaml-mode)
        (symbol-overlay-mode 1)))
    (advice-add #'deactivate-mark :after #'turn-on-symbol-overlay))
  :custom
  (symbol-overlay-idle-time 0.1))

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package hl-todo
  :defines hl-todo-include-modes
  :hook (after-init . global-hl-todo-mode)
  :config
  (with-eval-after-load 'protobuf-mode
    (add-to-list 'hl-todo-include-modes 'protobuf-mode)))

(use-package rainbow-mode
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook ((prog-mode protobuf-mode) . rainbow-delimiters-mode))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode protobuf-mode) . display-line-numbers-mode))

(use-package whitespace
  :ensure nil
  :hook ((prog-mode yaml-mode markdown-mode conf-mode protobuf-mode) . whitespace-mode)
  :custom
  (whitespace-line-column 120) ;; config for lines-tail style
  (whitespace-style
   '(face spaces tabs space-before-tab newline
          space-mark tab-mark newline-mark lines-tail))
  (whitespace-space-regexp "\\(\x3000+\\)") ;; -> "　"
  (whitespace-display-mappings
   '(
     (space-mark ?\x3000 [9633])
     ;; (space-mark 32 [183] [46]) ; normal space
     (space-mark 160 [164] [95])
     (space-mark 2208 [2212] [95])
     (space-mark 2336 [2340] [95])
     (space-mark 3616 [3620] [95])
     (space-mark 3872 [3876] [95])
     ;; (newline-mark 10 [8629 10]) ;; newlne
     (tab-mark 9 [187 9] [92 9]) ;; tab
     ))

  :config
  ;; (set-face-foreground 'whitespace-newline "brightblack")
  (set-face-foreground 'whitespace-space "brightblue") ;; -> "　"
  (set-face-foreground 'whitespace-tab "brightblack")
  (set-face-foreground 'whitespace-space-before-tab "brightmagenta"))

(provide 'init-highlight)
;;; init-highlight.el ends here
