;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package symbol-overlay
  :diminish
  :bind (("M-i" . symbol-overlay-put)
         ("M-n" . symbol-overlay-jump-next)
         ("M-p" . symbol-overlay-jump-prev)
         :map symbol-overlay-map
         ("c" . symbol-overlay-remove-all)
         )
  :hook ((prog-mode yaml-mode) . symbol-overlay-mode)
  :custom
  (symbol-overlay-idle-time 0.1)
  :custom-face
  (symbol-overlay-default-face ((t (:inherit highlight bold))))
  (symbol-overlay-face-1 ((t (:background "brightblue" :foreground "black"))))
  (symbol-overlay-face-2 ((t (:background "brightgreen" :foreground "black"))))
  (symbol-overlay-face-3 ((t (:background "brightred" :foreground "black"))))
  (symbol-overlay-face-4 ((t (:background "brightmagenta" :foreground "black"))))
  (symbol-overlay-face-5 ((t (:background "cyan" :foreground "black"))))
  (symbol-overlay-face-6 ((t (:background "brightyellow" :foreground "black"))))
  (symbol-overlay-face-7 ((t (:background "brightblack" :foreground "white"))))
  (symbol-overlay-face-8 ((t (:background "magenta" :foreground "black"))))
  )

(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode)
  :custom
  (show-paren-when-point-inside-paren t)
  (show-paren-when-point-in-periphery t))

(use-package fic-mode :hook prog-mode)

(use-package rainbow-mode :diminish
  :hook (emacs-lisp-mode . rainbow-mode))

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package display-line-numbers
  :ensure nil
  :hook ((prog-mode yaml-mode) . display-line-numbers-mode))

(use-package whitespace
  :ensure nil
  :diminish
  :hook (((prog-mode yaml-mode markdown-mode conf-mode) . whitespace-mode))
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
  (set-face-foreground 'whitespace-space "blue") ;; -> "　"
  (set-face-foreground 'whitespace-tab "brightblack")
  (set-face-foreground 'whitespace-space-before-tab "brightmagenta")
  )

(provide 'init-highlight)
