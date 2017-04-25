(setq-default show-trailing-whitespace t)

;;; Whitespace

(defun sanityinc/no-trailing-whitespace ()
  "Turn off display of trailing whitespace in this buffer."
  (setq show-trailing-whitespace nil))

;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(dolist (hook '(special-mode-hook
                Info-mode-hook
                eww-mode-hook
                term-mode-hook
                comint-mode-hook
                compilation-mode-hook
                twittering-mode-hook
                minibuffer-setup-hook))
  (add-hook hook #'sanityinc/no-trailing-whitespace))


(require-package 'whitespace-cleanup-mode)
(global-whitespace-cleanup-mode t)

(global-set-key [remap just-one-space] 'cycle-spacing)

(require 'whitespace)

;; (global-whitespace-mode t)
(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook))
  (add-hook hook 'whitespace-mode))

(setq whitespace-style
      '(face spaces tabs newline space-mark tab-mark newline-mark lines-tail))
(setq whitespace-line-column 100) ;; config for lines-tail style
(setq whitespace-space-regexp "\\(\x3000+\\)")
(setq whitespace-display-mappings
      '(
        (space-mark ?\x3000 [9633])
        ;;(space-mark 32 [183] [46]) ; normal space,
        (space-mark 160 [164] [95])
        (space-mark 2208 [2212] [95])
        (space-mark 2336 [2340] [95])
        (space-mark 3616 [3620] [95])
        (space-mark 3872 [3876] [95])
        ;;(newline-mark 10 [8629 10]) ; newlne, ?
        (tab-mark 9 [187 9] [92 9]) ; tab, ?
        ))
;; color
(set-face-foreground 'whitespace-newline "#5c5cff")
(set-face-foreground 'whitespace-space "#5c5cff")
(set-face-foreground 'whitespace-tab "#5c5cff")

(provide 'init-whitespace)
