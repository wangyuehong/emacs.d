(require 'whitespace)

;; (global-whitespace-mode t)
(add-hook 'prog-mode-hook (lambda ()
                            (whitespace-mode t)))
(add-hook 'prog-mode-hook (lambda ()
                            (setq show-trailing-whitespace t)))
(setq whitespace-style
      '(face spaces tabs newline space-mark tab-mark newline-mark))
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
