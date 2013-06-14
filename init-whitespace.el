(require 'whitespace)
(global-whitespace-mode t)
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
;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(whitespace-newline ((t (:foreground "dodger blue"))))
 ;;'(whitespace-space ((t (:foreground "dodger blue"))))
 ;;'(whitespace-tab ((t (:foreground "dodger blue"))))
;;)

(provide 'init-whitespace)