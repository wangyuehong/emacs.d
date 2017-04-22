(require-package 'highlight-symbol)

(require 'highlight-symbol)
(setq highlight-symbol-idle-delay 0.5)

(global-set-key (kbd "<f3>") 'highlight-symbol-next)
(global-set-key (kbd "<f4>") 'highlight-symbol-prev)
(global-set-key (kbd "<f5>") 'highlight-symbol)
(global-set-key (kbd "ESC <f3>") 'highlight-symbol-remove-all)
(global-set-key (kbd "M-<f3>") 'highlight-symbol-remove-all)

(set-face-attribute 'highlight-symbol-face nil
                    :inherit nil
                    :background "#626262")

(setq highlight-symbol-colors (quote ("#5c5cff" "#ff0000" "#00ff00" "#ff00ff" "#ffff00")))

(dolist (hook '(prog-mode-hook html-mode-hook css-mode-hook yaml-mode-hook))
  (add-hook hook 'highlight-symbol-mode)
  (add-hook hook 'highlight-symbol-nav-mode))
(add-hook 'org-mode-hook 'highlight-symbol-nav-mode)
(after-load 'highlight-symbol
  (diminish 'highlight-symbol-mode)
  (defadvice highlight-symbol-temp-highlight (around sanityinc/maybe-suppress activate)
    "Suppress symbol highlighting while isearching."
    (unless (or isearch-mode
                (and (boundp 'multiple-cursors-mode) multiple-cursors-mode))
      ad-do-it)))

(require-package 'fic-mode)

(require 'fic-mode)
(add-hook 'prog-mode-hook 'fic-mode)
(set-face-foreground 'fic-face "#7f7f7f")
(set-face-background 'fic-face "#ffff00")

(provide 'init-highlight)
