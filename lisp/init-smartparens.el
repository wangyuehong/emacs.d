(require-package 'smartparens)
(require 'smartparens-config)

(smartparens-global-mode t)

(show-smartparens-global-mode t)

(set-face-background 'sp-show-pair-match-face "#4e4e4e")

;; markdown-mode
(sp-with-modes '(markdown-mode gfm-mode rst-mode)
  (sp-local-pair "*" "*" :bind "C-*")
  (sp-local-tag "2" "**" "**")
  (sp-local-tag "s" "```scheme" "```")
  (sp-local-tag "<"  "<_>" "</_>" :transform 'sp-match-sgml-tags))

;; html-mode
(sp-with-modes '(html-mode sgml-mode)
  (sp-local-pair "<" ">"))


(provide 'init-smartparens)
