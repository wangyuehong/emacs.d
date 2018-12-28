;;; Basic ruby setup
(require-package 'ruby-mode)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")
(add-auto-mode 'conf-mode "Gemfile\\.lock\\'")

(setq-default
 ruby-use-encoding-map nil
 ruby-insert-encoding-magic-comment nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  (key-chord-define ruby-mode-map "--" (smartchr '("->" "=>")))
  ;; (modify-syntax-entry ?_ "w")
  )

(add-hook 'ruby-mode-hook 'subword-mode)

(require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

(provide 'init-ruby)
