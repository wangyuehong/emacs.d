;;; Basic ruby setup
(require-package 'ruby-mode)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")

(setq ruby-use-encoding-map nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  (key-chord-define ruby-mode-map "--" (smartchr '("->" "=>")))
  ;; (modify-syntax-entry ?_ "w")

  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (add-hook 'ruby-mode-hook
            (lambda ()
              (unless (derived-mode-p 'prog-mode)
                (run-hooks 'prog-mode-hook)))))

(add-hook 'ruby-mode-hook 'subword-mode)

;;; Inferior ruby
(require-package 'inf-ruby)
(setq inf-ruby-default-implementation "pry")


;;; Robe
(require-package 'robe)
(add-hook 'ruby-mode-hook 'robe-mode)

(require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)

;; (require-package 'goto-gem)

(require-package 'bundler)

(provide 'init-ruby)
