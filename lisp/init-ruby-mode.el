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

(require-package 'goto-gem)

;;; Robe
(when (maybe-require-package 'robe)
  (after-load 'ruby-mode
    (add-hook 'ruby-mode-hook 'robe-mode))
  (after-load 'company
    (dolist (hook (mapcar 'derived-mode-hook-name '(ruby-mode inf-ruby-mode html-erb-mode haml-mode)))
      (add-hook hook
                (lambda () (sanityinc/local-push-company-backend 'company-robe))))))

(require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)

(provide 'init-ruby-mode)
