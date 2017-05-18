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


;;; Robe
(when (maybe-require-package 'robe)
  (after-load 'ruby-mode
    (add-hook 'ruby-mode-hook 'robe-mode))
  (after-load 'company
    (dolist (hook '(ruby-mode-hook inf-ruby-mode-hook html-erb-mode-hook haml-mode))
      (add-hook hook
                (lambda () (sanityinc/local-push-company-backend 'company-robe))))))

(require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;; Customise highlight-symbol to not highlight do/end/class/def etc.
(defun sanityinc/suppress-ruby-mode-keyword-highlights ()
  "Suppress highlight-symbol for do/end etc."
  (set (make-local-variable 'highlight-symbol-ignore-list)
       (list (concat "\\_<" (regexp-opt '("do" "end")) "\\_>"))))
(add-hook 'ruby-mode-hook 'sanityinc/suppress-ruby-mode-keyword-highlights)

;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)

(provide 'init-ruby-mode)
