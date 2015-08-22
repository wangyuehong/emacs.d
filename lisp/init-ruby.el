;;; Basic ruby setup
(require-package 'ruby-mode)
(require-package 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")

(setq ruby-use-encoding-map nil)

(after-load 'ruby-mode
  (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)

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
(add-hook 'robe-mode-hook 'ac-robe-setup)

(require-package 'rubocop)
(add-hook 'ruby-mode-hook 'rubocop-mode)

;;; ri support
(require-package 'yari)
(defalias 'ri 'yari)


;;; YAML

(require-package 'yaml-mode)

;;; rails config
(require-package 'rinari)
(after-load 'rinari
  (diminish 'rinari-minor-mode "Rin"))
(global-rinari-mode)

(add-hook 'rinari-minor-mode-hook (lambda ()
    (define-prefix-command 'wangyh/rinari-minor-mode-map)
    (define-prefix-command 'wangyh/rinari-minor-mode-find-map)
    ;; (local-set-key (kbd "C-c r") 'wangyh/rinari-minor-mode-map)
    (local-set-key (kbd "C-c r") 'wangyh/rinari-minor-mode-find-map)

    (define-key wangyh/rinari-minor-mode-map "'" 'rinari-find-by-context)
    (define-key wangyh/rinari-minor-mode-map ";" 'rinari-find-by-context)
    (define-key wangyh/rinari-minor-mode-map "c" 'rinari-console)
    (define-key wangyh/rinari-minor-mode-map "d" 'rinari-cap)
    (define-key wangyh/rinari-minor-mode-map "e" 'rinari-insert-erb-skeleton)
    (define-key wangyh/rinari-minor-mode-map "g" 'rinari-rgrep)
    (define-key wangyh/rinari-minor-mode-map "p" 'rinari-goto-partial)
    (define-key wangyh/rinari-minor-mode-map "q" 'rinari-sql)
    (define-key wangyh/rinari-minor-mode-map "r" 'rinari-rake)
    (define-key wangyh/rinari-minor-mode-map "s" 'rinari-script)
    (define-key wangyh/rinari-minor-mode-map "t" 'rinari-test)
    (define-key wangyh/rinari-minor-mode-map "w" 'rinari-web-server)
    (define-key wangyh/rinari-minor-mode-map "x" 'rinari-extract-partial)

    (define-key wangyh/rinari-minor-mode-find-map ";" 'rinari-find-by-context)
    (define-key wangyh/rinari-minor-mode-find-map "C" 'rinari-find-cells)
    (define-key wangyh/rinari-minor-mode-find-map "F" 'rinari-find-features)
    (define-key wangyh/rinari-minor-mode-find-map "M" 'rinari-find-mailer)
    (define-key wangyh/rinari-minor-mode-find-map "S" 'rinari-find-steps)
    (define-key wangyh/rinari-minor-mode-find-map "Y" 'rinari-find-sass)
    (define-key wangyh/rinari-minor-mode-find-map "a" 'rinari-find-application)
    (define-key wangyh/rinari-minor-mode-find-map "c" 'rinari-find-controller)
    (define-key wangyh/rinari-minor-mode-find-map "e" 'rinari-find-environment)
    (define-key wangyh/rinari-minor-mode-find-map "f" 'rinari-find-file-in-project)
    (define-key wangyh/rinari-minor-mode-find-map "h" 'rinari-find-helper)
    (define-key wangyh/rinari-minor-mode-find-map "i" 'rinari-find-migration)
    (define-key wangyh/rinari-minor-mode-find-map "j" 'rinari-find-javascript)
    (define-key wangyh/rinari-minor-mode-find-map "l" 'rinari-find-lib)
    (define-key wangyh/rinari-minor-mode-find-map "m" 'rinari-find-model)
    (define-key wangyh/rinari-minor-mode-find-map "n" 'rinari-find-configuration)
    (define-key wangyh/rinari-minor-mode-find-map "o" 'rinari-find-log)
    (define-key wangyh/rinari-minor-mode-find-map "p" 'rinari-find-public)
    (define-key wangyh/rinari-minor-mode-find-map "r" 'rinari-find-rspec)
    (define-key wangyh/rinari-minor-mode-find-map "s" 'rinari-find-script)
    (define-key wangyh/rinari-minor-mode-find-map "t" 'rinari-find-test)
    (define-key wangyh/rinari-minor-mode-find-map "u" 'rinari-find-plugin)
    (define-key wangyh/rinari-minor-mode-find-map "v" 'rinari-find-view)
    (define-key wangyh/rinari-minor-mode-find-map "w" 'rinari-find-worker)
    (define-key wangyh/rinari-minor-mode-find-map "x" 'rinari-find-fixture)
    (define-key wangyh/rinari-minor-mode-find-map "y" 'rinari-find-stylesheet)
    (define-key wangyh/rinari-minor-mode-find-map "z" 'rinari-find-rspec-fixture)
    ))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat ctags-command " -a -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))

(provide 'init-ruby)
