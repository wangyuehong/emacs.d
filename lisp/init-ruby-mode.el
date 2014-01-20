;;; Basic ruby setup
(require-package 'ruby-mode)
(require-package 'ruby-hash-syntax)

(add-auto-mode 'ruby-mode
               "Rakefile\\'" "\\.rake\\'" "\\.rxml\\'"
               "\\.rjs\\'" "\\.irbrc\\'" "\\.pryrc\\'" "\\.builder\\'" "\\.ru\\'"
               "\\.gemspec\\'" "Gemfile\\'" "Kirkfile\\'")

(setq ruby-use-encoding-map nil)

(after-load 'ruby-mode
  (setq-default ruby-use-smie nil)
  (define-key ruby-mode-map (kbd "RET") 'reindent-then-newline-and-indent)
  ;; (define-key ruby-mode-map (kbd "TAB") 'indent-for-tab-command)
  )

(defun my/ruby-mode-hook ()
  ;; Stupidly the non-bundled ruby-mode isn't a derived mode of
  ;; prog-mode: we run the latter's hooks anyway in that case.
  (unless (derived-mode-p 'prog-mode)
    (run-hooks 'prog-mode-hook)))

(add-hook 'ruby-mode-hook 'my/ruby-mode-hook)

;;; Inferior ruby
(require-package 'inf-ruby)

;;; Ruby compilation
(require-package 'ruby-compilation)

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

(require-package 'haml-mode)
(require 'haml-mode)

(require-package 'slim-mode)
(require 'slim-mode)

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command (concat ctags-command " -a -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))

(provide 'init-ruby-mode)
