(when (maybe-require-package 'rinari)
  (after-load 'rinari
    (diminish 'rinari-minor-mode "Rin"))
  (global-rinari-mode))

;; (defun update-rails-ctags ()
;;   (interactive)
;;   (let ((default-directory (or (rinari-root) default-directory)))
;;     (shell-command (concat "ctags -a -e -f " rinari-tags-file-name " --tag-relative -R app lib vendor test"))))

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    ;; use universal-ctags
    (shell-command "rails runner 'system(%q(ctags --language-force=ruby -e -R ) + $:.map(&:to_s).select {|d|Dir.exists? d}.uniq.join(%q( )))'")))

(provide 'init-rails)
