;;; rails config
(require-package 'rinari)
(after-load 'rinari
  (diminish 'rinari-minor-mode "Rin"))
(global-rinari-mode)

(defun update-rails-ctags ()
  (interactive)
  (let ((default-directory (or (rinari-root) default-directory)))
    (shell-command "rails runner 'system(%q(ctags --languages=ruby -e -R ) + $:.map(&:to_s).select {|d|Dir.exists? d}.uniq.join(%q( )))'")))

;; (defun update-rails-ctags ()
;;   (interactive)
;;   (let ((default-directory (or (rinari-root) default-directory)))
;;     (shell-command (concat "ctags -a -e -f " rinari-tags-file-name
;;                            " --tag-relative -R app lib vendor test"))))

(when (maybe-require-package 'projectile-rails)
  (add-hook 'projectile-mode-hook 'projectile-rails-on)
  )


(provide 'init-rails)
