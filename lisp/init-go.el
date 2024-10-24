;;; init-go.el --- config go. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(defun project-find-go-module (dir)
  (when-let ((root (locate-dominating-file dir "go.mod")))
    (cons 'go-module root)))

(cl-defmethod project-root ((project (head go-module)))
  (cdr project))

(add-hook 'project-find-functions #'project-find-go-module)

(use-package go-mode
  :bind (:map go-mode-map
          ("C-c i" . go-import-add))
  :hook (go-mode . eglot-ensure)
  :config
  (use-package go-tag)
  (use-package go-gen-test)
  (use-package go-snippets)
  (use-package gotest
    :bind (:map go-mode-map
            ("C-c t t" . go-test-current-test)
            ("C-c t p" . go-test-current-project))
    :config
    (setq go-test-args "-failfast -race")
    :custom
    (go-test-verbose t)))

(provide 'init-go)
;;; init-go.el ends here
