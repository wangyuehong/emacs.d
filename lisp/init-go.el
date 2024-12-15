;;; init-go.el --- config go. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :defines (go-mode-map)
  :preface
  (defun project-find-go-mod (dir)
    (when-let ((root (locate-dominating-file dir "go.mod")))
      (cons 'go-module root)))

  (cl-defmethod project-root ((project (head go-module)))
    (cdr project))

  (defun my/set-go-project-find-functions ()
    "Add project-find-go-mod to project-find-functions in go-mode."
    (add-to-list 'project-find-functions #'project-find-go-mod))
  :bind (:map go-mode-map
          ("C-c i" . go-import-add))
  :hook ((go-mode  . eglot-ensure)
          (go-mode . my/set-go-project-find-functions)))

(use-package go-tag :after go-mode)
(use-package go-gen-test :after go-mode)
(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
          ("C-c t t" . go-test-current-test)
          ("C-c t p" . go-test-current-project))
  :config
  (setq go-test-args "-failfast -race")
  :custom
  (go-test-verbose t))

(provide 'init-go)
;;; init-go.el ends here
