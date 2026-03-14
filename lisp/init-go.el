;;; init-go.el --- config go. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-ts-mode
  :ensure nil
  :preface
  (defun my/project-find-go-root (dir)
    "Find Go workspace root for DIR preferring go.work over go.mod."
    (when-let* ((root (or (locate-dominating-file dir "go.work")
                         (locate-dominating-file dir "go.mod"))))
      (cons 'go root)))

  (cl-defmethod project-root ((project (head go)))
    "Return root directory for a Go project."
    (cdr project))

  (defun my/set-go-project-find-functions ()
    "Add Go project finder to `project-find-functions'."
    (add-to-list 'project-find-functions #'my/project-find-go-root))

  (defun my/go-ts-mode-enhance-font-lock ()
    "Enable extra font-lock features for `go-ts-mode'."
    (treesit-font-lock-recompute-features '(function property) nil))
  :hook ((go-ts-mode . eglot-ensure)
         (go-ts-mode . my/set-go-project-find-functions)
         (go-ts-mode . my/go-ts-mode-enhance-font-lock)))

(use-package go-tag
  :after go-ts-mode
  :bind (:map go-ts-mode-map
          ("C-c t a" . go-tag-add)
          ("C-c t x" . go-tag-remove)
          ("C-c t r" . go-tag-refresh)))

(use-package gotest
  :after go-ts-mode
  :bind (:map go-ts-mode-map
          ("C-c t t" . go-test-current-test)
          ("C-c t f" . go-test-current-file)
          ("C-c t b" . go-test-current-benchmark)
          ("C-c t c" . go-test-current-coverage)
          ("C-c t p" . go-test-current-project))
  :custom
  (go-test-verbose t)
  (go-test-args "-failfast -race"))

(provide 'init-go)
;;; init-go.el ends here
