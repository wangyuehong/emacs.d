;;; init-go.el --- config go. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package go-mode
  :defines (go-mode-map)
  :preface
  (defun my/project-find-go-root (dir)
    "Find Go workspace root for DIR preferring go.work over go.mod."
    (when-let ((root (or (locate-dominating-file dir "go.work")
                         (locate-dominating-file dir "go.mod"))))
      (cons 'go root)))

  (cl-defmethod project-root ((project (head go)))
    "Return root directory for a Go project."
    (cdr project))

  (defun my/set-go-project-find-functions ()
    "Add Go project finder to `project-find-functions` in `go-mode`."
    (add-to-list 'project-find-functions #'my/project-find-go-root))
  :bind (:map go-mode-map
          ("C-c i" . go-import-add))
  :hook ((go-mode  . eglot-ensure)
          (go-mode . my/set-go-project-find-functions)))

(use-package go-tag
  :after go-mode
  :bind (:map go-mode-map
          ("C-c t a" . go-tag-add)
          ("C-c t x" . go-tag-remove)
          ("C-c t r" . go-tag-refresh)))

(use-package flymake-golangci-lint
  :ensure nil ;; site-lisp/flymake-golangci-lint
  :if (executable-find "golangci-lint")
  :hook (go-mode . flymake-golangci-lint-load))

(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
          ("C-c t t" . go-test-current-test)
          ("C-c t f" . go-test-current-file)
          ("C-c t b" . go-test-current-benchmark)
          ("C-c t c" . go-test-current-coverage)
          ("C-c t p" . go-test-current-project))
  :config
  (setq go-test-args "-failfast -race")
  :custom
  (go-test-verbose t))

(provide 'init-go)
;;; init-go.el ends here
