;; init-go.el --- Golang configurations.  -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; Go packages:
;; go get -u github.com/mdempsky/gocode # github.com/nsf/gocode
;; go get -u github.com/rogpeppe/godef
;; go get -u golang.org/x/tools/cmd/goimports
;; go get -u golang.org/x/tools/cmd/guru
;; go get -u golang.org/x/tools/cmd/gorename
;; go get -u golang.org/x/tools/cmd/gotype
;; go get -u golang.org/x/tools/cmd/godoc
;; go get -u github.com/derekparker/delve/cmd/dlv
;; go get -u github.com/josharian/impl
;; go get -u github.com/cweill/gotests/...
;; go get -u github.com/fatih/gomodifytags
;; go get -u github.com/davidrjenni/reftools/cmd/fillstruct

(use-package go-mode
  :bind (:map go-mode-map
              ([remap xref-find-definitions] . godef-jump)
              ("C-c R" . go-remove-unused-imports)
              ("<f1>" . godoc-at-point))
  :init
  (setq gofmt-command "goimports")
  :config
  (add-hook 'before-save-hook #'gofmt-before-save)
  (add-hook 'go-mode-hook #'lsp)
  )

(use-package go-snippets)
(use-package toml-mode)

(use-package go-dlv :after go-mode)
(use-package go-fill-struct :after go-mode)
(use-package go-impl :after go-mode)
(use-package go-rename :after go-mode)
(use-package golint :after go-mode)
(use-package govet :after go-mode)

(use-package go-tag
  :after go-mode
  :bind (:map go-mode-map
              ("C-c t" . go-tag-add)
              ("C-c T" . go-tag-remove))
  :config (setq go-tag-args (list "-transform" "camelcase")))

(use-package gotest
  :after go-mode
  :bind (:map go-mode-map
              ("C-c a" . go-test-current-project)
              ("C-c m" . go-test-current-file)
              ("C-c ." . go-test-current-test)
              ("C-c x" . go-run)))

(use-package go-gen-test
  :after go-mode
  :bind (:map go-mode-map
              ("C-c C-t" . go-gen-test-dwim)))

(use-package go-guru
  :after go-mode
  :bind (:map go-mode-map
              ;; ([remap xref-find-definitions] . go-guru-definition)
              ([remap xref-find-references] . go-guru-referrers)))

;; Run: M-x `go-projectile-install-tools'
(use-package go-projectile
  :commands (go-projectile-mode go-projectile-switch-project)
  :after (projectile-mode go-mode)
  :hook ((go-mode . go-projectile-mode)
         (projectile-after-switch-project . go-projectile-switch-project))
  )

;; (require-package 'flycheck-gometalinter)
;; (eval-after-load 'flycheck
;;   '(add-hook 'flycheck-mode-hook #'flycheck-gometalinter-setup))

(provide 'init-go)
;;; init-go.el ends here
