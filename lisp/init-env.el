;;; init-env.el --- env configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:
(use-package exec-path-from-shell
  :if (display-graphic-p)
  :hook (after-init . exec-path-from-shell-initialize))

(use-package envrc
  :if (executable-find "direnv")
  :hook (after-init . envrc-global-mode))

(provide 'init-env)
;;; init-env.el ends here
