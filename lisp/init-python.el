;;; init-python.el --- config python. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :ensure nil
  :hook (python-mode . eglot-ensure))

(use-package lazy-ruff
  :if (executable-find "ruff")
  :hook (python-mode . lazy-ruff-mode)
  :config
  (setq lazy-ruff-only-format-buffer t))

(provide 'init-python)
;;; init-python.el ends here
