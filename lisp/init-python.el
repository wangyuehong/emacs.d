;;; init-python.el --- config python. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python
  :ensure nil
  :hook (python-ts-mode . eglot-ensure))

(use-package lazy-ruff
  :defines lazy-ruff-only-format-buffer
  :if (executable-find "ruff")
  :hook (python-ts-mode . lazy-ruff-mode)
  :config
  (setq lazy-ruff-only-format-buffer t))

(provide 'init-python)
;;; init-python.el ends here
