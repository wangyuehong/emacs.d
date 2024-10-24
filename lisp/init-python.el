;;; init-python.el --- config python. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package python-mode
  :ensure nil
  :hook (python-mode . eglot-ensure))

(use-package ruff-format
  :hook (python-mode . ruff-format-on-save-mode)
  :when (executable-find "ruff"))

(provide 'init-python)
;;; init-python.el ends here
