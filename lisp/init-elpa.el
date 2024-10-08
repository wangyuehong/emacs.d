;;; init-elpa.el --- init elpa config. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

;;; Install into separate package dirs for each Emacs version, to prevent bytecode incompatibility
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
                         user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))


(setq package-archives
  '(("gnu"    . "https://elpa.gnu.org/packages/")
     ("melpa"  . "https://melpa.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ;; ("gnu-devel" . "https://elpa.gnu.org/devel/")
     ))

(provide 'init-elpa)
;;; init-elpa.el ends here
