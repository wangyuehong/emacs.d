;;; flymake-golangci-lint.el --- A flymake backend for golangci-lint -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Wang Yuehong
;; Based on flymake-golangci.el by Jorge Javier Araya Navarro (2019)

;; Author: Wang Yuehong
;; Original Author: Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>
;; URL: https://github.com/wangyuehong/.emacs.d
;; Original URL: https://gitlab.com/shackra/flymake-golangci
;; Version: 1.0.0
;; Package-Requires: ((flymake-easy "0.1") (emacs "30.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; This package provides a Flymake backend for golangci-lint version 2.x.
;; It is based on and inspired by flymake-golangci.el by Jorge Javier Araya Navarro,
;; but has been extensively rewritten to support modern golangci-lint v2 features
;; and provide better performance and configurability.
;;
;; Key differences from the original:
;; - Updated for golangci-lint v2 command-line interface
;; - Modern error pattern matching with rx macro
;; - Emacs 30+ compatibility and modern features
;; - Simplified and cleaner command building logic
;; - Better documentation and configuration examples
;;
;; Usage:
;;   (require 'flymake-golangci-lint)
;;   (add-hook 'go-mode-hook 'flymake-golangci-lint-load)
;;
;; Manual usage:
;;   M-x flymake-golangci-lint-run
;;
;; To disable automatic linting:
;;   (setq flymake-golangci-lint-auto-enable nil)

;;; Code:

(require 'flymake-easy)
(require 'rx)

(defgroup flymake-golangci-lint nil
  "Flymake support for golangci-lint."
  :group 'flymake
  :prefix "flymake-golangci-lint-")

(defcustom flymake-golangci-lint-executable "golangci-lint"
  "The golangci-lint executable to use."
  :type 'string
  :group 'flymake-golangci-lint)

(defcustom flymake-golangci-lint-config nil
  "Path to golangci-lint configuration file."
  :type '(choice (const :tag "Default" nil)
                 (file :tag "Config file"))
  :group 'flymake-golangci-lint)

(defcustom flymake-golangci-lint-timeout "1m"
  "Timeout for golangci-lint execution."
  :type 'string
  :group 'flymake-golangci-lint)

(defcustom flymake-golangci-lint-enable-linters nil
  "List of linters to enable. If nil, use default linters."
  :type '(choice (const :tag "Default" nil)
                 (repeat string))
  :group 'flymake-golangci-lint)

(defcustom flymake-golangci-lint-disable-linters nil
  "List of linters to disable."
  :type '(choice (const :tag "None" nil)
                 (repeat string))
  :group 'flymake-golangci-lint)

(defcustom flymake-golangci-lint-tests t
  "Whether to analyze test files (*_test.go)."
  :type 'boolean
  :group 'flymake-golangci-lint)

(defcustom flymake-golangci-lint-fast nil
  "Run only fast linters for better performance."
  :type 'boolean
  :group 'flymake-golangci-lint)

(defcustom flymake-golangci-lint-auto-enable t
  "Whether to automatically enable flymake-golangci-lint in `go-mode'.
When nil, flymake-golangci-lint won't be automatically enabled,
but can still be triggered manually with `flymake-golangci-lint-run'."
  :type 'boolean
  :group 'flymake-golangci-lint)

;; Error patterns for golangci-lint output using rx macro for readability
(defconst flymake-golangci-lint-err-line-patterns
  `((,(rx bol (group (one-or-more (not ":")))
          ":" (group (one-or-more digit))
          ":" (group (one-or-more digit))
          ": " (group (zero-or-more nonl)) eol) 1 2 3 4)
    (,(rx bol (group (one-or-more (not ":")))
          ":" (group (one-or-more digit))
          ": " (group (zero-or-more nonl)) eol) 1 2 nil 3)))

(defun flymake-golangci-lint-command (filename)
  "Construct a command to run golangci-lint on FILENAME."
  (append
   (list flymake-golangci-lint-executable "run"
         "--output.text.path=stdout"
         "--output.text.colors=false"
         "--output.text.print-issued-lines=false")
   
   ;; Config file
   (when flymake-golangci-lint-config
     (list "-c" flymake-golangci-lint-config))
   
   ;; Timeout
   (when flymake-golangci-lint-timeout
     (list "--timeout" flymake-golangci-lint-timeout))
   
   ;; Enable linters
   (when flymake-golangci-lint-enable-linters
     (apply #'append (mapcar (lambda (linter) (list "-E" linter))
                             flymake-golangci-lint-enable-linters)))
   
   ;; Disable linters
   (when flymake-golangci-lint-disable-linters
     (apply #'append (mapcar (lambda (linter) (list "-D" linter))
                             flymake-golangci-lint-disable-linters)))
   
   ;; Fast mode
   (when flymake-golangci-lint-fast (list "--fast"))
   
   ;; Test flag
   (unless flymake-golangci-lint-tests (list "--tests=false"))
   
   ;; Filename
   (list filename)))

;;;###autoload
(defun flymake-golangci-lint-run ()
  "Run golangci-lint on the current buffer manually and show results in flymake."
  (interactive)
  (if (executable-find flymake-golangci-lint-executable)
      (progn
        (unless flymake-mode (flymake-mode 1))
        (flymake-easy-load 'flymake-golangci-lint-command
                           flymake-golangci-lint-err-line-patterns
                           'tempdir
                           "go"
                           "^[wW]arn"     ; warning regex
                           "^[iI]nfo")    ; info regex
        (flymake-start)
        (message "Running golangci-lint on current buffer..."))
    (user-error "Golangci-lint executable not found in PATH")))

;;;###autoload
(defun flymake-golangci-lint-load ()
  "Configure flymake mode to check the current buffer's Go syntax with golangci-lint."
  (interactive)
  (when (and flymake-golangci-lint-auto-enable
             (executable-find flymake-golangci-lint-executable))
    (flymake-easy-load 'flymake-golangci-lint-command
                       flymake-golangci-lint-err-line-patterns
                       'tempdir
                       "go"
                       "^[wW]arn"     ; warning regex
                       "^[iI]nfo")))  ; info regex

(provide 'flymake-golangci-lint)
;;; flymake-golangci-lint.el ends here