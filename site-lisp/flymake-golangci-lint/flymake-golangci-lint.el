;;; flymake-golangci-lint.el --- A flymake backend for golangci-lint -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Wang Yuehong
;; Based on flymake-golangci.el by Jorge Javier Araya Navarro (2019)

;; Author: Wang Yuehong
;; Original Author: Jorge Javier Araya Navarro <jorgejavieran@yahoo.com.mx>
;; URL: https://github.com/wangyuehong/.emacs.d
;; Original URL: https://gitlab.com/shackra/flymake-golangci
;; Version: 1.0.0
;; Package-Requires: ((emacs "30.1"))

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
;; Modern flymake backend for golangci-lint.
;; 
;; Features:
;; - Native flymake integration (no external dependencies)
;; - Async processing with proper process management
;; - Simple plist-based configuration
;; - Legacy configuration compatibility
;; - Configurable linters, timeout, and options
;;
;; Usage:
;;   (require 'flymake-golangci-lint)
;;   (add-hook 'go-mode-hook 'flymake-golangci-lint-load)
;;
;; Configuration:
;;   ;; New plist-based config (recommended)
;;   (setq flymake-golangci-lint-options
;;         '(:fast t :timeout "10s" :disable-linters ("typecheck")))
;;
;;   ;; Legacy config (still supported)
;;   (setq flymake-golangci-lint-fast t
;;         flymake-golangci-lint-timeout "10s")

;;; Code:

(require 'flymake)
(require 'rx)
(require 'cl-lib)

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
  "List of linters to enable.  If nil, use default linters."
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

;; Structured configuration (plist-based)
(defcustom flymake-golangci-lint-options
  '(:config nil
    :timeout "1m"
    :enable-linters nil
    :disable-linters nil
    :tests t
    :fast nil)
  "Golangci-lint command options as a plist."
  :type '(plist :key-type symbol :value-type sexp)
  :group 'flymake-golangci-lint)


;; Configuration helpers
(defun flymake-golangci-lint--get-option (key)
  "Get option KEY from new or legacy configuration."
  ;; Check if key exists in plist
  (if (plist-member flymake-golangci-lint-options key)
      (plist-get flymake-golangci-lint-options key)
    ;; Fall back to legacy config
    (pcase key
      (:config flymake-golangci-lint-config)
      (:timeout flymake-golangci-lint-timeout)
      (:enable-linters flymake-golangci-lint-enable-linters)
      (:disable-linters flymake-golangci-lint-disable-linters)
      (:tests flymake-golangci-lint-tests)
      (:fast flymake-golangci-lint-fast))))

(defun flymake-golangci-lint--build-linter-args (flag linters)
  "Build linter arguments with FLAG for LINTERS list."
  (cl-loop for linter in linters
           append (list flag linter)))

(defun flymake-golangci-lint-command (source-dir)
  "Construct a command to run golangci-lint on SOURCE-DIR."
  (append
   (list flymake-golangci-lint-executable "run")
   
   ;; Config file
   (when-let ((config (flymake-golangci-lint--get-option :config)))
     (list "-c" config))
   
   ;; Timeout
   (when-let ((timeout (flymake-golangci-lint--get-option :timeout)))
     (list "--timeout" timeout))
   
   ;; Enable linters
   (when-let ((enable (flymake-golangci-lint--get-option :enable-linters)))
     (flymake-golangci-lint--build-linter-args "-E" enable))
   
   ;; Disable linters
   (when-let ((disable (flymake-golangci-lint--get-option :disable-linters)))
     (flymake-golangci-lint--build-linter-args "-D" disable))
   
   ;; Fast mode
   (when (flymake-golangci-lint--get-option :fast)
     (list "--fast-only"))
   
   ;; Test flag
   (unless (flymake-golangci-lint--get-option :tests)
     (list "--tests=false"))
   
   ;; Source directory
   (list source-dir)))

;; Output parsing
(defconst flymake-golangci-lint--output-regex
  (rx bol
      (group (one-or-more (not ":")))      ; file
      ":" (group (one-or-more digit))      ; line
      ":" (group (one-or-more digit))      ; column
      ": " (group (zero-or-more (not "\n"))) ; message
      eol)
  "Regex for parsing golangci-lint output.")

(defun flymake-golangci-lint--file-matches-p (reported-file buffer-file)
  "Check if REPORTED-FILE matches BUFFER-FILE."
  (or (and (file-name-absolute-p reported-file)
           (string= (file-truename reported-file) buffer-file))
      (string= (expand-file-name reported-file (file-name-directory buffer-file)) buffer-file)
      (string-suffix-p (file-name-nondirectory buffer-file)
                       (file-name-nondirectory reported-file))))

(defun flymake-golangci-lint--parse-line (line buffer-file)
  "Parse a single LINE of golangci-lint output for BUFFER-FILE.
Returns plist with :line and :message, or nil if no match."
  (when (string-match flymake-golangci-lint--output-regex line)
    (let ((file (match-string 1 line))
          (line-no (string-to-number (match-string 2 line)))
          (message (match-string 4 line)))
      (when (flymake-golangci-lint--file-matches-p file buffer-file)
        (list :line line-no :message message)))))

(defun flymake-golangci-lint--make-single-diagnostic (source-buffer issue)
  "Create a flymake diagnostic from ISSUE for SOURCE-BUFFER."
  (with-current-buffer source-buffer
    (save-excursion
      (goto-char (point-min))
      (forward-line (1- (plist-get issue :line)))
      (let ((region-start (line-beginning-position))
            (region-end (line-end-position))
            (message (plist-get issue :message))
            (type (let ((msg (plist-get issue :message)))
                    (cond
                     ((string-match-p "^[wW]arn" msg) :warning)
                     ((string-match-p "^[iI]nfo" msg) :note)
                     (t :error)))))
        (flymake-make-diagnostic source-buffer region-start region-end type message)))))

(defun flymake-golangci-lint--make-diagnostics (source-buffer output)
  "Parse golangci-lint OUTPUT and return flymake diagnostics for SOURCE-BUFFER."
  (let ((buffer-file (with-current-buffer source-buffer
                       (file-truename (buffer-file-name))))
        (issues '()))
    ;; Parse output lines
    (dolist (line (split-string output "\n" t))
      (when-let ((issue (flymake-golangci-lint--parse-line line buffer-file)))
        (push issue issues)))
    ;; Create diagnostics
    (mapcar (lambda (issue)
              (flymake-golangci-lint--make-single-diagnostic source-buffer issue))
            (reverse issues))))

;; Process management
(defconst flymake-golangci-lint--process-name "flymake-golangci-lint"
  "Name for golangci-lint processes.")

(defconst flymake-golangci-lint--buffer-prefix " *flymake-golangci-lint*"
  "Prefix for golangci-lint process buffers.")

(defvar-local flymake-golangci-lint--process nil
  "The current golangci-lint process for this buffer.")

(defvar-local flymake-golangci-lint--report-fn nil
  "The current report function.")

(defvar-local flymake-golangci-lint--reported nil
  "Whether the report function has been called for current check.")

;; Backend helpers
(defun flymake-golangci-lint--validate-environment (source-file)
  "Validate environment for SOURCE-FILE.  Return error message or nil."
  (cond
   ((not source-file) "Buffer has no associated file")
   ((not (file-name-directory source-file)) "Cannot determine source directory")
   (t nil)))

(defun flymake-golangci-lint--cancel-process ()
  "Cancel current golangci-lint process if running."
  (when (and flymake-golangci-lint--process
             (process-live-p flymake-golangci-lint--process))
    (kill-process flymake-golangci-lint--process)
    (setq flymake-golangci-lint--process nil
          flymake-golangci-lint--report-fn nil
          flymake-golangci-lint--reported nil)))

(defun flymake-golangci-lint--cleanup-process (proc buffer)
  "Clean up process PROC and its BUFFER."
  (when (buffer-live-p buffer)
    (kill-buffer buffer))
  (when (and (buffer-live-p (current-buffer))
             (eq proc flymake-golangci-lint--process))
    (setq flymake-golangci-lint--process nil
          flymake-golangci-lint--report-fn nil
          flymake-golangci-lint--reported nil)))

(defun flymake-golangci-lint--safe-report (source-buffer report-fn &rest args)
  "Safely call REPORT-FN with ARGS in SOURCE-BUFFER context.
This function includes all necessary safety checks to prevent state errors."
  (when (and (buffer-live-p source-buffer)
             (with-current-buffer source-buffer
               (and flymake-mode
                    (memq #'flymake-golangci-lint-backend flymake-diagnostic-functions)
                    (not flymake-golangci-lint--reported))))
    (with-current-buffer source-buffer
      (setq flymake-golangci-lint--reported t)
      (condition-case err
          (apply report-fn args)
        (error
         (flymake-log :warning "Error in flymake-golangci-lint report: %s" err))))))

(defun flymake-golangci-lint--handle-process-result (proc source-buffer report-fn)
  "Handle process PROC result for SOURCE-BUFFER, calling REPORT-FN."
  (let ((exit-status (process-exit-status proc))
        (output (with-current-buffer (process-buffer proc)
                  (buffer-string))))
    (cond
     ;; Success: no issues
     ((eq exit-status 0)
      (flymake-golangci-lint--safe-report source-buffer report-fn nil))
     ;; Issues found
     ((eq exit-status 1)
      (flymake-golangci-lint--safe-report source-buffer report-fn
                                         (flymake-golangci-lint--make-diagnostics source-buffer output)))
     ;; Error
     (t
      (flymake-golangci-lint--safe-report source-buffer report-fn nil :panic
                                         :explanation (format "golangci-lint failed (exit %d): %s"
                                                             exit-status output))))))

(defun flymake-golangci-lint--create-process-sentinel (source-buffer report-fn)
  "Create process sentinel for SOURCE-BUFFER that will call REPORT-FN."
  (lambda (proc _event)
    (when (memq (process-status proc) '(exit signal))
      (unwind-protect
          (when (and (buffer-live-p source-buffer)
                     (with-current-buffer source-buffer
                       (and (eq proc flymake-golangci-lint--process)
                            flymake-mode
                            (memq #'flymake-golangci-lint-backend flymake-diagnostic-functions))))
            (flymake-golangci-lint--handle-process-result proc source-buffer report-fn))
        (flymake-golangci-lint--cleanup-process proc (process-buffer proc))))))

(defun flymake-golangci-lint--start-process (source-buffer source-file report-fn)
  "Start golangci-lint process for SOURCE-BUFFER and SOURCE-FILE.
Call REPORT-FN when process completes."
  (flymake-golangci-lint--cancel-process)
  (let* ((source-dir (file-name-directory source-file))
         (command (flymake-golangci-lint-command source-dir)))
    (setq flymake-golangci-lint--process
          (make-process
           :name flymake-golangci-lint--process-name
           :buffer (generate-new-buffer flymake-golangci-lint--buffer-prefix)
           :command command
           :noquery t
           :connection-type 'pipe
           :sentinel (flymake-golangci-lint--create-process-sentinel
                     source-buffer report-fn))
          flymake-golangci-lint--report-fn report-fn
          flymake-golangci-lint--reported nil)
    flymake-golangci-lint--process))

(defun flymake-golangci-lint-backend (report-fn &rest _args)
  "Flymake backend function.  Call REPORT-FN with diagnostics."
  (let* ((source-buffer (current-buffer))
         (source-file (buffer-file-name)))
    
    ;; Initialize state for this check
    (setq flymake-golangci-lint--report-fn report-fn
          flymake-golangci-lint--reported nil)
    
    (if-let ((error-msg (flymake-golangci-lint--validate-environment source-file)))
        (flymake-golangci-lint--safe-report source-buffer report-fn nil :panic :explanation error-msg)
      (flymake-golangci-lint--start-process source-buffer source-file report-fn))))

;;;###autoload
(defun flymake-golangci-lint-run ()
  "Run golangci-lint on the current buffer manually and show results in flymake."
  (interactive)
  (if (executable-find flymake-golangci-lint-executable)
      (progn
        (unless flymake-mode (flymake-mode 1))
        ;; Add hook if not present
        (unless (memq #'flymake-golangci-lint-backend flymake-diagnostic-functions)
          (add-hook 'flymake-diagnostic-functions #'flymake-golangci-lint-backend nil t))
        (flymake-start)
        (message "Running golangci-lint on current buffer..."))
    (user-error "Golangci-lint executable not found in PATH")))

;;;###autoload
(defun flymake-golangci-lint-load ()
  "Configure flymake to check Go syntax with golangci-lint."
  (interactive)
  (when (and flymake-golangci-lint-auto-enable
             (executable-find flymake-golangci-lint-executable))
    ;; Add hook if not present
    (unless (memq #'flymake-golangci-lint-backend flymake-diagnostic-functions)
      (add-hook 'flymake-diagnostic-functions #'flymake-golangci-lint-backend nil t))
    (flymake-mode 1)))

(provide 'flymake-golangci-lint)
;;; flymake-golangci-lint.el ends here