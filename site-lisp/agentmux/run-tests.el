;;; run-tests.el --- Batch test runner for agentmux -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides minimal `emamux' stubs so the test suite is independent of any
;; installed emamux version, then loads `code-ref-core', `agentmux', and
;; `agentmux-test', and runs all tests.
;; Usage: emacs --batch -l run-tests.el

;;; Code:

(require 'cl-lib)
(require 'ert)

;;; Emamux stubs
;;
;; Names and signatures mirror the real emamux package just enough for
;; `(require 'emamux)' and `advice-add' to succeed at agentmux load time.
;; Tests override individual functions via `cl-letf' to supply behaviour.

(defvar emamux:session nil)
(defvar emamux:window nil)
(defvar emamux:pane nil)
(defvar emamux:last-command nil)
(defvar emamux:completing-read-type 'normal)

(defun emamux:tmux-run-command (_output &rest _args)
  "Stub: tests must override via `cl-letf'."
  (error "emamux:tmux-run-command not mocked"))

(defun emamux:set-parameters ()
  "Stub: tests bind defvars directly via `agentmux-test-with-target'."
  nil)

(defun emamux:set-parameters-p ()
  (and emamux:session emamux:window emamux:pane))

(cl-defun emamux:target-session (&optional
                                  (session emamux:session)
                                  (window emamux:window)
                                  (pane emamux:pane))
  (format "%s:%s.%s" session window pane))

(defun emamux:get-pane ()
  (error "emamux:get-pane stub not overridden"))

(defun emamux:get-window ()
  (error "emamux:get-window stub not overridden"))

(defun emamux:read-parameter-pane ()
  (error "emamux:read-parameter-pane stub not overridden"))

(defun emamux:read-parameter-window ()
  (error "emamux:read-parameter-window stub not overridden"))

(defun emamux:completing-read (_prompt &rest _args)
  (error "emamux:completing-read stub not overridden"))

(provide 'emamux)

;;; Load sources and tests

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "."))
  (add-to-list 'load-path (expand-file-name "../code-ref"))
  (load (expand-file-name "agentmux") nil t)
  (load (expand-file-name "agentmux-test") nil t))

;;; Run tests
;;
;; Skipped when the caller only needs the stub environment (e.g.
;; `make compile' loads this file to satisfy `(require 'emamux)' but then
;; hands off to `batch-byte-compile').

(unless (bound-and-true-p agentmux-test-load-only)
  (let ((stats (ert-run-tests-batch "^agentmux-test-")))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

;;; run-tests.el ends here
