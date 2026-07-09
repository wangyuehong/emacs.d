;;; run-tests.el --- Batch test runner for md-tui-preview -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides a minimal `markdown-mode' stub so the test suite is
;; independent of any installed markdown-mode version, then loads
;; `md-tui-preview-core', `md-tui-preview', and `md-tui-preview-test',
;; and runs all tests.
;; Usage: emacs --batch -l run-tests.el

;;; Code:

(require 'cl-lib)
(require 'ert)

;;; markdown-mode stub
;;
;; Just enough of the real markdown-mode package for md-tui-preview's
;; `(markdown-mode)' call and `markdown-mode-command-map' keybinding to
;; work in tests, without depending on the real package being installed.

(defvar markdown-mode-command-map (make-sparse-keymap)
  "Stub: minimal command map so keybinding wiring can be tested.")

(define-derived-mode markdown-mode text-mode "Markdown-Stub"
  "Stub: stands in for the real `markdown-mode' during tests.")

(define-derived-mode gfm-mode markdown-mode "GFM-Stub"
  "Stub: stands in for the real `gfm-mode' during tests.")

(provide 'markdown-mode)

;;; Load sources and tests

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (load (expand-file-name "md-tui-preview-core") nil t)
  (load (expand-file-name "md-tui-preview") nil t)
  (load (expand-file-name "md-tui-preview-test") nil t))

;;; Run tests

(let ((stats (ert-run-tests-batch "^md-tui-preview-test-")))
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))

;;; run-tests.el ends here
