;;; run-tests.el --- Batch test runner for tmpl -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>

;;; Commentary:
;; Loads `tmpl-core', `tmpl' and `tmpl-test' and runs the unit tests,
;; with a `web-mode' stub standing in for a mode that colors templates
;; itself.
;; Meant for `emacs -Q --batch': no package is initialized, so dumb-jump
;; and evil are absent, as the soft-dependency tests require.
;; Usage: emacs -Q --batch -l run-tests.el

;;; Code:

(require 'ert)

(define-derived-mode web-mode text-mode "Web-Stub"
  "Stub: stands in for the real `web-mode', absent under `emacs -Q'.")

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (add-to-list 'load-path default-directory)
  (load (expand-file-name "tmpl-core") nil t)
  (load (expand-file-name "tmpl") nil t)
  (load (expand-file-name "tmpl-test") nil t))

(let ((stats (ert-run-tests-batch "^tmpl-test-")))
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))

;;; run-tests.el ends here
