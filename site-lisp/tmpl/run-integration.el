;;; run-integration.el --- Run tmpl integration tests under the real init -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>

;;; Commentary:
;; Loads this repository's early-init.el and init.el in batch, recording
;; every warning raised while they load, then runs tmpl-integration-test.
;; Usage, from the repository root:
;;   emacs --batch -l site-lisp/tmpl/run-integration.el

;;; Code:

(require 'ert)

(defvar tmpl-integration--warnings nil
  "Warnings displayed while the init files load, most recent first.")

(defvar tmpl-integration--dumb-jump-before-xref 'unset
  "Whether dumb-jump was loaded after visiting templates, before any xref.")

(defun tmpl-integration--record-warning (type message &rest _)
  "Record the warning of TYPE with MESSAGE."
  (push (format "%s: %s" type message) tmpl-integration--warnings))

(let ((tests-dir (file-name-directory (or load-file-name buffer-file-name))))
  (advice-add 'display-warning :before #'tmpl-integration--record-warning)
  (load (expand-file-name "early-init.el" user-emacs-directory) nil t)
  (load (expand-file-name "init.el" user-emacs-directory) nil t)
  (advice-remove 'display-warning #'tmpl-integration--record-warning)
  (load (expand-file-name "tmpl-test" tests-dir) nil t)
  (dolist (rel '("gomod/layout.tmpl" "dbt/models/orders.sql"
                 "django/shop/jinja2/shop/page.html"))
    (kill-buffer (find-file-noselect (tmpl-test--path rel))))
  (setq tmpl-integration--dumb-jump-before-xref (featurep 'dumb-jump))
  (load (expand-file-name "tmpl-integration-test" tests-dir) nil t))

(let ((stats (ert-run-tests-batch "^tmpl-integration-test-")))
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))

;;; run-integration.el ends here
