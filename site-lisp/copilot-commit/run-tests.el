;;; run-tests.el --- Batch test runner for copilot-commit -*- lexical-binding: t; -*-

;;; Commentary:
;; Provides minimal copilot stubs and runs all copilot-commit tests.
;; Usage: emacs --batch -l run-tests.el

;;; Code:

(require 'cl-lib)
(require 'ert)

;;; Copilot stubs

;; Stub copilot--dbind as a simple destructuring macro
(defmacro copilot--dbind (keys expr &rest body)
  "Stub for copilot--dbind: destructure KEYS from EXPR, execute BODY."
  (declare (indent 2))
  (let ((val (gensym "val")))
    `(let* ((,val ,expr)
            ,@(mapcar (lambda (k)
                        `(,k (plist-get ,val ,(intern (concat ":" (symbol-name k))))))
                      keys))
       ,@body)))

(defun copilot--connection-alivep ()
  "Stub: always return t."
  t)

(defun cc-test--request (_method _params &rest _args)
  "Stub: no-op synchronous request."
  nil)

(defmacro copilot--request (method &optional params &rest args)
  "Stub for `copilot--request'."
  `(cc-test--request ,method ,params ,@args))

(defun cc-test--async-request (_method _params &rest _args)
  "Stub: no-op asynchronous request."
  nil)

(cl-defmacro copilot--async-request (method params &rest args
                                            &key &allow-other-keys)
  "Stub for `copilot--async-request'."
  `(cc-test--async-request ,method ,params ,@args))

(defun copilot-on-notification (_method _handler)
  "Stub: no-op."
  nil)

(defun copilot-on-request (_method _handler)
  "Stub: no-op."
  nil)

(defvar copilot--ignore-response (lambda (&rest _) nil)
  "Stub: ignore response function.")

(defvar copilot--request-handlers (make-hash-table :test 'equal)
  "Stub: empty request handlers table.")

;; Provide copilot so (require 'copilot) succeeds
(provide 'copilot)

;;; Load sources and tests

(let ((default-directory (file-name-directory
                          (or load-file-name buffer-file-name))))
  (load (expand-file-name "copilot-commit-core.el") nil t)
  (load (expand-file-name "copilot-commit.el") nil t)
  (load (expand-file-name "copilot-commit-test.el") nil t))

;;; Run tests

(unless (bound-and-true-p copilot-commit-test-load-only)
  (let ((stats (ert-run-tests-batch "^cc-test-")))
    (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1))))

;;; run-tests.el ends here
