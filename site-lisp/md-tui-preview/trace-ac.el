;;; trace-ac.el --- Map SPEC acceptance criteria to the tests covering them -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>

;;; Commentary:
;; Prints one line per acceptance criterion in SPEC.md with the tests that
;; name it, deriving the mapping from the two files rather than keeping a
;; hand-written copy that could drift.  An AC with no test prints an em
;; dash: coverage is reported, not enforced.  What does fail the run is an
;; AC number that appears in a test but not in SPEC.md -- that is a typo or
;; a leftover from a renumbering, and it makes the mapping lie.
;; Usage: emacs --batch -l trace-ac.el
;;
;;; Code:

(require 'subr-x)

(defconst trace-ac-id-regexp "AC-[0-9]+-[0-9]+"
  "Regexp matching an acceptance criterion identifier.")

(defun trace-ac-spec-ids (file)
  "Return the acceptance criteria FILE defines, in document order.
An AC is defined by its own heading; a mention anywhere else in the prose
is a cross-reference, not a definition."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (ids)
      (while (re-search-forward (concat "^### \\(" trace-ac-id-regexp "\\)") nil t)
        (push (match-string 1) ids))
      (nreverse ids))))

(defun trace-ac-test-ids (file)
  "Return an alist of (AC-ID . TEST-NAMES) from the tests in FILE.
A test claims an AC by naming it in its docstring."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let (claims)
      (while (re-search-forward "^(ert-deftest \\([^ ]+\\) ()" nil t)
        (let ((name (match-string 1))
              (limit (save-excursion
                       (or (and (re-search-forward "^(ert-deftest " nil t)
                                (match-beginning 0))
                           (point-max)))))
          (while (re-search-forward trace-ac-id-regexp limit t)
            (let* ((id (match-string 0))
                   (entry (assoc id claims)))
              (cond ((null entry) (push (list id name) claims))
                    ;; A test naming the same AC twice is still one test.
                    ((not (member name (cdr entry)))
                     (setcdr entry (cons name (cdr entry)))))))
          (goto-char limit)))
      claims)))

(defun trace-ac-report (spec-file test-file)
  "Print the AC-to-test mapping between SPEC-FILE and TEST-FILE.
Returns non-nil when a test names an AC that SPEC-FILE does not define."
  (let* ((ids (trace-ac-spec-ids spec-file))
         (claims (trace-ac-test-ids test-file))
         (uncovered (seq-remove (lambda (id) (assoc id claims)) ids))
         (dangling (seq-remove (lambda (claim) (member (car claim) ids)) claims)))
    (dolist (id ids)
      (let ((names (reverse (cdr (assoc id claims)))))
        (princ (format "%s  %s\n" id (if names (string-join names ", ") "—")))))
    (princ (format "\n%d/%d 条 AC 有测试覆盖；未覆盖：%s\n"
                   (- (length ids) (length uncovered)) (length ids)
                   (if uncovered (string-join uncovered " ") "无")))
    (when dangling
      (princ (format "SPEC 未定义的 AC 编号被测试引用：%s\n"
                     (mapconcat (lambda (claim)
                                  (format "%s (%s)" (car claim) (cadr claim)))
                                dangling " "))))
    dangling))

(let ((default-directory (file-name-directory (or load-file-name buffer-file-name))))
  (kill-emacs (if (trace-ac-report "SPEC.md" "md-tui-preview-test.el") 1 0)))

;;; trace-ac.el ends here
