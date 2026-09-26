;;; tmpl-integration-test.el --- tmpl under the real init -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;;
;;; Commentary:
;; Integration tests run by run-integration.el after this repository's
;; init files are loaded: major modes come from `lisp/init-prog.el', and
;; definition lookups go through xref to dumb-jump with the init's rg
;; settings.  Fixtures and helpers come from tmpl-test.el.
;;
;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'xref)
(require 'tmpl-test)

(defvar tmpl-integration--warnings)
(defvar tmpl-integration--dumb-jump-before-xref)
(defvar dumb-jump-find-rules)
(defvar dumb-jump-language-file-exts)

(defmacro tmpl-integration--visiting (rel &rest body)
  "Visit fixture REL as a user would, fontified, and run BODY there."
  (declare (indent 1))
  `(let ((buf (find-file-noselect (tmpl-test--path ,rel))))
     (unwind-protect
         (with-current-buffer buf
           (let ((noninteractive nil))
             (font-lock-mode 1))
           (font-lock-ensure)
           ,@body)
       (kill-buffer buf))))

(ert-deftest tmpl-integration-test-init-clean ()
  "The init files load without any warning."
  (should (equal tmpl-integration--warnings nil))
  (should tmpl-global-mode))

;;; US-0050: host modes

(defconst tmpl-integration--host-modes
  '(("gomod/a.go.tmpl" "a.go" go-ts-mode)
    ("gomod/q.sql.tmpl" "q.sql" sql-mode)
    ("gomod/c.yaml.tmpl" "c.yaml" yaml-ts-mode)
    ("gomod/p.html.tmpl" "p.html" html-ts-mode)
    ("gomod/page.tpl" "page.html" html-ts-mode)
    ("gomod/mail.body.tmpl" "mail" text-mode))
  "Template files, the name whose mode they should share, and that mode.")

(ert-deftest tmpl-integration-test-host-mode ()
  "AC-0050-0010: template suffixes open in the inner file's major mode."
  (pcase-dolist (`(,rel ,name ,mode) tmpl-integration--host-modes)
    (let ((opened (tmpl-integration--visiting rel major-mode)))
      (should (equal (list rel opened) (list rel (tmpl-test--mode-for rel name))))
      (should (equal (list rel opened) (list rel mode))))))

;;; US-0030 and US-0040 under the real init

(ert-deftest tmpl-integration-test-detection ()
  "AC-0030-0010: the detection table holds with the init's major modes.
Also covers AC-0030-0030 and AC-0040-0010 through the same table."
  (pcase-dolist (`(,rel ,engine ,_) tmpl-test--detection)
    (should (equal (list rel (tmpl-integration--visiting rel
                               (cons tmpl-engine (and tmpl-mode t))))
                   (list rel (cons engine (and engine t)))))))

(ert-deftest tmpl-integration-test-template-modes ()
  "AC-0060-0030: `web-mode' files stay without tmpl under the real init.
`page.djhtml' sits where `.dir-locals.el' sets an engine with the nil
key; `a.erb.tmpl' is under go.mod and opens in `web-mode' once stripped."
  (dolist (rel '("dirlocals/page.djhtml" "gomod/a.erb.tmpl"))
    (should (equal (list rel (tmpl-integration--visiting rel
                               (list major-mode tmpl-engine tmpl-mode)))
                   (list rel '(web-mode nil nil))))))

(defconst tmpl-integration--faces
  '(("dbt/models/orders.sql" "config" 0 tmpl-builtin-face)
    ("dbt/models/orders.sql" "select" 0 font-lock-keyword-face)
    ("dbt/models/orders.sql" "'paid'" 0 font-lock-string-face)
    ("dbt/models/orders.sql" "\"a/*\"" 0 tmpl-string-face)
    ("django/shop/jinja2/shop/page.html" "price" 0 tmpl-function-call-face)
    ("django/shop/templates/shop/item.html" "title" 0 tmpl-filter-face)
    ("django/shop/templates/shop/item.html" "class" 0 font-lock-function-name-face)
    ("gomod/a.go.tmpl" ".Name" 0 tmpl-property-face)
    ("gomod/a.go.tmpl" "func" 0 font-lock-keyword-face)
    ("gomod/a.go.tmpl" "$f" 0 tmpl-variable-face)
    ("gomod/c.yaml.tmpl" "printf" 0 tmpl-builtin-face)
    ("gomod/c.yaml.tmpl" "true" 0 font-lock-constant-face))
  "Hand-written faces under the real init: (FILE NEEDLE NTH FACE).")

(ert-deftest tmpl-integration-test-faces ()
  "AC-0010-0010: hand-written faces hold with the init's modes and theme.
Also covers AC-0010-0020 and AC-0020-0010 through the same table."
  (pcase-dolist (`(,rel ,needle ,nth ,face) tmpl-integration--faces)
    (tmpl-integration--visiting rel
      (goto-char (point-min))
      (dotimes (_ (1+ nth))
        (search-forward needle))
      (should (equal (list rel needle (tmpl-test--span-face (match-beginning 0) (match-end 0)))
                     (list rel needle face))))))

;;; US-0080: definitions

(defun tmpl-integration--definitions (rel needle nth)
  "Return \"FILE:LINE\" of each definition xref finds for NEEDLE in REL.
Point goes to the start of the NTH occurrence of NEEDLE."
  (tmpl-integration--visiting rel
    (goto-char (point-min))
    (dotimes (_ (1+ nth))
      (search-forward needle))
    (goto-char (match-beginning 0))
    (let* ((backend (xref-find-backend))
           (id (xref-backend-identifier-at-point backend)))
      (mapcar (lambda (item)
                (let ((loc (xref-item-location item)))
                  (format "%s:%d"
                          (file-relative-name (xref-location-group loc)
                                              (tmpl-test--path ""))
                          (xref-location-line loc))))
              (xref-backend-definitions backend id)))))

(defconst tmpl-integration--jumps
  '(("django/shop/jinja2/shop/page.html" "price(1" 0
     "django/shop/jinja2/shop/macros.html:1" jinja2 macro)
    ("django/shop/jinja2/shop/page.html" "price(2" 0
     "django/shop/jinja2/shop/macros.html:1" jinja2 macro)
    ("django/templates/child.html" "content" 0
     "django/templates/base.html:4" django block)
    ("django/shop/jinja2/shop/list.html" "main" 0
     "django/shop/jinja2/shop/layout.html:1" jinja2 block)
    ("django/shop/jinja2/shop/page.html" "total" 0
     "django/shop/jinja2/shop/macros.html:2" jinja2 set)
    ("django/shop/jinja2/shop/page.html" "banner" 0
     "django/shop/jinja2/shop/macros.html:3" jinja2 set)
    ("django/shop/jinja2/shop/page.html" "ui." 0
     "django/shop/jinja2/shop/page.html:1" jinja2 import)
    ("django/shop/jinja2/shop/page.html" "badge(" 0
     "django/shop/jinja2/shop/page.html:2" jinja2 from)
    ("dbt/models/orders.sql" "cents_to_usd" 0 "dbt/macros/money.sql:1" dbt macro)
    ("dbt/models/orders.sql" "tabled" 0 "dbt/macros/tabled.sql:1" dbt materialization)
    ("dbt/models/orders.sql" "orders_snap" 0
     "dbt/snapshots/orders_snap.sql:1" dbt snapshot)
    ("dbt/models/orders.sql" "is_positive" 0
     "dbt/tests/generic/is_positive.sql:1" dbt test)
    ("gomod/mail.body.tmpl" "header" 0 "gomod/layout.tmpl:1" go-template define)
    ("gomod/mail.body.tmpl" "footer" 0 "gomod/layout.tmpl:2" go-template block)
    ;; Line 3 assigns with `$who =', which is no definition.
    ("gomod/mail.body.tmpl" "who" 2 "gomod/mail.body.tmpl:2" go-template variable))
  "Definition cases: (FILE NEEDLE NTH DEFINITION ENGINE WRITING).")

(ert-deftest tmpl-integration-test-jump ()
  "AC-0080-0010: each definition writing is found, in this or another file."
  (pcase-dolist (`(,rel ,needle ,nth ,target . ,_) tmpl-integration--jumps)
    (should (equal (list rel needle (tmpl-integration--definitions rel needle nth))
                   (list rel needle (list target))))))

(defvar dumb-jump-fallback-search)

(ert-deftest tmpl-integration-test-no-definition ()
  "AC-0080-0020: tmpl's rules find nothing for a name without a definition.
Each name occurs in another fixture file too, so dumb-jump's own
fallback, a plain search for the name outside tmpl's scope, does find
it.  Binding `dumb-jump-fallback-search' to nil for the lookup leaves
the rule search alone, whose results must be empty; the user's global
setting is untouched."
  (dolist (case '(("django/shop/jinja2/shop/page.html" "nowhere_defined" 0)
                  ("gomod/mail.body.tmpl" "missing" 0)))
    (should (apply #'tmpl-integration--definitions case))
    (let ((dumb-jump-fallback-search nil))
      (should (equal (list case (apply #'tmpl-integration--definitions case))
                     (list case nil))))))

(ert-deftest tmpl-integration-test-dumb-jump-regression ()
  "AC-0080-0030: files without templates jump as without tmpl's rules."
  (let ((cases '(("regress/app.js" "greet" 1) ("regress/app.js" "total" 1)
                 ("regress/schema.sql" "order_total" 1)
                 ("regress/schema.sql" "orders" 1))))
    (dolist (case cases)
      (let ((with (apply #'tmpl-integration--definitions case))
            (without (let ((dumb-jump-find-rules
                            (seq-difference dumb-jump-find-rules (tmpl-dumb-jump-rules)))
                           (dumb-jump-language-file-exts
                            (seq-remove (lambda (e) (equal (plist-get e :language)
                                                           tmpl-dumb-jump-language))
                                        dumb-jump-language-file-exts)))
                       (apply #'tmpl-integration--definitions case))))
        (should (equal (list case with) (list case without)))
        (should with)))))

(ert-deftest tmpl-integration-test-dumb-jump-soft ()
  "AC-0080-0040: templates open without loading dumb-jump."
  (should (eq tmpl-integration--dumb-jump-before-xref nil)))

(defconst tmpl-integration--writings
  '((django block) (jinja2 macro) (jinja2 block) (jinja2 set) (jinja2 import)
    (jinja2 from) (dbt macro) (dbt test) (dbt snapshot) (dbt materialization)
    (go-template define) (go-template block) (go-template variable))
  "Engine and definition writing pairs the SPEC names.")

(ert-deftest tmpl-integration-test-writing-matrix ()
  "AC-0080-0010: every engine and writing pair has a jump case."
  (dolist (pair tmpl-integration--writings)
    (should (equal (list pair t)
                   (list pair (and (seq-some (lambda (c) (equal (nthcdr 4 c) pair))
                                             tmpl-integration--jumps)
                                   t))))))

(provide 'tmpl-integration-test)
;;; tmpl-integration-test.el ends here
