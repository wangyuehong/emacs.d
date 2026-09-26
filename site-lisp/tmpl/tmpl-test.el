;;; tmpl-test.el --- Tests for tmpl -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;;
;;; Commentary:
;; Unit tests for tmpl, run under `emacs -Q' by run-tests.el.  The
;; expectations do not come from tmpl itself: faces are written by hand,
;; host faces come from a copy of the text with each template region
;; blanked out, and template regions are recomputed by a plain regexp.
;; Fixtures live in test/fixtures and are all synthetic.
;;
;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'subr-x)
(require 'sql)
(require 'tmpl-core)
(require 'tmpl)

(defconst tmpl-test--root
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the tmpl package.")

(defun tmpl-test--path (rel)
  "Return the absolute path of fixture REL."
  (expand-file-name (concat "test/fixtures/" rel) tmpl-test--root))

(defun tmpl-test--read (rel)
  "Return the contents of fixture REL."
  (with-temp-buffer
    (insert-file-contents (tmpl-test--path rel))
    (buffer-string)))

(defun tmpl-test--face (category)
  "Return the face of CATEGORY."
  (intern (format "tmpl-%s-face" category)))

(defconst tmpl-test--categories
  '(delimiter tag keyword operator filter test function-call builtin
    property variable string number constant comment unregistered)
  "Every coloring category of the SPEC.")

(defconst tmpl-test--faces
  (cons 'tmpl-region-face (mapcar #'tmpl-test--face tmpl-test--categories))
  "Every face tmpl sets.")

;;; Buffers

(defmacro tmpl-test--with-text (text mode engine &rest body)
  "Run BODY in a buffer holding TEXT in MODE, fully fontified.
When ENGINE is non-nil `tmpl-mode' is on with that engine.  Font-lock
runs through jit-lock as in an interactive session."
  (declare (indent 3))
  `(let ((buf (generate-new-buffer "tmpl-test")))
     (unwind-protect
         (with-current-buffer buf
           (insert ,text)
           (goto-char (point-min))
           (funcall ,mode)
           (let ((noninteractive nil))
             (font-lock-mode 1))
           (when ,engine
             (setq-local tmpl-engine ,engine)
             (tmpl-mode 1))
           (font-lock-ensure)
           ,@body)
       (kill-buffer buf))))

(defun tmpl-test--faces-of (text mode engine)
  "Return TEXT fontified in MODE with ENGINE, as a propertized string."
  (tmpl-test--with-text text mode engine
    (buffer-string)))

(defun tmpl-test--face-at (text mode engine needle &optional nth)
  "Return the face over the NTH occurrence of NEEDLE in TEXT.
TEXT is fontified in MODE with ENGINE.  Return the symbol `mixed' when
the characters of NEEDLE differ in face."
  (tmpl-test--with-text text mode engine
    (dotimes (_ (1+ (or nth 0)))
      (unless (search-forward needle nil t)
        (error "Needle %S not found in %S" needle text)))
    (let ((faces (delete-dups
                  (mapcar (lambda (pos) (get-text-property pos 'face))
                          (number-sequence (match-beginning 0) (1- (match-end 0)))))))
      (if (cdr faces) 'mixed (car faces)))))

(defun tmpl-test--span-face (beg end)
  "Return the single face over BEG..END, or the symbol `mixed'."
  (let ((faces (delete-dups (mapcar (lambda (pos) (get-text-property pos 'face))
                                    (number-sequence beg (1- end))))))
    (if (cdr faces) 'mixed (car faces))))

;;; Region oracle and placeholder copies

(defun tmpl-test--oracle-regions (text family)
  "Return the template regions of TEXT for FAMILY as (BEG . END) positions.
A plain non-greedy regexp; fixtures used with it have no raw blocks."
  (let ((re (pcase family
              ('jinja "{{\\(?:.\\|\n\\)*?}}\\|{%\\(?:.\\|\n\\)*?%}\\|{#\\(?:.\\|\n\\)*?#}")
              ('go "{{\\(?:.\\|\n\\)*?}}")))
        (start 0)
        regions)
    (while (string-match re text start)
      (push (cons (1+ (match-beginning 0)) (1+ (match-end 0))) regions)
      (setq start (match-end 0)))
    (nreverse regions)))

(defun tmpl-test--placeholder (text regions)
  "Return TEXT with REGIONS blanked out, keeping newlines."
  (let ((chars (string-to-vector text)))
    (dolist (region regions)
      (cl-loop for pos from (car region) below (cdr region)
               unless (eq (aref chars (1- pos)) ?\n)
               do (aset chars (1- pos) ?\s)))
    (concat chars)))

(defun tmpl-test--inside-p (pos regions)
  "Return non-nil when POS lies in one of REGIONS."
  (seq-some (lambda (r) (and (>= pos (car r)) (< pos (cdr r)))) regions))

(defun tmpl-test--family (engine)
  "Return the family symbol of ENGINE."
  (plist-get (tmpl-engine-spec engine) :family-name))

(defun tmpl-test--ppss-state (pos)
  "Return the syntax state at POS that host coloring depends on.
Drops the start of the last complete sexp and the obsolete element."
  (let ((s (syntax-ppss pos)))
    (list (nth 0 s) (nth 1 s) (nth 3 s) (nth 4 s) (nth 5 s) (nth 7 s)
          (nth 8 s) (nth 9 s))))

(cl-defstruct (tmpl-test--pair (:constructor tmpl-test--pair-create))
  "A fixture fontified with tmpl and its placeholder copy without tmpl."
  text regions tmpl-faces host-faces tmpl-ppss host-ppss scanned)

(defun tmpl-test--pair (rel mode engine)
  "Return the `tmpl-test--pair' of fixture REL in MODE with ENGINE."
  (let* ((text (tmpl-test--read rel))
         (regions (tmpl-test--oracle-regions text (tmpl-test--family engine)))
         (ends (mapcar #'cdr regions))
         scanned tmpl-ppss host-ppss)
    (let ((tmpl-faces
           (tmpl-test--with-text text mode engine
             (setq scanned (tmpl-regions)
                   tmpl-ppss (mapcar #'tmpl-test--ppss-state ends))
             (buffer-string)))
          (host-faces
           (tmpl-test--with-text (tmpl-test--placeholder text regions) mode nil
             (setq host-ppss (mapcar #'tmpl-test--ppss-state ends))
             (buffer-string))))
      (tmpl-test--pair-create :text text :regions regions :scanned scanned
                              :tmpl-faces tmpl-faces :host-faces host-faces
                              :tmpl-ppss tmpl-ppss :host-ppss host-ppss))))

(defun tmpl-test--outside-mismatches (pair)
  "Return positions outside template regions whose faces differ in PAIR."
  (let ((regions (tmpl-test--pair-regions pair))
        (a (tmpl-test--pair-tmpl-faces pair))
        (b (tmpl-test--pair-host-faces pair))
        bad)
    (dotimes (i (length a))
      (unless (or (tmpl-test--inside-p (1+ i) regions)
                  (equal (get-text-property i 'face a) (get-text-property i 'face b)))
        (push (1+ i) bad)))
    (nreverse bad)))

(defun tmpl-test--host-face (pair pos)
  "Return the face at POS of the placeholder copy in PAIR."
  (get-text-property (1- pos) 'face (tmpl-test--pair-host-faces pair)))

(defun tmpl-test--tmpl-face (pair pos)
  "Return the face at POS of the tmpl buffer in PAIR."
  (get-text-property (1- pos) 'face (tmpl-test--pair-tmpl-faces pair)))

(defconst tmpl-test--literal-faces
  '(font-lock-comment-face font-lock-comment-delimiter-face font-lock-string-face
    font-lock-doc-face)
  "Host faces of comments and strings.")

(defun tmpl-test--find-outside (pair regexp &optional literal)
  "Return the (BEG . END) of group 1 of the first REGEXP match in host code.
Matches inside template regions are skipped, and so are those in host
comments and strings unless LITERAL is non-nil.  PAIR gives the text,
regions and host faces."
  (let ((text (tmpl-test--pair-text pair))
        (regions (tmpl-test--pair-regions pair))
        (start 0)
        found)
    (while (and (not found) (string-match regexp text start))
      (let ((beg (1+ (match-beginning 1))) (end (1+ (match-end 1))))
        (if (or (tmpl-test--inside-p beg regions)
                (tmpl-test--inside-p (1- end) regions)
                (and (not literal)
                     (memq (tmpl-test--host-face pair beg) tmpl-test--literal-faces)))
            (setq start (1+ (match-beginning 0)))
          (setq found (cons beg end)))))
    found))

(defun tmpl-test--word-re (word)
  "Return a regexp matching WORD as a whole word in group 1."
  (concat "\\(?:^\\|[^[:alnum:]_]\\)\\(" (regexp-quote word) "\\)\\(?:[^[:alnum:]_]\\|$\\)"))

;;; Host cases

(defconst tmpl-test--host-cases
  '(("dbt/models/host-sql.sql" sql-mode dbt)
    ("dbt/models/orders.sql" sql-mode dbt)
    ("dbt/models/host-yaml.yml" yaml-ts-mode dbt)
    ("dbt/models/schema.yml" yaml-ts-mode dbt)
    ("django/templates/host.html" html-ts-mode django)
    ("django/shop/templates/shop/item.html" html-ts-mode django)
    ("django/shop/jinja2/shop/page.html" html-ts-mode jinja2)
    ("gomod/a.go.tmpl" go-ts-mode go-template)
    ("gomod/q.sql.tmpl" sql-mode go-template)
    ("gomod/c.yaml.tmpl" yaml-ts-mode go-template)
    ("gomod/p.html.tmpl" html-ts-mode go-template)
    ("gomod/page.tpl" html-ts-mode go-template)
    ("gomod/mail.body.tmpl" text-mode go-template))
  "Fixtures checked for host invariance: (FILE HOST-MODE ENGINE).")

(defvar tmpl-test--pairs (make-hash-table :test #'equal)
  "Cache of `tmpl-test--pair' per host case.")

(defun tmpl-test--case-pair (case)
  "Return the cached `tmpl-test--pair' of host CASE."
  (or (gethash case tmpl-test--pairs)
      (puthash case (apply #'tmpl-test--pair case) tmpl-test--pairs)))

;;; US-0010: coloring

(defconst tmpl-test--jinja-examples
  '(("{{ x }}" "{{" delimiter) ("{{ x }}" "}}" delimiter)
    ("{%- if x -%}" "{%-" delimiter) ("{%- if x -%}" "-%}" delimiter)
    ("{# c #}" "{#" delimiter) ("{# c #}" "#}" delimiter)
    ("{% for x in y %}" "for" tag) ("{% endfor %}" "endfor" tag)
    ("{{ x | upper }}" "upper" filter)
    ("{{ d | date:\"Y\" }}" "date" filter (django))
    ("{{ x is defined }}" "defined" test (jinja2 dbt))
    ("{{ x is not defined }}" "defined" test (jinja2 dbt))
    ("{% my_tag a %}" "my_tag" unregistered)
    ("{% endfro %}" "endfro" unregistered)
    ("{{ x | shout }}" "shout" unregistered)
    ("{{ x is odd_ish }}" "odd_ish" unregistered (jinja2 dbt))
    ;; Django has no tests: `is' compares identity with a plain operand.
    ("{% if a is b %}" "is" keyword (django))
    ("{% if a is b %}" "b" variable (django))
    ("{% if a is not None %}" "not" keyword (django))
    ("{% if a is not None %}" "None" constant (django))
    ("{% if a is True %}" "True" constant (django))
    ("{{ x is defined }}" "defined" variable (django))
    ("{{ d | date:\"Y\" }}" "date" unregistered (jinja2 dbt))
    ("{{ f(1) }}" "f" function-call)
    ("{{ shop.name }}" "name" property)
    ;; Plain names are variables; the categories above still come first.
    ("{{ shop.name }}" "shop" variable)
    ("{% for x in xs %}" "x" variable) ("{% for x in xs %}" "xs" variable)
    ("{{ None }}" "None" constant) ("{{ True }}" "True" constant)
    ("{{ forloop.counter }}" "forloop" builtin (django))
    ("{{ loop.index }}" "loop" builtin (jinja2 dbt))
    ("{{ ref('m') }}" "ref" builtin (dbt))
    ("{{ ref('m') }}" "ref" function-call (django jinja2))
    ("{{ a == b }}" "==" operator) ("{{ a ~ b }}" "~" operator)
    ("{{ a | b }}" "|" operator)
    ("{{ 'a' }}" "'a'" string) ("{{ \"b\" }}" "\"b\"" string)
    ("{{ 3 }}" "3" number) ("{{ 1.5 }}" "1.5" number)
    ("{# a note #}" " a note " comment)
    ("{% comment %} a {{ z }} note {% endcomment %}" " a {{ z }} note " comment))
  "Jinja family examples of AC-0010-0010: (TEXT NEEDLE CATEGORY [ENGINES]).
Without ENGINES an example holds for every jinja family engine.")

(ert-deftest tmpl-test-jinja-structure ()
  "AC-0010-0010: jinja family writings get their category face."
  (dolist (engine '(jinja2 django dbt))
    (pcase-dolist (`(,text ,needle ,category ,engines) tmpl-test--jinja-examples)
      (when (or (null engines) (memq engine engines))
        (should (equal (list engine text needle
                             (tmpl-test--face-at text #'text-mode engine needle))
                       (list engine text needle (tmpl-test--face category))))))))

(defconst tmpl-test--go-examples
  '(("{{ x }}" "{{" delimiter) ("{{ x }}" "}}" delimiter)
    ("{{- x -}}" "{{-" delimiter) ("{{- x -}}" "-}}" delimiter)
    ("{{ .Name }}" ".Name" property) ("{{ . }}" "." property)
    ("{{ $x }}" "$x" variable) ("{{ $ }}" "$" variable)
    ("{{ .A | f }}" "|" operator) ("{{ $x := 1 }}" ":=" operator)
    ("{{ $x = 1 }}" "=" operator)
    ("{{ \"a\" }}" "\"a\"" string) ("{{ `raw` }}" "`raw`" string)
    ("{{ 3 }}" "3" number) ("{{ 1.5 }}" "1.5" number)
    ("{{/* a note */}}" "/* a note */" comment))
  "Go family examples: (TEXT NEEDLE CATEGORY).")

(ert-deftest tmpl-test-go-structure ()
  "AC-0010-0020: go-template writings get their category face."
  (pcase-dolist (`(,text ,needle ,category) tmpl-test--go-examples)
    (should (equal (list text needle
                         (tmpl-test--face-at text #'text-mode 'go-template needle))
                   (list text needle (tmpl-test--face category))))))

(defun tmpl-test--list-mismatches (rel engine items pattern category)
  "Return ITEMS whose occurrence in fixture REL lacks the CATEGORY face.
PATTERN is a format string turning an item into a regexp whose group 1
is the item; ENGINE is the buffer's engine."
  (tmpl-test--with-text (tmpl-test--read rel) #'text-mode engine
    (seq-remove (lambda (item)
                  (goto-char (point-min))
                  (and (re-search-forward (format pattern (regexp-quote item)) nil t)
                       (eq (tmpl-test--span-face (match-beginning 1) (match-end 1))
                           (tmpl-test--face category))))
                items)))

(defconst tmpl-test--jinja2-loop-attributes
  '("index0" "depth0" "length" "depth" "index" "revindex0" "revindex" "first"
    "last" "previtem" "nextitem" "cycle" "changed")
  "Jinja 3.1.6 runtime.py LoopContext public members (13).")

(defconst tmpl-test--django-forloop-attributes
  '("parentloop" "length" "counter0" "counter" "revcounter" "revcounter0"
    "first" "last")
  "Django 6.0.8 defaulttags.py ForNode loop_dict keys (8).")

(defconst tmpl-test--django-if-operators
  '("or" "and" "not" "in" "not in" "is" "is not")
  "Word operators of Django 6.0.8 smartif.py OPERATORS.")

(defconst tmpl-test--list-cases
  '((jinja2 "django/shop/jinja2/keywords.html")
    (django "django/templates/keywords.html")
    (dbt "dbt/models/keywords.sql"))
  "Keyword fixtures of the jinja family engines: (ENGINE FILE).")

(ert-deftest tmpl-test-list-coverage-jinja ()
  "AC-0010-0030: every listed jinja family name gets its category face."
  (pcase-dolist (`(,engine ,rel) tmpl-test--list-cases)
    (let* ((spec (tmpl-engine-spec engine))
           (family (plist-get spec :family)))
      (dolist (check `((,(plist-get spec :tags) "{%% \\(%s\\) %%}" tag)
                       (,(plist-get spec :filters) "| \\(%s\\) }}" filter)
                       (,(plist-get spec :tests) "is \\(%s\\) }}" test)
                       (,(plist-get spec :builtins) "{{ \\(%s\\) }}" builtin)
                       (,(plist-get family :keywords) "{{ a \\(%s\\) b }}" keyword)
                       (,(plist-get family :constants) "{{ \\(%s\\) }}" constant)))
        (should (equal (list engine (nth 2 check) nil)
                       (list engine (nth 2 check)
                             (tmpl-test--list-mismatches rel engine (nth 0 check)
                                                         (nth 1 check) (nth 2 check)))))))))

(ert-deftest tmpl-test-list-coverage-special-variables ()
  "AC-0010-0030: `loop', `forloop' and `block' and their attributes."
  (should-not (tmpl-test--list-mismatches "django/shop/jinja2/keywords.html" 'jinja2
                                          tmpl-test--jinja2-loop-attributes
                                          "loop\\.\\(%s\\) }}" 'property))
  (should-not (tmpl-test--list-mismatches "django/shop/jinja2/keywords.html" 'jinja2
                                          '("loop") "{{ \\(%s\\)\\." 'builtin))
  (should-not (tmpl-test--list-mismatches "django/templates/keywords.html" 'django
                                          tmpl-test--django-forloop-attributes
                                          "forloop\\.\\(%s\\) }}" 'property))
  (should-not (tmpl-test--list-mismatches "django/templates/keywords.html" 'django
                                          '("forloop" "block") "{{ \\(%s\\)\\." 'builtin))
  (should-not (tmpl-test--list-mismatches "django/templates/keywords.html" 'django
                                          '("super") "block\\.\\(%s\\) }}" 'property)))

(ert-deftest tmpl-test-list-coverage-django-if ()
  "AC-0010-0030: Django `{% if %}' word operators are keywords."
  (tmpl-test--with-text (tmpl-test--read "django/templates/keywords.html") #'text-mode 'django
    (dolist (op tmpl-test--django-if-operators)
      (goto-char (point-min))
      (should (re-search-forward (format "{%% if a \\(%s\\) b %%}" op) nil t))
      (goto-char (match-beginning 1))
      (dolist (word (split-string op))
        (search-forward word)
        (should (equal (list op word (tmpl-test--span-face (match-beginning 0) (match-end 0)))
                       (list op word 'tmpl-keyword-face)))))))

(ert-deftest tmpl-test-list-coverage-go ()
  "AC-0010-0030: every go-template action, function and constant is colored."
  (let* ((spec (tmpl-engine-spec 'go-template))
         (family (plist-get spec :family))
         (rel "gomod/keywords.gotmpl"))
    (should-not (tmpl-test--list-mismatches rel 'go-template (plist-get spec :builtins)
                                            "{{ \\(%s\\) \\.X }}" 'builtin))
    (should-not (tmpl-test--list-mismatches rel 'go-template (plist-get family :constants)
                                            "{{ \\(%s\\) }}" 'constant))
    (should-not (tmpl-test--list-mismatches rel 'go-template (plist-get family :keywords)
                                            "{{-? ?\\(?:else \\)?\\(%s\\)\\_>" 'keyword))
    ;; `else if' and `else with' color both words.
    (should-not (tmpl-test--list-mismatches rel 'go-template '("if" "with")
                                            "{{ else \\(%s\\) " 'keyword))))

(ert-deftest tmpl-test-list-counts ()
  "AC-0010-0030: registry lists have the item counts of their sources.
Each count is that of the source file and pinned version named in the
comment above the list in `tmpl-core.el', as written in the comments
below; reading those files reproduces it."
  (let ((own (lambda (engine key) (length (plist-get (cdr (assq engine tmpl-engines)) key)))))
    ;; Jinja 3.1.6: FILTERS 54; TESTS 39 keys minus 6 symbol keys; the 6
    ;; DEFAULT_NAMESPACE names plus `loop'; 20 statement tags plus 14
    ;; middle and end tags.
    (should (= 54 (funcall own 'jinja2 :filters)))
    (should (= 33 (funcall own 'jinja2 :tests)))
    (should (= 7 (funcall own 'jinja2 :builtins)))
    (should (= 34 (funcall own 'jinja2 :tags)))
    ;; Django 6.0.8: 57 + 4 + 2 + 3 filters; 23 tags, `querystring', 3
    ;; loader tags and 3 + 10 + 1 + 3 + 1 library tags, plus 22 middle and
    ;; end tags.
    (should (= 66 (funcall own 'django :filters)))
    (should (= 67 (funcall own 'django :tags)))
    (should (= 2 (funcall own 'django :builtins)))
    ;; dbt builtins: 42 single-name pages + 34 `###' macros of
    ;; cross-database-macros.md + 2 names of on-run-end-context.md not
    ;; already counted (`database_schemas', `results') + 0 from the other
    ;; 4 context pages, whose names are all among the 42 = 78.  Block
    ;; tags: 5 and their ends (`call statement' is jinja2's `call' plus
    ;; the `statement' builtin).
    (should (= 78 (funcall own 'dbt :builtins)))
    (should (= 8 (funcall own 'dbt :tags)))
    ;; Go 1.27.1: builtins() 19; lex.go `key' 12 minus "." and "nil".
    (should (= 19 (funcall own 'go-template :builtins)))
    (should (= 10 (length (plist-get (cdr (assq 'go tmpl-families)) :keywords))))
    (should (= 3 (length (plist-get (cdr (assq 'go tmpl-families)) :constants))))
    ;; Jinja 3.1.6 parser.py: 7 keywords and 6 constant spellings.
    (should (= 7 (length (plist-get (cdr (assq 'jinja tmpl-families)) :keywords))))
    (should (= 6 (length (plist-get (cdr (assq 'jinja tmpl-families)) :constants))))))

(ert-deftest tmpl-test-builtin-per-engine ()
  "AC-0010-0040: `ref' is a builtin under dbt and a call under django."
  (should (eq (tmpl-test--face-at "{{ ref('m') }}" #'text-mode 'dbt "ref")
              'tmpl-builtin-face))
  (should (eq (tmpl-test--face-at "{{ ref('m') }}" #'text-mode 'django "ref")
              'tmpl-function-call-face))
  (should (eq (tmpl-test--face-at "{{ dbt.concat(['a']) }}" #'text-mode 'dbt "dbt.concat")
              'tmpl-builtin-face))
  (should (eq (tmpl-test--face-at "{{ x.dbt.concat }}" #'text-mode 'dbt "dbt")
              'tmpl-property-face))
  (should (eq (tmpl-test--face-at "{{ dbt.concat(['a']) }}" #'text-mode 'django "concat")
              'tmpl-property-face)))

(ert-deftest tmpl-test-raw-blocks ()
  "AC-0010-0050: raw and verbatim bodies carry no template face."
  (dolist (case '(("a {% raw %} {{ x }} {% if %} {% endraw %} b" jinja2 " {{ x }} {% if %} ")
                  ("a {%- raw -%}{{ x }}{%- endraw -%} b" jinja2 "{{ x }}")
                  ("a {% verbatim %} {{ x }} {% endverbatim %} b" django " {{ x }} ")
                  ("a {% verbatim v %} {{ x }} {% endverbatim %} {{ y }} {% endverbatim v %} b"
                   django " {{ x }} {% endverbatim %} {{ y }} ")))
    (pcase-let ((`(,text ,engine ,body) case))
      (tmpl-test--with-text text #'text-mode engine
        (search-forward body)
        (dolist (pos (number-sequence (match-beginning 0) (1- (match-end 0))))
          (should-not (memq (get-text-property pos 'face) tmpl-test--faces))))))
  ;; A named end tag closes its own block and colors as a tag.
  (tmpl-test--with-text "{% verbatim v %}x{% endverbatim v %} {{ y }}" #'text-mode 'django
    (search-forward "endverbatim")
    (should (eq (get-text-property (match-beginning 0) 'face) 'tmpl-tag-face))
    (search-forward "y")
    (should (eq (get-text-property (match-beginning 0) 'face) 'tmpl-variable-face)))
  (should (eq (tmpl-test--face-at "{% endblock content %}" #'text-mode 'jinja2 "endblock")
              'tmpl-tag-face)))

(defconst tmpl-test--edit-text
  "select 1\n{% set x = [\n  1,\n  'a' ] %}\nselect '{{ y\n  | upper }}' from t\n{# a\nb #}\n-- end\nselect 2\n"
  "Buffer text for the incremental editing test.")

(defconst tmpl-test--edits
  '((insert-inside . (lambda () (search-forward "1,") (insert " 2,")))
    (delete-inside . (lambda () (search-forward "'a'") (delete-char -3)))
    (split-close . (lambda () (search-forward "%}") (backward-char 1) (insert " ")))
    (join-close . (lambda () (search-forward "% }") (backward-char 1) (delete-char -1)))
    (split-open . (lambda () (search-forward "{{") (backward-char 1) (insert " ")))
    (join-open . (lambda () (search-forward "{ {") (backward-char 1) (delete-char -1)))
    (merge-lines . (lambda () (search-forward "{{ y") (delete-char 1)))
    (open-raw . (lambda () (insert "{% raw %}")))
    (close-raw . (lambda () (goto-char (point-max)) (insert "{% endraw %}\n")))
    (remove-raw . (lambda () (search-forward "{% raw %}") (delete-region (match-beginning 0) (match-end 0))))
    (unclosed . (lambda () (search-forward "select 2") (insert " {{ z")))
    (close-it . (lambda () (search-forward "{{ z") (insert " }}")))
    (quote-before . (lambda () (insert "'")))
    (unquote . (lambda () (delete-char 1))))
  "Scripted edits, each run from the start of the buffer.")

(defconst tmpl-test--host-syntax-edits '(quote-before unquote)
  "Edits that change host syntax outside any region.
For these Emacs itself relies on jit-lock's contextual refontification,
run after `jit-lock-context-time'; the test runs it too.  Template edits
are checked without it: their redisplay must be right at once.")

(ert-deftest tmpl-test-incremental ()
  "AC-0010-0060: faces after each edit equal a full refontification."
  (dolist (mode '(sql-mode yaml-ts-mode))
    (tmpl-test--with-text tmpl-test--edit-text mode 'dbt
      (dolist (edit tmpl-test--edits)
        (goto-char (point-min))
        (funcall (cdr edit))
        (when (memq (car edit) tmpl-test--host-syntax-edits)
          (jit-lock-context-fontify))
        (jit-lock-fontify-now (point-min) (point-max))
        (let ((fresh (tmpl-test--faces-of (buffer-string) mode 'dbt))
              (now (buffer-string)))
          (should (equal (list mode (car edit) nil)
                         (list mode (car edit)
                               (cl-loop for i below (length now)
                                        unless (equal (get-text-property i 'face now)
                                                      (get-text-property i 'face fresh))
                                        collect (1+ i))))))))))

;;; US-0020: the host

(ert-deftest tmpl-test-regions-oracle ()
  "AC-0020-0010: the scanned regions equal the regexp oracle's."
  (dolist (case tmpl-test--host-cases)
    (let ((pair (tmpl-test--case-pair case)))
      (should (equal (list (car case) (tmpl-test--pair-scanned pair))
                     (list (car case) (tmpl-test--pair-regions pair)))))))

(ert-deftest tmpl-test-host-invariance ()
  "AC-0020-0010: faces outside regions equal the placeholder copy's."
  (dolist (case tmpl-test--host-cases)
    (should (equal (list (car case) nil)
                   (list (car case)
                         (tmpl-test--outside-mismatches (tmpl-test--case-pair case)))))))

(defun tmpl-test--sql-groups ()
  "Return (FACE . WORDS) groups of the ANSI keywords, read from sql.el."
  (with-temp-buffer
    (insert-file-contents (concat (file-name-sans-extension (locate-library "sql")) ".el"))
    (re-search-forward "(setq sql-mode-ansi-font-lock-keywords")
    (goto-char (match-beginning 0))
    (mapcar (lambda (builder)
              (cons (cadr (nth 1 builder)) (seq-filter #'stringp (nthcdr 3 builder))))
            (cdr (nth 2 (read (current-buffer)))))))

(defun tmpl-test--host-list-mismatches (case words expected &optional pattern)
  "Return WORDS of host CASE not found in host code or wrongly colored.
Each occurrence must have the placeholder copy's face and, when EXPECTED
is non-nil, that face.  PATTERN, a format string whose group 1 is the
word, locates it; by default a word stands alone and anything else is
matched literally."
  (let ((pair (tmpl-test--case-pair case)))
    (seq-remove
     (lambda (word)
       (let ((span (tmpl-test--find-outside
                    pair (cond (pattern (format pattern (regexp-quote word)))
                               ((string-match-p "\\`[[:alnum:]_]+\\'" word)
                                (tmpl-test--word-re word))
                               (t (concat "\\(" (regexp-quote word) "\\)")))
                    (memq expected tmpl-test--literal-faces))))
         (and span
              (cl-loop for pos from (car span) below (cdr span)
                       always (and (equal (tmpl-test--tmpl-face pair pos)
                                          (tmpl-test--host-face pair pos))
                                   (or (null expected)
                                       (equal (tmpl-test--tmpl-face pair pos) expected)))))))
     words)))

(ert-deftest tmpl-test-host-sql-list ()
  "AC-0020-0010: every ANSI keyword, type and function keeps the SQL face."
  (let* ((groups (tmpl-test--sql-groups))
         (seen nil))
    (should (= 436 (apply #'+ (mapcar (lambda (g) (length (cdr g))) groups))))
    (pcase-dolist (`(,face . ,words) groups)
      ;; A word in an earlier group keeps that group's face.
      (let ((own (seq-remove (lambda (w) (member w seen)) words)))
        (setq seen (append seen words))
        (should (equal (list face nil)
                       (list face (tmpl-test--host-list-mismatches
                                   '("dbt/models/host-sql.sql" sql-mode dbt) own face))))))))

(defun tmpl-test--html-list (name)
  "Return the words of test/lists/html-NAME.txt.
The lists were taken from the WHATWG HTML Living Standard indices
(https://html.spec.whatwg.org/multipage/indices.html, 25 September
2026): \"List of elements\", and the rows of \"List of attributes\" and
\"List of event handler content attributes\" that apply to all HTML
elements."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (format "test/lists/html-%s.txt" name)
                                            tmpl-test--root))
    (split-string (buffer-string) "\n" t)))

(defconst tmpl-test--html-elements-count 113
  "Elements in WHATWG HTML \"List of elements\" (25 September 2026).")

(ert-deftest tmpl-test-host-html-list ()
  "AC-0020-0010: every element, global attribute, doctype and comment of HTML."
  (let ((case '("django/templates/host.html" html-ts-mode django))
        (elements (tmpl-test--html-list "elements"))
        (attributes (append (tmpl-test--html-list "global-attributes")
                            (tmpl-test--html-list "event-handlers"))))
    (should (= tmpl-test--html-elements-count (length elements)))
    (should (= (+ 31 71) (length attributes)))
    (should-not (tmpl-test--host-list-mismatches
                 case (mapcar (lambda (e) (concat "<" e)) elements) nil))
    (should-not (tmpl-test--host-list-mismatches case elements 'font-lock-function-name-face
                                                 "<\\(%s\\)[ >]"))
    (should-not (tmpl-test--host-list-mismatches
                 case (mapcar (lambda (a) (concat a "=")) attributes) nil))
    (should-not (tmpl-test--host-list-mismatches case attributes 'font-lock-variable-name-face
                                                 " \\(%s\\)="))
    (should-not (tmpl-test--host-list-mismatches case '("DOCTYPE") 'font-lock-keyword-face))
    (should-not (tmpl-test--host-list-mismatches case '("<!--") 'font-lock-comment-face))))

(ert-deftest tmpl-test-host-yaml-list ()
  "AC-0020-0010: YAML booleans, nulls, directives, anchors, tags, markers."
  (let ((case '("dbt/models/host-yaml.yml" yaml-ts-mode dbt)))
    (should-not (tmpl-test--host-list-mismatches
                 case '("true" "True" "TRUE" "false" "False" "FALSE" "null" "Null" "NULL" "~")
                 'font-lock-constant-face))
    (should-not (tmpl-test--host-list-mismatches case '("# YAML") 'font-lock-comment-face))
    (should-not (tmpl-test--host-list-mismatches
                 case '("%YAML" "&base" "*base" "!!str" "---" "..." "version" "flags")
                 nil))))

(defconst tmpl-test--go-predeclared
  '("bool" "byte" "complex64" "complex128" "error" "float32" "float64" "int"
    "int8" "int16" "int32" "int64" "rune" "string" "uint" "uint8" "uint16"
    "uint32" "uint64" "uintptr" "any" "comparable" "true" "false" "iota" "nil")
  "Predeclared types and constants of the Go specification.")

(ert-deftest tmpl-test-host-go-list ()
  "AC-0020-0010: every Go keyword, builtin, operator and predeclared name."
  (require 'go-ts-mode)
  (let ((case '("gomod/a.go.tmpl" go-ts-mode go-template)))
    (should-not (tmpl-test--host-list-mismatches case (symbol-value 'go-ts-mode--keywords)
                                                 'font-lock-keyword-face))
    (should-not (tmpl-test--host-list-mismatches
                 case (symbol-value 'go-ts-mode--builtin-functions) nil))
    (should-not (tmpl-test--host-list-mismatches
                 case (symbol-value 'go-ts-mode--operators) nil))
    (should-not (tmpl-test--host-list-mismatches case tmpl-test--go-predeclared nil))))

(ert-deftest tmpl-test-syntax-invariance ()
  "AC-0020-0020: the syntax state after each region equals the placeholder's."
  (dolist (case tmpl-test--host-cases)
    (let ((pair (tmpl-test--case-pair case)))
      (should (equal (list (car case) (tmpl-test--pair-tmpl-ppss pair))
                     (list (car case) (tmpl-test--pair-host-ppss pair)))))))

(ert-deftest tmpl-test-syntax-examples ()
  "AC-0020-0020: quotes and comment starters in a region stay inside it."
  (dolist (text '("{# don't -- x #} select 1" "{{ \"a/*\" }} select 1"
                  "'{{ 'x' }}' select 1" "-- {{ a\n}} select 1"))
    (tmpl-test--with-text text #'sql-mode 'dbt
      (search-forward "select")
      (should (equal (list text nil) (list text (nth 8 (syntax-ppss (point))))))
      (should (equal (list text 'font-lock-keyword-face)
                     (list text (get-text-property (1- (point)) 'face)))))))

(ert-deftest tmpl-test-no-host-face-inside ()
  "AC-0020-0030: regions carry only template faces, never the host's."
  (dolist (case tmpl-test--host-cases)
    (let ((pair (tmpl-test--case-pair case)))
      (should (equal (list (car case) nil)
                     (list (car case)
                           (cl-loop for region in (tmpl-test--pair-regions pair)
                                    append (cl-loop for pos from (car region) below (cdr region)
                                                    unless (memq (tmpl-test--tmpl-face pair pos)
                                                                 tmpl-test--faces)
                                                    collect pos))))))))

(ert-deftest tmpl-test-homographs ()
  "AC-0020-0030: the same word gets a template face inside, the host's outside."
  (dolist (case '(("dbt/models/host-sql.sql" sql-mode dbt)
                  ("django/templates/host.html" html-ts-mode django)
                  ("dbt/models/host-yaml.yml" yaml-ts-mode dbt)))
    (let ((pair (tmpl-test--case-pair case)))
      (dolist (word '("and" "or" "not" "in" "is" "if" "else" "end" "select" "range"))
        (let ((span (tmpl-test--find-outside pair (tmpl-test--word-re word))))
          (should (equal (list (car case) word t)
                         (list (car case) word
                               (and span (equal (tmpl-test--tmpl-face pair (car span))
                                                (tmpl-test--host-face pair (car span))))))))))
    (should (equal (tmpl-test--host-list-mismatches
                    '("dbt/models/host-sql.sql" sql-mode dbt)
                    '("and" "or" "not" "in" "is" "end" "select") 'font-lock-keyword-face)
                   nil)))
  ;; (TEXT WORD CATEGORY INSIDE OUTSIDE): occurrence indexes of WORD in
  ;; TEXT; the placeholder copy has the outside one first.
  (dolist (case '(("{{ config(bind=false) }} false" "false" constant 0 1)
                  ("select {{ a and b }} and" "and" keyword 0 1)
                  ("select {{ x | select }} select" "select" filter 1 2)))
    (pcase-let ((`(,text ,word ,category ,inside ,outside) case))
      (should (eq (tmpl-test--face-at text #'sql-mode 'dbt word inside)
                  (tmpl-test--face category)))
      (should (eq (tmpl-test--face-at text #'sql-mode 'dbt word outside)
                  (tmpl-test--face-at (tmpl-test--placeholder
                                       text (tmpl-test--oracle-regions text 'jinja))
                                      #'sql-mode nil word (1- outside))))
      (unless (equal word "false")
        (should (eq (tmpl-test--face-at text #'sql-mode 'dbt word outside)
                    'font-lock-keyword-face)))))
  ;; HTML: a region inside an attribute value carries no host string face.
  (let* ((case '("django/shop/templates/shop/item.html" html-ts-mode django))
         (pair (tmpl-test--case-pair case))
         (text (tmpl-test--pair-text pair))
         (beg (1+ (string-search "{% url" text)))
         (region (assoc beg (tmpl-test--pair-regions pair))))
    (should region)
    (should (eq (tmpl-test--tmpl-face pair (+ beg 3)) 'tmpl-tag-face))
    (should (eq (tmpl-test--host-face pair (1- beg)) 'font-lock-string-face))
    (should-not (cl-loop for pos from beg below (cdr region)
                         thereis (eq (tmpl-test--tmpl-face pair pos)
                                     'font-lock-string-face)))))

;;; US-0030 and US-0040: detection

(defconst tmpl-test--detection
  '(("django/templates/base.html" django django-project)
    ("django/templates/child.html" django django-project)
    ("django/shop/templates/shop/item.html" django django-app)
    ("django/shop/jinja2/shop/page.html" jinja2 jinja2-app)
    ("django/nested/templates/app/jinja2/x.html" jinja2 nearest)
    ("django/shop/static/shop/plain.html" nil dir-mismatch)
    ("django/manage.py" nil no-rule)
    ("no-settings/templates/index.html" nil content-mismatch)
    ("dbt/models/orders.sql" dbt dbt)
    ("dbt/models/schema.yml" dbt dbt)
    ("dbt/macros/money.sql" dbt dbt)
    ("dbt/snapshots/orders_snap.sql" dbt dbt)
    ("dbt/tests/generic/is_positive.sql" dbt dbt)
    ("dbt/.github/workflows/ci.yml" nil excluded-dir)
    ("gomod/a.go.tmpl" go-template go)
    ("gomod/q.sql.tmpl" go-template go)
    ("gomod/c.yaml.tmpl" go-template go)
    ("gomod/p.html.tmpl" go-template go)
    ("gomod/page.tpl" go-template go)
    ("gomod/mail.body.tmpl" go-template go)
    ("gomod/keywords.gotmpl" go-template go)
    ("monorepo/web/templates/home.html" django django-project)
    ("monorepo/analytics/models/visits.sql" dbt dbt)
    ("monorepo/.github/workflows/ci.yml" nil no-marker)
    ("monorepo/docs/guide.md" nil no-rule)
    ("mixed/warehouse/models/daily.sql" dbt dbt)
    ("mixed/warehouse/tools/seed.sql" dbt dbt)
    ("mixed/warehouse/tools/seed.sql.tmpl" go-template nearest)
    ("mixed/codegen/model.tmpl" go-template go)
    ("plain/static.html" nil no-marker)
    ("plain/query.sql" nil no-marker)
    ("dirlocals/page.sql" jinja2 explicit)
    ("dirlocals/filelocal.html" django explicit)
    ("dirlocals/off/q.sql" nil explicit-nil))
  "Detection table: (FILE ENGINE BRANCH).")

(defmacro tmpl-test--visiting (rel &rest body)
  "Visit fixture REL with `tmpl-global-mode' on and run BODY there."
  (declare (indent 1))
  `(let ((was tmpl-global-mode)
         (buf nil))
     (unwind-protect
         (progn
           (tmpl-global-mode 1)
           (let ((enable-local-variables :safe))
             (setq buf (find-file-noselect (tmpl-test--path ,rel))))
           (with-current-buffer buf
             ;; As `global-font-lock-mode' does, after local variables.
             (let ((noninteractive nil))
               (font-lock-mode 1))
             ,@body))
       (when buf (kill-buffer buf))
       (unless was (tmpl-global-mode -1)))))

(defun tmpl-test--visit-engine (rel)
  "Return (ENGINE . MODE-ON) after visiting fixture REL."
  (tmpl-test--visiting rel
    (cons tmpl-engine (and tmpl-mode t))))

(ert-deftest tmpl-test-detect-standard ()
  "AC-0030-0010: standard projects get their engine."
  (pcase-dolist (`(,rel ,engine ,_) (seq-filter (lambda (r) (nth 1 r)) tmpl-test--detection))
    (should (equal (list rel (tmpl-test--visit-engine rel))
                   (list rel (cons engine t))))))

(ert-deftest tmpl-test-detect-nearest ()
  "AC-0030-0020: the nearest marker decides among matching rules."
  (should (eq (tmpl-detect (tmpl-test--path "django/nested/templates/app/jinja2/x.html"))
              'jinja2))
  (let ((tmpl-rules (append tmpl-rules
                            '((:engine go-template :extensions ("sql") :marker "go.mod")))))
    (should (eq (tmpl-detect (tmpl-test--path "mixed/warehouse/tools/seed.sql")) 'go-template))
    (should (eq (tmpl-detect (tmpl-test--path "mixed/warehouse/models/daily.sql")) 'dbt))))

(ert-deftest tmpl-test-detect-negative ()
  "AC-0030-0030: files outside the rules get no engine and stay untouched."
  (pcase-dolist (`(,rel ,_ ,branch) (seq-remove (lambda (r) (nth 1 r)) tmpl-test--detection))
    (unless (eq branch 'explicit-nil)
      (should (equal (list rel (tmpl-test--visit-engine rel)) (list rel (cons nil nil)))))))

(defconst tmpl-test--fs-operations
  '(file-exists-p file-regular-p file-directory-p file-readable-p
    file-attributes file-truename file-symlink-p directory-files
    insert-file-contents)
  "File name operations that touch the file system.")

(defvar tmpl-test--fs-calls 0
  "Count of `tmpl-test--fs-operations' seen by `tmpl-test--fs-handler'.")

(defun tmpl-test--fs-handler (operation &rest args)
  "File name handler counting OPERATION when it touches the file system.
ARGS are passed on to the normal handling."
  (when (memq operation tmpl-test--fs-operations)
    (cl-incf tmpl-test--fs-calls))
  (let ((inhibit-file-name-handlers
         (cons #'tmpl-test--fs-handler
               (and (eq inhibit-file-name-operation operation)
                    inhibit-file-name-handlers)))
        (inhibit-file-name-operation operation))
    (apply operation args)))

(defun tmpl-test--count-fs (fn)
  "Return how many file system operations calling FN performs."
  (let ((tmpl-test--fs-calls 0)
        (file-name-handler-alist (cons '("" . tmpl-test--fs-handler)
                                       file-name-handler-alist)))
    (funcall fn)
    tmpl-test--fs-calls))

(ert-deftest tmpl-test-no-lookup-for-other-files ()
  "AC-0030-0040: names no rule applies to cause no file system lookup."
  (dolist (file (list (tmpl-test--path "monorepo/docs/guide.md")
                      (tmpl-test--path "django/manage.py")
                      "/nonexistent/dir/notes.txt"))
    (should (equal (list file 0)
                   (list file (tmpl-test--count-fs (lambda () (tmpl-detect file)))))))
  ;; The counter does see the lookups of a file a rule applies to.
  (should (< 0 (tmpl-test--count-fs
              (lambda () (tmpl-detect (tmpl-test--path "dbt/models/orders.sql")))))))

(ert-deftest tmpl-test-explicit ()
  "AC-0040-0010: local variables choose the engine over the markers."
  (should (equal (tmpl-test--visit-engine "dirlocals/page.sql") '(jinja2 . t)))
  (should (equal (tmpl-test--visit-engine "dirlocals/filelocal.html") '(django . t)))
  (tmpl-test--visiting "dirlocals/page.sql"
    (font-lock-ensure)
    (search-forward "ref")
    (should (eq (get-text-property (match-beginning 0) 'face) 'tmpl-function-call-face)))
  (tmpl-test--visiting "dirlocals/filelocal.html"
    (font-lock-ensure)
    (search-forward "forloop")
    (should (eq (get-text-property (match-beginning 0) 'face) 'tmpl-builtin-face))))

(ert-deftest tmpl-test-explicit-nil ()
  "AC-0040-0020: an explicit nil disables tmpl despite a matching marker."
  (should (eq (tmpl-detect (tmpl-test--path "dirlocals/off/q.sql")) 'dbt))
  (should (equal (tmpl-test--visit-engine "dirlocals/off/q.sql") '(nil . nil))))

(ert-deftest tmpl-test-explicit-unregistered ()
  "AC-0040-0030: an unregistered engine is a `user-error' naming it."
  (with-temp-buffer
    (setq-local tmpl-engine 'no-such-engine)
    (let ((err (should-error (tmpl-resolve) :type 'user-error)))
      (should (string-match-p "no-such-engine" (error-message-string err)))))
  (let ((logged (with-current-buffer (messages-buffer) (point-max))))
    (should (equal (tmpl-test--visit-engine "dirlocals/bad/q.sql") '(no-such-engine . nil)))
    (with-current-buffer (messages-buffer)
      (should (string-match-p "no-such-engine"
                              (buffer-substring logged (point-max)))))))

;;; US-0050: host modes of template files

(defun tmpl-test--mode-for (rel name)
  "Return the major mode Emacs picks for NAME holding the text of fixture REL.
NAME is taken in the fixture's directory."
  (with-temp-buffer
    (insert-file-contents (tmpl-test--path rel))
    (let ((buffer-file-name (expand-file-name name (file-name-directory
                                                    (tmpl-test--path rel)))))
      (set-auto-mode)
      major-mode)))

(defconst tmpl-test--suffix-cases
  '(("gomod/a.go.tmpl" "a.go") ("gomod/q.sql.tmpl" "q.sql")
    ("gomod/c.yaml.tmpl" "c.yaml") ("gomod/p.html.tmpl" "p.html")
    ("gomod/page.tpl" "page") ("gomod/mail.body.tmpl" "mail"))
  "Template files and the name whose mode each should share.")

(defun tmpl-test--suffix-modes ()
  "Return the major mode of each file of `tmpl-test--suffix-cases'."
  (mapcar (lambda (case)
            (tmpl-test--mode-for (car case) (file-name-nondirectory (car case))))
          tmpl-test--suffix-cases))

(defmacro tmpl-test--with-global-mode-off (&rest body)
  "Run BODY with `tmpl-global-mode' off, restoring its state afterwards."
  (declare (indent 0))
  `(let ((was tmpl-global-mode))
     (unwind-protect
         (progn (tmpl-global-mode -1) ,@body)
       (tmpl-global-mode (if was 1 -1)))))

(ert-deftest tmpl-test-suffix-host-mode ()
  "AC-0050-0010: with the global mode on, templates open in the inner mode.
The mode equals that of the name without the suffix, or for `page.tpl'
and `mail.body.tmpl' that of a file with the same text and no extension."
  (tmpl-test--with-global-mode-off
    (tmpl-global-mode 1)
    (pcase-dolist (`(,rel ,name) tmpl-test--suffix-cases)
      (should (equal (list rel (tmpl-test--mode-for rel (file-name-nondirectory rel)))
                     (list rel (tmpl-test--mode-for rel name)))))
    (should (eq (tmpl-test--mode-for "gomod/q.sql.tmpl" "q.sql.tmpl") 'sql-mode))
    (should (eq (tmpl-test--mode-for "gomod/page.tpl" "page.tpl") 'mhtml-mode))))

(ert-deftest tmpl-test-suffix-restored ()
  "AC-0050-0020: turning the global mode off restores how templates open."
  (tmpl-test--with-global-mode-off
    (let ((alist (copy-sequence auto-mode-alist))
          (before (tmpl-test--suffix-modes)))
      (tmpl-global-mode 1)
      (should-not (equal (tmpl-test--suffix-modes) before))
      (tmpl-global-mode -1)
      (should (equal auto-mode-alist alist))
      (should (equal (tmpl-test--suffix-modes) before)))))

;;; US-0060: other buffers

(defun tmpl-test--buffer-signature ()
  "Return what tmpl could change in the current buffer."
  (font-lock-ensure)
  (syntax-ppss (point-max))
  (list (buffer-string)
        (let (props)
          (dotimes (i (1- (point-max)))
            (push (get-text-property (1+ i) 'syntax-table) props))
          props)
        syntax-propertize-function
        font-lock-keywords
        (local-variable-p 'parse-sexp-lookup-properties)
        after-change-functions before-change-functions
        font-lock-extend-region-functions
        syntax-propertize-extend-region-functions))

(ert-deftest tmpl-test-nil-buffers-untouched ()
  "AC-0060-0010: buffers without an engine equal those without tmpl."
  (dolist (rel '("plain/static.html" "plain/query.sql" "dirlocals/off/q.sql"))
    (let ((with (tmpl-test--visiting rel (tmpl-test--buffer-signature)))
          (without (let ((buf (find-file-noselect (tmpl-test--path rel))))
                     (unwind-protect
                         (with-current-buffer buf
                           (let ((noninteractive nil))
                             (font-lock-mode 1))
                           (tmpl-test--buffer-signature))
                       (kill-buffer buf)))))
      (should (equal (list rel with) (list rel without))))))

(defun tmpl-test--template-mode-visit (rel global)
  "Return (MODE ENGINE MODE-ON SIGNATURE) after visiting fixture REL.
`.djhtml' and `.erb' open in `web-mode' and `.tmpl' is stripped even
with GLOBAL nil, so both visits reach the same major mode; GLOBAL
non-nil turns `tmpl-global-mode' on."
  (tmpl-test--with-global-mode-off
    (when global
      (tmpl-global-mode 1))
    (let ((auto-mode-alist (append '(("\\.djhtml\\'" . web-mode) ("\\.erb\\'" . web-mode)
                                     ("\\.tmpl\\'" nil t))
                                   auto-mode-alist))
          (enable-local-variables :safe)
          (buf nil))
      (unwind-protect
          (progn
            (setq buf (find-file-noselect (tmpl-test--path rel)))
            (with-current-buffer buf
              (let ((noninteractive nil))
                (font-lock-mode 1))
              (list major-mode tmpl-engine tmpl-mode (tmpl-test--buffer-signature))))
        (when buf (kill-buffer buf))))))

(ert-deftest tmpl-test-template-modes ()
  "AC-0060-0030: modes that color templates themselves stay untouched.
The `web-mode' stub of run-tests.el stands in for such a mode; one file
gets its engine from `.dir-locals.el', the other from the go.mod rule.
Emptying `tmpl-template-modes' shows both would otherwise turn tmpl on."
  (dolist (rel '("dirlocals/page.djhtml" "gomod/a.erb.tmpl"))
    (pcase-let ((`(,mode ,engine ,on ,signature) (tmpl-test--template-mode-visit rel t))
                (`(,_ ,_ ,_ ,without) (tmpl-test--template-mode-visit rel nil)))
      (should (equal (list rel mode engine on) (list rel 'web-mode nil nil)))
      (should (equal (list rel signature) (list rel without))))
    (let ((tmpl-template-modes nil))
      (should (nth 2 (tmpl-test--template-mode-visit rel t))))))

(ert-deftest tmpl-test-non-file-buffers ()
  "AC-0060-0010: buffers without a file ignore directory-local engines.
Dired and similar buffers apply directory-local variables too; neither a
registered nor an unregistered engine there turns on or signals."
  (let ((was tmpl-global-mode))
    (unwind-protect
        (progn
          (tmpl-global-mode 1)
          (dolist (dir '("dirlocals/" "dirlocals/bad/"))
            (with-temp-buffer
              (setq default-directory (tmpl-test--path dir))
              (let ((enable-local-variables :safe))
                (hack-dir-local-variables-non-file-buffer))
              (should tmpl-engine)
              (should-not tmpl-mode))))
      (unless was (tmpl-global-mode -1)))))

(ert-deftest tmpl-test-delimiter-stands-out ()
  "AC-0010-0070: delimiters look unlike host tags, functions and keywords.
Batch Emacs has no color display, where those faces all fall back to
bold, so this checks what yields the difference on color displays: the
delimiter's color comes from none of them and its weight is its own."
  (let ((chain nil) (face 'tmpl-delimiter-face))
    (while (and face (symbolp face) (not (eq face 'unspecified)))
      (push face chain)
      (setq face (face-attribute face :inherit)))
    (dolist (host '(font-lock-function-name-face font-lock-function-call-face
                    font-lock-keyword-face font-lock-preprocessor-face))
      (should-not (memq host chain))))
  (should (eq 'bold (face-attribute 'tmpl-delimiter-face :weight))))

(defun tmpl-test--face-chain (face)
  "Return FACE and the faces it inherits from, nearest first."
  (let (chain)
    (while (and face (symbolp face) (not (eq face 'unspecified)))
      (push face chain)
      (setq face (face-attribute face :inherit)))
    (nreverse chain)))

(ert-deftest tmpl-test-unregistered-stands-out ()
  "AC-0010-0070: unregistered names look unlike tags, filters and tests.
As in `tmpl-test-delimiter-stands-out', this checks what yields the
difference on color displays: the color comes from none of those faces
or their parents, and nothing looks like a warning or an error."
  (let ((chain (tmpl-test--face-chain 'tmpl-unregistered-face)))
    (dolist (face '(tmpl-tag-face tmpl-filter-face tmpl-test-face))
      (dolist (source (tmpl-test--face-chain face))
        (should-not (memq source chain))))
    (dolist (alarm '(font-lock-warning-face warning error))
      (should-not (memq alarm chain)))
    (dolist (face chain)
      (let ((underline (face-attribute face :underline)))
        (should-not (and (consp underline) (eq (plist-get underline :style) 'wave)))))))

(ert-deftest tmpl-test-extra-names ()
  "AC-0010-0080: names added by local variables color as registered.
`.dir-locals.el' adds a tag pair, a filter and a test; a file-local
value replaces it for its file."
  (tmpl-test--visiting "extra/page.html"
    (font-lock-ensure)
    (dolist (case '(("panel" tag) ("endpanel" tag) ("shout" filter) ("odd_ish" test)
                    ("whisper" unregistered)))
      (goto-char (point-min))
      (search-forward (car case))
      (should (equal (list (car case) (tmpl-test--span-face (match-beginning 0) (match-end 0)))
                     (list (car case) (tmpl-test--face (cadr case)))))))
  (tmpl-test--visiting "extra/filelocal.html"
    (font-lock-ensure)
    (dolist (case '(("whisper" filter) ("shout" unregistered)))
      (goto-char (point-min))
      (search-forward (concat "| " (car case)))
      (should (equal (list (car case) (tmpl-test--span-face (- (match-end 0) (length (car case)))
                                                            (match-end 0)))
                     (list (car case) (tmpl-test--face (cadr case))))))))

(ert-deftest tmpl-test-extra-names-safety ()
  "AC-0010-0080: only (CATEGORY . STRINGS) alists are safe to add names.
An ill-formed value is not applied from `.dir-locals.el', and set by
hand it stops `tmpl-mode' with a `user-error'."
  (should (tmpl-extra-names-p '((tags "a" "enda") (filters) (tests "t"))))
  (dolist (bad '("tags" ((tags . "a")) ((colors "a")) ((tags 1)) ((tags "a" . "b")) (tags)))
    (should-not (tmpl-extra-names-p bad)))
  (should-not (safe-local-variable-p 'tmpl-extra-names '((colors "shout"))))
  (tmpl-test--visiting "extra/bad/page.html"
    (font-lock-ensure)
    (should tmpl-mode)
    (should-not tmpl-extra-names)
    (search-forward "shout")
    (should (eq (get-text-property (match-beginning 0) 'face) 'tmpl-unregistered-face)))
  (with-temp-buffer
    (setq-local tmpl-engine 'jinja2
                tmpl-extra-names '((colors "a")))
    (should-error (tmpl-mode 1) :type 'user-error)
    (should-not tmpl-mode)))

(ert-deftest tmpl-test-without-evil ()
  "AC-0060-0020: tmpl colors templates without loading evil."
  (should-not (featurep 'evil))
  (tmpl-test--visiting "dbt/models/orders.sql"
    (font-lock-ensure)
    (should tmpl-mode)
    (goto-char (point-min))
    (search-forward "config")
    (should (eq (get-text-property (match-beginning 0) 'face) 'tmpl-builtin-face)))
  (should-not (featurep 'evil))
  (should-not (seq-some (lambda (entry) (string-match-p "/evil" (or (car entry) "")))
                        load-history)))

;;; US-0090: delimiter completion

(defun tmpl-test--type (text engine keys electric)
  "Return TEXT after typing KEYS at its `|', with `|' marking point.
The buffer is in `text-mode' with `tmpl-mode' on for ENGINE, or off
when ENGINE is nil.  ELECTRIC is nil for `electric-pair-mode' off, or
the `electric-pair-inhibit-predicate' to turn it on with."
  (let ((was electric-pair-mode))
    (unwind-protect
        (with-temp-buffer
          (electric-pair-mode (if electric 1 -1))
          (insert text)
          (goto-char (point-min))
          (search-forward "|")
          (delete-char -1)
          (text-mode)
          (when engine
            (setq-local tmpl-engine engine)
            (tmpl-mode 1))
          (let ((electric-pair-inhibit-predicate (or electric electric-pair-inhibit-predicate)))
            (dolist (char (string-to-list keys))
              (let ((last-command-event char))
                (self-insert-command 1 char))))
          (concat (buffer-substring-no-properties (point-min) (point)) "|"
                  (buffer-substring-no-properties (point) (point-max))))
      (electric-pair-mode (if was 1 -1)))))

(defconst tmpl-test--electric-states
  '(nil electric-pair-default-inhibit electric-pair-conservative-inhibit)
  "`electric-pair-mode' off, and on with each stock inhibit predicate.")

(ert-deftest tmpl-test-complete-delimiter ()
  "AC-0090-0010: an opening delimiter gets a space and its closer.
The result is the same with `electric-pair-mode' off and on."
  (dolist (case '((jinja2 "{{" "{{ | }}") (jinja2 "{%" "{% | %}")
                  (jinja2 "{#" "{# | #}") (go-template "{{" "{{ | }}")))
    (pcase-let ((`(,engine ,keys ,result) case))
      (dolist (electric tmpl-test--electric-states)
        (should (equal (list engine keys electric
                             (tmpl-test--type "a |\nb" engine keys electric))
                       (list engine keys electric
                             (concat "a " result "\nb"))))))))

(ert-deftest tmpl-test-complete-delimiter-inside ()
  "AC-0090-0020: inside a template region only the typed text is inserted.
With `electric-pair-mode' on, tmpl adds nothing to what it inserts."
  (dolist (text '("{{ a | }}" "{% if | %}"))
    (dolist (keys '("{{" "{%"))
      (should (equal (tmpl-test--type text 'jinja2 keys nil)
                     (string-replace "|" (concat keys "|") text)))
      (dolist (electric (cdr tmpl-test--electric-states))
        (should (equal (tmpl-test--type text 'jinja2 keys electric)
                       (tmpl-test--type text nil keys electric)))))))

;;; US-0100: block matching

(defun tmpl-test--matched (text engine needle &optional nth)
  "Return the highlighted spans after putting point in NEEDLE of TEXT.
Point goes one past the start of the NTH occurrence of NEEDLE; each
span is (STRING FACE), in buffer order."
  (with-temp-buffer
    (insert text)
    (text-mode)
    (setq-local tmpl-engine engine)
    (tmpl-mode 1)
    (goto-char (point-min))
    (dotimes (_ (1+ (or nth 0)))
      (search-forward needle))
    (goto-char (1+ (match-beginning 0)))
    (tmpl-block-match-update)
    (sort (mapcar (lambda (overlay)
                    (list (overlay-start overlay)
                          (buffer-substring-no-properties (overlay-start overlay)
                                                          (overlay-end overlay))
                          (overlay-get overlay 'face)))
                  tmpl--block-overlays)
          (lambda (a b) (< (car a) (car b))))))

(defun tmpl-test--matched-strings (text engine needle &optional nth)
  "Return the strings `tmpl-test--matched' gives, checking their face.
TEXT, ENGINE, NEEDLE and NTH are as there."
  (mapcar (lambda (span)
            (should (eq (nth 2 span) 'tmpl-block-match-face))
            (nth 1 span))
          (tmpl-test--matched text engine needle nth)))

(defconst tmpl-test--block-cases
  '((jinja2 "{% if a %}1{% elif b %}2{% else %}3{% endif %}"
            ("{% if a %}" "{% elif b %}" "{% else %}" "{% endif %}"))
    (django "{% for x in xs %}{{ x }}{% empty %}none{% endfor %}"
            ("{% for x in xs %}" "{% empty %}" "{% endfor %}"))
    (jinja2 "{% block b %}{% block c %}{% endblock c %}{% endblock b %}"
            ("{% block b %}" "{% endblock b %}"))
    (go-template "{{ if .A }}a{{ else }}b{{ end }}"
                 ("{{ if .A }}" "{{ else }}" "{{ end }}")))
  "Blocks of the SPEC table: (ENGINE TEXT TAGS), TAGS in buffer order.")

(ert-deftest tmpl-test-block-match ()
  "AC-0100-0010: point on any tag of a block highlights all its tags."
  (pcase-dolist (`(,engine ,text ,tags) tmpl-test--block-cases)
    (dolist (tag tags)
      (should (equal (list engine tag (tmpl-test--matched-strings text engine tag))
                     (list engine tag tags))))))

(ert-deftest tmpl-test-block-match-nested ()
  "AC-0100-0010: nested go-template blocks match their own `end' by nesting."
  (let ((text "{{ range .A }}{{ with .B }}b{{ else }}c{{ end }}{{ end }}")
        (outer '("{{ range .A }}" "{{ end }}"))
        (inner '("{{ with .B }}" "{{ else }}" "{{ end }}")))
    (should (equal (mapcar #'car (tmpl-test--matched text 'go-template "{{ range"))
                   '(1 49)))
    (should (equal (tmpl-test--matched-strings text 'go-template "{{ range") outer))
    (should (equal (mapcar #'car (tmpl-test--matched text 'go-template "{{ end" 1))
                   '(1 49)))
    (dolist (needle '("{{ with" "{{ else"))
      (should (equal (tmpl-test--matched-strings text 'go-template needle) inner)))
    (should (equal (mapcar #'car (tmpl-test--matched text 'go-template "{{ end"))
                   '(15 29 40)))))

(ert-deftest tmpl-test-block-match-leave ()
  "AC-0100-0020: moving off the block tags removes the highlight."
  (with-temp-buffer
    (insert "{% if a %}text{% endif %}")
    (text-mode)
    (setq-local tmpl-engine 'jinja2)
    (tmpl-mode 1)
    (goto-char 2)
    (tmpl-block-match-update)
    (should (= 2 (length tmpl--block-overlays)))
    (goto-char 12)
    (tmpl-block-match-update)
    (should-not tmpl--block-overlays)
    (should-not (overlays-in (point-min) (point-max)))))

(defun tmpl-test--tmpl-buffers ()
  "Return the live buffers with `tmpl-mode' set up."
  (seq-filter (lambda (buffer) (buffer-local-value 'tmpl--st buffer)) (buffer-list)))

(ert-deftest tmpl-test-teardown ()
  "AC-0060-0010 and AC-0100-0020: resetting or killing a buffer cleans up.
A new major mode, `normal-mode' included, and killing the buffer do not
turn `tmpl-mode' off; no overlay, syntax property or timer may remain."
  (should-not (tmpl-test--tmpl-buffers))
  (dolist (reset (list #'text-mode #'normal-mode))
    (with-temp-buffer
      (insert "select 1 {% if a %}'{{ x }}{% endif %}")
      (sql-mode)
      (setq-local tmpl-engine 'dbt)
      (tmpl-mode 1)
      (syntax-ppss (point-max))
      (goto-char (1+ (string-search "{%" (buffer-string))))
      (tmpl-block-match-update)
      (should (overlays-in (point-min) (point-max)))
      (should (text-property-not-all (point-min) (point-max) 'syntax-table nil))
      (should tmpl--block-timer)
      (funcall reset)
      (should-not tmpl--st)
      (should-not (overlays-in (point-min) (point-max)))
      (should-not (text-property-not-all (point-min) (point-max) 'syntax-table nil))
      (should-not tmpl--block-timer)))
  (let ((buffer (generate-new-buffer "tmpl-test")))
    (with-current-buffer buffer
      (insert "{% if a %}{% endif %}")
      (text-mode)
      (setq-local tmpl-engine 'jinja2)
      (tmpl-mode 1))
    (should tmpl--block-timer)
    (kill-buffer buffer)
    (should-not (tmpl-test--tmpl-buffers))
    (should-not tmpl--block-timer)))

(ert-deftest tmpl-test-block-match-unclosed ()
  "AC-0100-0030: a tag without its block's other end highlights nothing."
  (dolist (case '((jinja2 "{% if a %}x" "{% if")
                  (jinja2 "{% for x in xs %}{% if a %}{% endfor %}" "{% for")
                  (jinja2 "x{% endif %}" "{% endif")
                  (jinja2 "{% else %}" "{% else")
                  (go-template "{{ if .A }}{{ range .B }}{{ end }}" "{{ if")))
    (pcase-let ((`(,engine ,text ,needle) case))
      (should (equal (list text (tmpl-test--matched text engine needle)) (list text nil))))))

;;; US-0070: registration

(ert-deftest tmpl-test-registration ()
  "AC-0070-0010: a new family, engine and rule take effect when registered."
  (let ((tmpl-families
         (cons '(erb :delimiters (("<%" "%>" expr)) :trim-chars "-" :strings (?\")
                     :pipe-filters t :operators ("|") :keywords () :constants ())
               tmpl-families))
        (tmpl-engines
         (append '((erb-like :family erb :filters ("upper"))
                   (jinja2-extra :parent jinja2 :builtins ("extra_fn")))
                 tmpl-engines))
        (tmpl-rules
         (cons '(:engine jinja2-extra :extensions ("j2") :marker "site.j2root")
               tmpl-rules)))
    (let ((text (tmpl-test--read "registry/erb.page")))
      (should (eq (tmpl-test--face-at text #'text-mode 'erb-like "<%") 'tmpl-delimiter-face))
      (should (eq (tmpl-test--face-at text #'text-mode 'erb-like "upper") 'tmpl-filter-face)))
    (should (equal (tmpl-test--visit-engine "registry/j2site/page.j2") '(jinja2-extra . t)))
    (let ((text (tmpl-test--read "registry/j2site/page.j2")))
      (should (eq (tmpl-test--face-at text #'text-mode 'jinja2-extra "extra_fn")
                  'tmpl-builtin-face))
      (should (eq (tmpl-test--face-at text #'text-mode 'jinja2-extra "range")
                  'tmpl-builtin-face)))))

;;; US-0080: dumb-jump when it is absent

(ert-deftest tmpl-test-without-dumb-jump ()
  "AC-0080-0040: without dumb-jump tmpl works and does not load it."
  (should-not (locate-library "dumb-jump"))
  (tmpl-test--visiting "gomod/layout.tmpl"
    (font-lock-ensure)
    (should tmpl-mode))
  (should-not (featurep 'dumb-jump)))

;;; Coverage matrix

(defconst tmpl-test--engine-categories
  '((jinja2 . (delimiter tag keyword operator filter test function-call builtin
               property variable string number constant comment unregistered))
    ;; Django has no test syntax (its `is' compares identity), so no tests.
    (django . (delimiter tag keyword operator filter function-call builtin
               property variable string number constant comment unregistered))
    (dbt . (delimiter tag keyword operator filter test function-call builtin
            property variable string number constant comment unregistered))
    (go-template . (delimiter keyword operator function-call builtin property
                    variable string number constant comment)))
  "Categories each engine's syntax has.")

(defconst tmpl-test--engine-fixtures
  '((jinja2 "django/shop/jinja2/keywords.html")
    (django "django/templates/keywords.html")
    (dbt "dbt/models/keywords.sql")
    (go-template "gomod/keywords.gotmpl"))
  "The keyword fixture of each engine.")

(defconst tmpl-test--host-engine-pairs
  '((sql-mode . dbt) (yaml-ts-mode . dbt) (html-ts-mode . django)
    (html-ts-mode . jinja2) (go-ts-mode . go-template) (sql-mode . go-template)
    (yaml-ts-mode . go-template) (html-ts-mode . go-template)
    (text-mode . go-template))
  "Host modes each engine's rules reach, per `lisp/init-prog.el'.")

(defconst tmpl-test--branches
  '(django-project django-app jinja2-app dbt go nearest dir-mismatch
    content-mismatch excluded-dir no-marker no-rule explicit explicit-nil)
  "Detection branches the table must exercise.")

(ert-deftest tmpl-test-coverage-matrix ()
  "Coverage matrix: engine x category, host x engine, branches, lists."
  (pcase-dolist (`(,engine ,rel) tmpl-test--engine-fixtures)
    (let ((seen (tmpl-test--with-text (tmpl-test--read rel) #'text-mode engine
                  (delete-dups
                   (cl-loop for pos from (point-min) below (point-max)
                            collect (get-text-property pos 'face))))))
      (dolist (category (alist-get engine tmpl-test--engine-categories))
        (should (equal (list engine category t)
                       (list engine category
                             (and (memq (tmpl-test--face category) seen) t)))))))
  (dolist (pair tmpl-test--host-engine-pairs)
    (should (equal (list pair t)
                   (list pair (and (seq-some (lambda (c) (and (eq (nth 1 c) (car pair))
                                                              (eq (nth 2 c) (cdr pair))))
                                             tmpl-test--host-cases)
                                   t)))))
  (dolist (branch tmpl-test--branches)
    (should (equal (list branch t)
                   (list branch (and (tmpl-test--row-for-branch branch) t)))))
  (dolist (rule tmpl-rules)
    (should (seq-some (lambda (row) (eq (nth 1 row) (plist-get rule :engine)))
                      tmpl-test--detection)))
  (dolist (engine (mapcar #'car tmpl-engines))
    (should (assq engine tmpl-test--engine-fixtures))))

(defun tmpl-test--row-for-branch (branch)
  "Return the first detection row whose branch is BRANCH."
  (seq-find (lambda (row) (eq (nth 2 row) branch)) tmpl-test--detection))

(provide 'tmpl-test)
;;; tmpl-test.el ends here
