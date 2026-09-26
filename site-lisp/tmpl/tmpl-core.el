;;; tmpl-core.el --- Registry and engine detection for tmpl -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; The registry of template families, engines and detection rules, and the
;; resolution of a buffer's engine from explicit settings or marker files.
;;
;; A family fixes delimiters and structural rules; an engine belongs to a
;; family (directly or through a parent engine) and adds its own name
;; lists.  Adding a language or a detection rule is a registry entry, not a
;; code change.
;;
;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defgroup tmpl nil
  "Template syntax layered over the host major mode."
  :group 'languages
  :prefix "tmpl-")

;;; Families

(defcustom tmpl-families
  '((jinja
     :delimiters (("{{" "}}" expr) ("{%" "%}" stmt) ("{#" "#}" comment))
     :trim-chars "-+"
     :trim-space nil
     :strings (?\' ?\")
     :raw-tags ("raw" "verbatim")
     :comment-tags ("comment")
     :pipe-filters t
     :fields nil
     :bare-calls nil
     :operators ("**" "//" "==" "!=" "<=" ">=" "<" ">" "=" "+" "-" "*" "/"
                 "%" "~" "|")
     ;; Jinja 3.1.6 src/jinja2/parser.py: words compared by value in
     ;; parse_or/parse_and/parse_not/parse_compare/parse_condexpr, and
     ;; the constants of parse_primary.  Django 6.0.8
     ;; django/template/smartif.py OPERATORS uses the same words.
     :keywords ("and" "or" "not" "in" "is" "if" "else")
     :constants ("true" "True" "false" "False" "none" "None")
     :definitions
     (("function" "\\{%[-+]?\\s*macro\\s+JJJ\\s*\\(")
      ("type" "\\{%[-+]?\\s*block\\s+JJJ\\b")
      ("variable" "\\{%[-+]?\\s*set\\s+JJJ\\s*(=|[-+]?%\\})")
      ("variable" "\\{%[-+]?\\s*import\\s+\\S+\\s+as\\s+JJJ\\b")
      ("function" "\\{%[-+]?\\s*from\\s+\\S+\\s+import\\s+([\\w\\s,]*,\\s*)?JJJ\\b")))
    (go
     :delimiters (("{{" "}}" expr))
     ;; text/template fixes no extension; these are the common ones.
     :suffixes ("tmpl" "tpl" "gotmpl")
     :trim-chars "-"
     :trim-space t
     :strings (?\" ?` ?\')
     :comment-inner ("/*" . "*/")
     :pipe-filters nil
     :fields t
     :bare-calls t
     :operators (":=" "=" "|")
     ;; Go 1.27.1 src/text/template/parse/lex.go `key' map, minus "."
     ;; (a field) and "nil" (a constant).
     :keywords ("block" "break" "continue" "define" "else" "end" "if"
                "range" "template" "with")
     :constants ("true" "false" "nil")
     :definitions
     (("function" "\\{\\{-?\\s*define\\s+\"JJJ\"")
      ("function" "\\{\\{-?\\s*block\\s+\"JJJ\"")
      ("variable" "\\$JJJ\\s*:="))))
  "Template families: delimiters and structural rules shared by engines.
Each entry is (FAMILY . PLIST).  PLIST keys:

:delimiters    list of (OPEN CLOSE KIND); KIND is `expr', `stmt' (the
               first word is a tag) or `comment' (the inside is a comment).
:trim-chars    characters that may follow OPEN or precede CLOSE as
               whitespace-control markers.
:trim-space    non-nil when a marker must be separated from the inside
               by whitespace.
:strings       characters that quote strings; a backtick quotes a raw
               string without escapes.
:raw-tags      statement tags whose body up to the end tag is plain text.
:comment-tags  statement tags whose body up to the end tag is a comment.
:comment-inner (BEGIN . END) of a comment written inside an expression.
:pipe-filters  non-nil when a word after `|' is a filter.
:fields        non-nil when `.Name' is a field and `$x' a variable.
:bare-calls    non-nil when any other bare word is a function call.
:operators     operator strings.
:keywords      expression keywords.
:constants     constant words.
:definitions   list of (TYPE REGEX): dumb-jump rules for definitions,
               with JJJ standing for the name.
:suffixes      file name suffixes of the language's own files; the host
               mode comes from the name without them."
  :type '(alist :key-type symbol :value-type plist))

;;; Engines

(defcustom tmpl-engines
  '((jinja2
     :family jinja
     ;; Jinja 3.1.6 parser.py parse_test: a test follows `is'.  Django has
     ;; no tests: its `is' compares identity (smartif.py OPERATORS).
     :test-keyword "is"
     ;; Jinja 3.1.6: parser.py `_statement_keywords' with the middle and
     ;; end tags passed to parse_statements, parse_statement's `call' and
     ;; `filter', lexer.py `raw', and ext.py `tags'.
     :tags ("for" "else" "endfor" "if" "elif" "endif" "block" "endblock"
            "extends" "print" "macro" "endmacro" "include" "from" "import"
            "set" "endset" "with" "endwith" "autoescape" "endautoescape"
            "call" "endcall" "filter" "endfilter" "raw" "endraw"
            "trans" "pluralize" "endtrans" "do" "break" "continue" "debug")
     ;; Jinja 3.1.6 src/jinja2/filters.py FILTERS (54).
     :filters ("abs" "attr" "batch" "capitalize" "center" "count" "d"
               "default" "dictsort" "e" "escape" "filesizeformat" "first"
               "float" "forceescape" "format" "groupby" "indent" "int"
               "join" "last" "length" "list" "lower" "items" "map" "min"
               "max" "pprint" "random" "reject" "rejectattr" "replace"
               "reverse" "round" "safe" "select" "selectattr" "slice"
               "sort" "string" "striptags" "sum" "title" "trim" "truncate"
               "unique" "upper" "urlencode" "urlize" "wordcount"
               "wordwrap" "xmlattr" "tojson")
     ;; Jinja 3.1.6 src/jinja2/tests.py TESTS, word keys (33).
     :tests ("odd" "even" "divisibleby" "defined" "undefined" "filter"
             "test" "none" "boolean" "false" "true" "integer" "float"
             "lower" "upper" "string" "mapping" "number" "sequence"
             "iterable" "callable" "sameas" "escaped" "in" "eq" "equalto"
             "ne" "gt" "greaterthan" "ge" "lt" "lessthan" "le")
     ;; Jinja 3.1.6 src/jinja2/defaults.py DEFAULT_NAMESPACE, and `loop'.
     :builtins ("range" "dict" "lipsum" "cycler" "joiner" "namespace"
                "loop")
     ;; Jinja 3.1.6: the end tokens each parse_* method of parser.py passes
     ;; to parse_statements; `set' without `=' (parse_set), lexer.py
     ;; `raw', and ext.py InternationalizationExtension (`pluralize').
     :blocks (("for" ("else") "endfor") ("if" ("elif" "else") "endif")
              ("block" () "endblock") ("macro" () "endmacro")
              ("call" () "endcall") ("filter" () "endfilter")
              ("set" () "endset" "=") ("with" () "endwith")
              ("autoescape" () "endautoescape") ("raw" () "endraw")
              ("trans" ("pluralize") "endtrans")))
    (django
     :family jinja
     ;; Django 6.0.8: django/template/defaulttags.py `@register.tag' and
     ;; the `querystring' simple tag, with the middle and end tags each
     ;; parser looks for; django/template/loader_tags.py (`block',
     ;; `extends', `include'); django/templatetags/{static,i18n,l10n,tz,cache}.py.
     :tags ("autoescape" "endautoescape" "comment" "endcomment" "cycle"
            "csrf_token" "debug" "filter" "endfilter" "firstof" "for"
            "empty" "endfor" "if" "elif" "else" "endif" "ifchanged"
            "endifchanged" "load" "lorem" "now" "partialdef"
            "endpartialdef" "partial" "regroup" "resetcycle" "spaceless"
            "endspaceless" "templatetag" "url" "verbatim" "endverbatim"
            "widthratio" "with" "endwith" "querystring"
            "block" "endblock" "extends" "include"
            "get_static_prefix" "get_media_prefix" "static"
            "get_available_languages" "get_language_info"
            "get_language_info_list" "get_current_language"
            "get_current_language_bidi" "translate" "trans"
            "blocktranslate" "plural" "endblocktranslate" "blocktrans"
            "endblocktrans" "language" "endlanguage"
            "localize" "endlocalize"
            "localtime" "endlocaltime" "timezone" "endtimezone"
            "get_current_timezone"
            "cache" "endcache")
     ;; Django 6.0.8: django/template/defaultfilters.py (57) and the
     ;; filters of django/templatetags/{i18n,l10n,tz}.py (9).
     :filters ("addslashes" "capfirst" "escapejs" "json_script"
               "floatformat" "iriencode" "linenumbers" "lower" "make_list"
               "slugify" "stringformat" "title" "truncatechars"
               "truncatechars_html" "truncatewords" "truncatewords_html"
               "upper" "urlencode" "urlize" "urlizetrunc" "wordcount"
               "wordwrap" "ljust" "rjust" "center" "cut" "escape"
               "escapeseq" "force_escape" "linebreaks" "linebreaksbr"
               "safe" "safeseq" "striptags" "dictsort" "dictsortreversed"
               "first" "join" "last" "length" "random" "slice"
               "unordered_list" "add" "get_digit" "date" "time"
               "timesince" "timeuntil" "default" "default_if_none"
               "divisibleby" "yesno" "filesizeformat" "pluralize"
               "phone2numeric" "pprint"
               "language_name" "language_name_translated"
               "language_name_local" "language_bidi"
               "localize" "unlocalize"
               "localtime" "utc" "timezone")
     :tests ()
     ;; Django 6.0.8: `forloop' (defaulttags.py ForNode.render) and
     ;; `block' (loader_tags.py BlockNode.render).
     :builtins ("forloop" "block")
     ;; Django 6.0.8: the tags each parser.parse((...)) or skip_past call
     ;; ends at in defaulttags.py, loader_tags.py and templatetags/
     ;; {i18n,l10n,tz,cache}.py.
     :blocks (("autoescape" () "endautoescape") ("comment" () "endcomment")
              ("filter" () "endfilter") ("for" ("empty") "endfor")
              ("if" ("elif" "else") "endif")
              ("ifchanged" ("else") "endifchanged")
              ("partialdef" () "endpartialdef") ("spaceless" () "endspaceless")
              ("verbatim" () "endverbatim") ("with" () "endwith")
              ("block" () "endblock")
              ("blocktranslate" ("plural") "endblocktranslate")
              ("blocktrans" ("plural") "endblocktrans")
              ("language" () "endlanguage") ("localize" () "endlocalize")
              ("localtime" () "endlocaltime") ("timezone" () "endtimezone")
              ("cache" () "endcache")))
    (dbt
     :parent jinja2
     ;; docs.getdbt.com b27389b3cba9d0aaf8cde2eb82a2e41da7c5e66f: the
     ;; block tags of jinja-macros.md, data-tests.md,
     ;; snapshots-jinja-legacy.md, create-new-materializations.md,
     ;; documentation.md and statement-blocks.md.
     :tags ("test" "endtest" "snapshot" "endsnapshot" "materialization"
            "endmaterialization" "docs" "enddocs")
     ;; Same commit, website/docs/reference/dbt-jinja-functions (48 pages):
     ;; - 42 pages naming a single function or variable, one name each;
     ;; - cross-database-macros.md: its 34 `###' macros, called `dbt.NAME';
     ;; - on-run-end-context.md: `database_schemas' and `results' (its
     ;;   `schemas' is the page of the same name);
     ;; - dbt-project-yml-context.md, profiles-yml-context.md,
     ;;   properties-yml-context.md, packages.yml.md: only names above.
     :builtins ("adapter" "as_bool" "as_native" "as_number" "builtins"
                "config" "dbt_version" "debug" "dispatch" "doc" "env_var"
                "exceptions" "execute" "flags" "fromjson" "fromyaml"
                "graph" "info_schema" "invocation_id" "local_md5" "log"
                "model" "modules" "print" "project_name" "ref" "return"
                "run_query" "run_started_at" "schema" "schemas"
                "selected_resources" "set" "source" "statement" "target"
                "this" "thread_id" "tojson" "toyaml" "var" "zip"
                "dbt.type_bigint" "dbt.type_boolean" "dbt.type_float"
                "dbt.type_int" "dbt.type_numeric" "dbt.type_string"
                "dbt.type_timestamp" "dbt.current_timestamp"
                "dbt.except" "dbt.intersect"
                "dbt.array_append" "dbt.array_concat" "dbt.array_construct"
                "dbt.concat" "dbt.hash" "dbt.length" "dbt.position"
                "dbt.replace" "dbt.right" "dbt.split_part"
                "dbt.escape_single_quotes" "dbt.string_literal"
                "dbt.any_value" "dbt.bool_or" "dbt.listagg"
                "dbt.cast" "dbt.cast_bool_to_text" "dbt.safe_cast"
                "dbt.equals"
                "dbt.date" "dbt.dateadd" "dbt.datediff" "dbt.date_trunc"
                "dbt.last_day"
                "database_schemas" "results")
     :definitions
     (("function" "\\{%[-+]?\\s*test\\s+JJJ\\s*\\(")
      ("type" "\\{%[-+]?\\s*snapshot\\s+JJJ\\b")
      ("function" "\\{%[-+]?\\s*materialization\\s+JJJ\\s*,"))
     ;; The block tags above, each with its end tag.
     :blocks (("test" () "endtest") ("snapshot" () "endsnapshot")
              ("materialization" () "endmaterialization") ("docs" () "enddocs")))
    (go-template
     :family go
     ;; Go 1.27.1 src/text/template/funcs.go builtins() (19).
     :builtins ("and" "call" "html" "index" "slice" "js" "len" "not" "or"
                "print" "printf" "println" "urlquery" "eq" "ge" "gt" "le"
                "lt" "ne")
     ;; Go 1.27.1 src/text/template/parse/parse.go: parseControl ends
     ;; `if', `range' and `with' at itemEnd or itemElse, and `define'
     ;; (parseDefinition) and `block' (blockControl) at itemEnd.
     :blocks (("if" ("else") "end") ("range" ("else") "end")
              ("with" ("else") "end") ("define" () "end") ("block" () "end"))))
  "Template engines.
Each entry is (ENGINE . PLIST).  PLIST has `:family' naming an entry of
`tmpl-families', or `:parent' naming another engine whose lists it
extends.  List keys, each appended to the parent's:

:tags         statement tags, including middle and end tags.
:filters      built-in filters.
:tests        built-in tests.
:builtins     built-in functions and variables.
:definitions  extra (TYPE REGEX) definition rules, as in `tmpl-families'.
:blocks       list of (OPEN MIDDLES END [SINGLE]): statement tags that open
              a block, its middle tags and its end tag; with the regexp
              SINGLE, an OPEN statement whose text matches it has no end.

`:test-keyword', inherited unless given, is the word after which a word
is a test; an engine without it has no tests."
  :type '(alist :key-type symbol :value-type plist))

;;; Detection rules

(defcustom tmpl-rules
  '((:engine django :extensions ("html") :marker "manage.py"
     :marker-content "DJANGO_SETTINGS_MODULE" :dir "templates")
    (:engine jinja2 :extensions ("html") :marker "manage.py"
     :marker-content "DJANGO_SETTINGS_MODULE" :dir "jinja2")
    (:engine dbt :extensions ("sql" "yml" "yaml") :marker "dbt_project.yml"
     :exclude-dirs (".github"))
    (:engine go-template :marker "go.mod"))
  "Rules that detect a file's engine from a marker file above it.
Each rule is a plist:

:engine          the engine a match gives.
:extensions      file extensions the rule applies to; by default the
                 `:suffixes' of the engine's family.
:marker          file name looked up in the file's ancestor directories.
:marker-content  regexp the marker's contents must match, or nil.
:dir             directory name the path from the marker to the file
                 must contain, or nil.
:exclude-dirs    directory names that path must not contain.

When several rules match, the one whose marker is nearest wins; ties go
to the earlier rule."
  :type '(repeat plist))

;;; Modes with their own template support

(defcustom tmpl-template-modes '(web-mode)
  "Major modes that color templates themselves.
tmpl stays off in these modes and in modes derived from them, even when
an engine is set explicitly or a rule matches: the mode's own template
support would otherwise be overridden."
  :type '(repeat symbol))

;;; Explicit setting

(defvar-local tmpl-engine nil
  "Template engine of the current buffer, or nil for none.
Set it as a file-local or directory-local variable to choose the engine
explicitly; nil set that way disables detection.")
(put 'tmpl-engine 'safe-local-variable #'symbolp)

(defvar-local tmpl-extra-names nil
  "Names the project adds to the engine's lists, as an alist.
Each entry is (CATEGORY . NAMES): CATEGORY is `tags', `filters' or
`tests' and NAMES a list of strings, colored as registered names of
that category.  Set it as a file-local or directory-local variable.")
(put 'tmpl-extra-names 'safe-local-variable #'tmpl-extra-names-p)

(defun tmpl-extra-names-p (value)
  "Return non-nil when VALUE has the form of `tmpl-extra-names'."
  (and (proper-list-p value)
       (seq-every-p (lambda (entry)
                      (and (consp entry)
                           (memq (car entry) '(tags filters tests))
                           (proper-list-p (cdr entry))
                           (seq-every-p #'stringp (cdr entry))))
                    value)))

;;; Engine specs

(defun tmpl-engine-spec (engine)
  "Return the effective plist of ENGINE with inherited lists merged.
The result has `:family' bound to the family plist and `:family-name'
to its symbol.  Signal `user-error' when ENGINE is not registered."
  (let ((entry (assq engine tmpl-engines)))
    (unless entry
      (user-error "Template engine `%s' is not registered in `tmpl-engines'" engine))
    (let* ((own (cdr entry))
           (parent (plist-get own :parent))
           (base (if parent
                     (tmpl-engine-spec parent)
                   (let ((family (plist-get own :family)))
                     (unless (assq family tmpl-families)
                       (user-error "Template engine `%s' names unregistered family `%s'"
                                   engine family))
                     (list :family-name family
                           :family (cdr (assq family tmpl-families)))))))
      (when (plist-member own :test-keyword)
        (setq base (plist-put (copy-sequence base) :test-keyword
                              (plist-get own :test-keyword))))
      (dolist (key '(:tags :filters :tests :builtins :definitions :blocks))
        (setq base (plist-put (copy-sequence base) key
                              (append (plist-get base key) (plist-get own key)))))
      base)))

;;; Detection

(defun tmpl-rule-extensions (rule)
  "Return the file extensions RULE of `tmpl-rules' applies to."
  (or (plist-get rule :extensions)
      (plist-get (plist-get (tmpl-engine-spec (plist-get rule :engine)) :family)
                 :suffixes)))

(defun tmpl-suffixes ()
  "Return the template suffixes of all families in `tmpl-families'."
  (delete-dups (mapcan (lambda (family) (copy-sequence (plist-get (cdr family) :suffixes)))
                       tmpl-families)))

(defun tmpl--rule-applies-p (rule file)
  "Return non-nil when RULE's extensions match the name of FILE."
  (member (file-name-extension file) (tmpl-rule-extensions rule)))

(defun tmpl--marker-p (rule dir)
  "Return non-nil when DIR holds the marker file of RULE."
  (let ((marker (expand-file-name (plist-get rule :marker) dir))
        (content (plist-get rule :marker-content)))
    (and (file-regular-p marker)
         (or (null content)
             (with-temp-buffer
               (condition-case err
                   (insert-file-contents marker)
                 (error (user-error "Failed to read template marker %s: %s"
                                    marker (error-message-string err))))
               (goto-char (point-min))
               (re-search-forward content nil t))))))

(defun tmpl--path-matches-p (rule root file)
  "Return non-nil when the path from ROOT to FILE satisfies RULE."
  (let ((dirs (butlast (file-name-split (file-relative-name file root))))
        (dir (plist-get rule :dir)))
    (and (or (null dir) (member dir dirs))
         (not (cl-intersection dirs (plist-get rule :exclude-dirs)
                               :test #'string=)))))

(defun tmpl-detect (file)
  "Return the engine the rules in `tmpl-rules' give FILE, or nil.
The file system is not touched when no rule applies to the name of FILE."
  (let ((file (expand-file-name file))
        best best-depth)
    (dolist (rule tmpl-rules)
      (when (tmpl--rule-applies-p rule file)
        (let ((root (locate-dominating-file
                     (file-name-directory file)
                     (lambda (dir) (tmpl--marker-p rule dir)))))
          (when (and root
                     (tmpl--path-matches-p rule root file)
                     (or (null best-depth)
                         (> (length (expand-file-name root)) best-depth)))
            (setq best rule
                  best-depth (length (expand-file-name root)))))))
    (plist-get best :engine)))

(defun tmpl-resolve ()
  "Return the engine for the current buffer.
In a mode of `tmpl-template-modes' it is nil.  Otherwise an explicit
`tmpl-engine' wins, including nil, and failing that the engine comes
from `tmpl-detect' on the visited file.  Signal `user-error' when the
explicit engine is not registered."
  (cond
   ((derived-mode-p tmpl-template-modes) nil)
   ((local-variable-p 'tmpl-engine)
    (when tmpl-engine
      (tmpl-engine-spec tmpl-engine)
      tmpl-engine))
   (buffer-file-name (tmpl-detect buffer-file-name))))

(provide 'tmpl-core)
;;; tmpl-core.el ends here
