;;; tmpl.el --- Template syntax layered over the host major mode -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; `tmpl-mode' colors template regions ({{ }}, {% %}, {# #} and the like)
;; by syntax category on top of the file's own major mode, and keeps the
;; host's syntax state from being disturbed by quotes or comment starters
;; inside them.  `tmpl-global-mode' picks the engine of each visited file
;; once its local variables are in effect (see `tmpl-core').  When
;; dumb-jump is loaded, template definitions (macro, block, define...)
;; become jump targets.
;;
;; How it works:
;; - A buffer-wide scan lists template regions; it is redone after every
;;   change and drives both syntax and font-lock, so both see the same
;;   regions.
;; - Syntax: each region's interior loses every syntax class but word,
;;   symbol and whitespace; outside host strings and comments the region's
;;   first and last characters become generic string fences, making the
;;   region one opaque token.  The host's `syntax-propertize-function'
;;   runs only on the text between regions.
;; - Font-lock: one keyword, appended after the host's, sets a base face
;;   on each region and then the category faces.  Regions are extended to
;;   whole in `font-lock-extend-region-functions'; a change that alters
;;   the region list flushes font-lock from the first altered region on.
;; - Tree-sitter hosts: the primary parser's included ranges exclude the
;;   regions, so the host parses the text as if the regions were blank.
;;
;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tmpl-core)

(declare-function treesit-parser-set-included-ranges "treesit.c")
(defvar treesit-primary-parser)
(defvar font-lock-beg)
(defvar font-lock-end)
(defvar dumb-jump-find-rules)
(defvar dumb-jump-language-file-exts)

;;; Faces

(defface tmpl-region-face '((t))
  "Base face of a template region; text in no category shows it."
  :group 'tmpl)

(defface tmpl-delimiter-face '((t :inherit font-lock-bracket-face :weight bold))
  "Face for template delimiters and whitespace-control markers.
Bold keeps the region boundary apart from the host's tag, function and
keyword faces, which themes often share with preprocessor text."
  :group 'tmpl)

(defface tmpl-tag-face '((t :inherit font-lock-keyword-face))
  "Face for statement tag names."
  :group 'tmpl)

(defface tmpl-keyword-face '((t :inherit font-lock-keyword-face))
  "Face for expression keywords and actions."
  :group 'tmpl)

(defface tmpl-operator-face '((t :inherit font-lock-operator-face))
  "Face for operators."
  :group 'tmpl)

(defface tmpl-filter-face '((t :inherit font-lock-function-call-face))
  "Face for filters."
  :group 'tmpl)

(defface tmpl-test-face '((t :inherit font-lock-function-call-face))
  "Face for tests."
  :group 'tmpl)

(defface tmpl-function-call-face '((t :inherit font-lock-function-call-face))
  "Face for function calls."
  :group 'tmpl)

(defface tmpl-builtin-face '((t :inherit font-lock-builtin-face))
  "Face for built-in functions and variables of the engine."
  :group 'tmpl)

(defface tmpl-property-face '((t :inherit font-lock-property-use-face))
  "Face for attribute and field access."
  :group 'tmpl)

(defface tmpl-variable-face '((t :inherit font-lock-variable-use-face))
  "Face for variables."
  :group 'tmpl)

(defface tmpl-string-face '((t :inherit font-lock-string-face))
  "Face for strings."
  :group 'tmpl)

(defface tmpl-number-face '((t :inherit font-lock-number-face))
  "Face for numbers."
  :group 'tmpl)

(defface tmpl-constant-face '((t :inherit font-lock-constant-face))
  "Face for constants."
  :group 'tmpl)

(defface tmpl-comment-face '((t :inherit font-lock-comment-face))
  "Face for template comments."
  :group 'tmpl)

(defface tmpl-unregistered-face '((t :inherit shadow))
  "Face for a name in a tag, filter or test position that is not registered.
It is muted rather than a warning: the name may be a project's own,
see `tmpl-extra-names'."
  :group 'tmpl)

;;; Buffer state

(cl-defstruct (tmpl--state (:constructor tmpl--state-create))
  "Per-buffer state of `tmpl-mode'."
  engine spec family open-re
  regions tick snapshot
  host-syntax-function plp-local plp-value)

(defvar-local tmpl--st nil
  "The `tmpl--state' of the current buffer while `tmpl-mode' is on.")

(defconst tmpl--ident-re "[A-Za-z_][A-Za-z0-9_]*"
  "Regexp matching an identifier.")

(defconst tmpl--number-re
  (concat "0[xX][0-9a-fA-F_]+\\(?:\\.[0-9a-fA-F_]*\\)?\\(?:[pP][+-]?[0-9_]+\\)?"
          "\\|0[bBoO][0-9_]+"
          "\\|[0-9][0-9_]*\\(?:\\.[0-9][0-9_]*\\)?\\(?:[eE][+-]?[0-9_]+\\)?")
  "Regexp matching a number literal.")

(defun tmpl--fam (st key)
  "Return KEY of the family plist of ST."
  (plist-get (tmpl--state-family st) key))

(defun tmpl--delimiter (st open)
  "Return the delimiter entry of ST whose opening string is OPEN."
  (assoc open (tmpl--fam st :delimiters)))

;;; Region scan
;;
;; An entry is (BEG END KIND) with KIND one of `expr', `stmt', `comment'
;; and `raw' (the plain-text body of a raw block, not a region), or
;; (BEG END block OPEN-END CLOSE-BEG) for a comment block whose opening
;; tag ends at OPEN-END and closing tag starts at CLOSE-BEG.

(defun tmpl--inner-start (st beg open)
  "Return where the inside of a region at BEG opened by OPEN starts.
Skips a whitespace-control marker per the family of ST."
  (let ((pos (+ beg (length open)))
        (trim (tmpl--fam st :trim-chars)))
    (if (and trim
             (char-after pos)
             (seq-contains-p trim (char-after pos))
             (or (not (tmpl--fam st :trim-space))
                 (memq (char-after (1+ pos)) '(?\s ?\t ?\n ?\r))))
        (1+ pos)
      pos)))

(defun tmpl--inner-end (st inner end close)
  "Return where the inside of a region ending at END with CLOSE ends.
INNER is where the inside starts; ST gives the family's markers."
  (let ((pos (- end (length close)))
        (trim (tmpl--fam st :trim-chars)))
    (if (and trim
             (> pos inner)
             (seq-contains-p trim (char-before pos))
             (or (not (tmpl--fam st :trim-space))
                 (and (> (1- pos) inner)
                      (memq (char-before (1- pos)) '(?\s ?\t ?\n ?\r)))))
        (1- pos)
      pos)))

(defun tmpl--tag (st beg end open)
  "Return (TAG . NAME) of the statement region BEG..END opened by OPEN.
TAG is the first word, or nil; NAME is a second word, as in
`{% verbatim name %}', or nil.  ST gives the family's markers."
  (save-excursion
    (goto-char (tmpl--inner-start st beg open))
    (skip-chars-forward " \t\r\n" end)
    (when (looking-at tmpl--ident-re)
      (let ((tag (match-string-no-properties 0)))
        (goto-char (match-end 0))
        (cons tag
              (and (looking-at (concat "[ \t\r\n]+\\(" tmpl--ident-re "\\)"))
                   (<= (match-end 1) end)
                   (match-string-no-properties 1)))))))

(defun tmpl--end-tag-re (st open close tag name)
  "Return a regexp matching the end tag of TAG between OPEN and CLOSE.
With NAME the end tag must repeat it, as in `{% endverbatim name %}'.
ST gives the family's markers."
  (let* ((trim (tmpl--fam st :trim-chars))
         (mark (if trim (concat "[" (regexp-quote trim) "]?") "")))
    (concat (regexp-quote open) mark "[ \t\r\n]*end" (regexp-quote tag)
            (if name (concat "[ \t\r\n]+" (regexp-quote name)) "")
            "[ \t\r\n]*" mark (regexp-quote close))))

(defun tmpl--scan (st)
  "Return the entries of the current buffer for ST, in buffer order."
  (let ((case-fold-search nil)
        (raw-tags (tmpl--fam st :raw-tags))
        (comment-tags (tmpl--fam st :comment-tags))
        entries)
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while (re-search-forward (tmpl--state-open-re st) nil t)
          (let* ((beg (match-beginning 0))
                 (open (match-string-no-properties 0))
                 (delim (tmpl--delimiter st open))
                 (close (nth 1 delim))
                 (kind (nth 2 delim)))
            (if (not (search-forward close nil t))
                (goto-char (+ beg (length open)))
              (let* ((end (point))
                     (head (and (eq kind 'stmt) (tmpl--tag st beg end open)))
                     (tag (car head))
                     (closer (and tag
                                  (or (member tag raw-tags) (member tag comment-tags))
                                  (save-excursion
                                    (and (re-search-forward
                                          (tmpl--end-tag-re st open close tag (cdr head))
                                          nil t)
                                         (cons (match-beginning 0) (match-end 0)))))))
                (cond
                 ((and closer (member tag raw-tags))
                  (push (list beg end 'stmt) entries)
                  (push (list end (car closer) 'raw) entries)
                  (push (list (car closer) (cdr closer) 'stmt) entries)
                  (goto-char (cdr closer)))
                 (closer
                  (push (list beg (cdr closer) 'block end (car closer)) entries)
                  (goto-char (cdr closer)))
                 (t (push (list beg end kind) entries)))))))))
    (nreverse entries)))

(defun tmpl--entries (st)
  "Return the current entries of ST, rescanning after buffer changes."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (eql tick (tmpl--state-tick st))
      (setf (tmpl--state-regions st) (tmpl--scan st)
            (tmpl--state-tick st) tick))
    (tmpl--state-regions st)))

(defun tmpl--regions-in (st beg end)
  "Return the template regions of ST overlapping BEG..END."
  (seq-filter (lambda (e)
                (and (not (eq (nth 2 e) 'raw))
                     (< (nth 0 e) end)
                     (> (nth 1 e) beg)))
              (tmpl--entries st)))

(defun tmpl--region-at (st pos)
  "Return the template region of ST containing POS, or nil."
  (car (tmpl--regions-in st pos (1+ pos))))

(defun tmpl-regions ()
  "Return the template regions of the current buffer as (BEG . END) pairs."
  (unless tmpl--st
    (user-error "`tmpl-mode' is not enabled in %s" (buffer-name)))
  (mapcar (lambda (e) (cons (nth 0 e) (nth 1 e)))
          (tmpl--regions-in tmpl--st (point-min) (point-max))))

;;; Change tracking

(defun tmpl--before-change (_beg _end)
  "Record the entries before a change."
  (when tmpl--st
    (setf (tmpl--state-snapshot tmpl--st) (tmpl--entries tmpl--st))))

(defun tmpl--shift (pos beg old-end delta)
  "Return POS moved by a change of BEG..OLD-END that grew by DELTA.
Return nil when POS lay inside the replaced text."
  (cond ((<= pos beg) pos)
        ((>= pos old-end) (+ pos delta))))

(defun tmpl--after-change (beg end old-len)
  "Refresh syntax and font-lock when the change BEG..END altered the regions.
OLD-LEN is the length of the replaced text.  Both are flushed from the
first region that differs, which may lie before BEG: an end tag typed
far below turns the text after its opening tag into a raw body."
  (when tmpl--st
    (let* ((old (tmpl--state-snapshot tmpl--st))
           (new (tmpl--entries tmpl--st))
           (old-end (+ beg old-len))
           (delta (- (- end beg) old-len))
           (from nil))
      (while (and (or old new) (not from))
        (let ((o (car old)) (n (car new)))
          (if (and o n
                   (equal (cons (nth 2 o)
                                (mapcar (lambda (p) (tmpl--shift p beg old-end delta))
                                        (cons (nth 0 o) (cons (nth 1 o) (nthcdr 3 o)))))
                          (cons (nth 2 n) (cons (nth 0 n) (cons (nth 1 n) (nthcdr 3 n))))))
              (setq old (cdr old) new (cdr new))
            (setq from (min beg
                            (if o (nth 0 o) beg)
                            (if n (nth 0 n) beg))))))
      (when from
        (syntax-ppss-flush-cache from)
        (font-lock-flush from (point-max)))
      (tmpl--update-treesit-ranges tmpl--st))))

;;; Syntax

(defun tmpl--neutral-p (char)
  "Return non-nil when CHAR inside a region must become punctuation.
Only letters, digits and `_' with word or symbol syntax, and whitespace,
keep the host syntax, and only without flags: host parsing ignores
them, and `symbol-at-point' then reads a template identifier (`ns' of
`ns.f', `v' of `$v').  Newlines keep their syntax so line comments
still end."
  (let ((syn (aref (syntax-table) char)))
    (let ((class (logand (car syn) #xffff))
          (flags (ash (car syn) -16)))
      (not (or (eq char ?\n)
               (and (= flags 0)
                    (if (string-match-p "[[:alnum:]_]" (string char))
                        (memq class '(2 3))
                      (= class 0))))))))

(defun tmpl--propertize-region (beg end)
  "Hide the template region BEG..END from host syntax."
  (let ((punct (string-to-syntax "."))
        (fence (string-to-syntax "|"))
        (opaque (not (nth 8 (syntax-ppss beg)))))
    (put-text-property beg end 'syntax-multiline t)
    (save-excursion
      (goto-char beg)
      (while (< (point) end)
        (let ((pos (point)))
          (cond ((and opaque (or (= pos beg) (= pos (1- end))))
                 (put-text-property pos (1+ pos) 'syntax-table fence))
                ((tmpl--neutral-p (char-after pos))
                 (put-text-property pos (1+ pos) 'syntax-table punct)))
          (forward-char 1))))))

(defun tmpl--syntax-propertize (host start end)
  "Propertize START..END: HOST between regions, then each region.
HOST is the host's `syntax-propertize-function', which this function
advises, or nil."
  (let ((pos start))
    (dolist (region (tmpl--regions-in tmpl--st start end))
      (let ((rbeg (nth 0 region)) (rend (nth 1 region)))
        (when (and host (< pos rbeg))
          (funcall host pos rbeg))
        (tmpl--propertize-region rbeg rend)
        (setq pos (max pos rend))))
    (when (and host (< pos end))
      (funcall host pos end))))

(defun tmpl--syntax-alone (start end)
  "`syntax-propertize-function' for hosts without one, on START..END."
  (tmpl--syntax-propertize nil start end))

;;; Tree-sitter hosts

(defun tmpl--treesit-parser ()
  "Return the primary tree-sitter parser of the buffer, or nil."
  (and (boundp 'treesit-primary-parser) treesit-primary-parser))

(defun tmpl--update-treesit-ranges (st)
  "Make the primary tree-sitter parser skip the regions of ST."
  (when-let* ((parser (tmpl--treesit-parser)))
    (let ((pos (point-min)) ranges)
      (dolist (region (tmpl--regions-in st (point-min) (point-max)))
        (when (< pos (nth 0 region))
          (push (cons pos (nth 0 region)) ranges))
        (setq pos (nth 1 region)))
      (when (< pos (point-max))
        (push (cons pos (point-max)) ranges))
      ;; A buffer that is all template still needs a range; an empty one
      ;; at the end parses as an empty document.
      (treesit-parser-set-included-ranges
       parser (or (nreverse ranges) (list (cons (point-max) (point-max))))))))

;;; Font-lock

(defun tmpl--put (beg end category)
  "Give BEG..END the face of CATEGORY."
  (when (< beg end)
    (put-text-property beg end 'face (intern (format "tmpl-%s-face" category)))))

(defun tmpl--skip-string (quote limit)
  "Move past the string opened by QUOTE at point, stopping at LIMIT."
  (forward-char 1)
  (let ((stop (if (eq quote ?`) "^`" (concat "^\\\\" (string quote)))))
    (while (progn (skip-chars-forward stop limit)
                  (and (< (point) limit)
                       (not (eq (char-after) quote))))
      (forward-char (min 2 (- limit (point)))))
    (when (< (point) limit)
      (forward-char 1))))

(defun tmpl--listed-p (word spec list)
  "Return non-nil when WORD is in LIST of SPEC or of `tmpl-extra-names'.
LIST is `tags', `filters' or `tests'."
  (or (member word (plist-get spec (intern (format ":%s" list))))
      (member word (alist-get list tmpl-extra-names))))

(defun tmpl--registered (word spec list category)
  "Return (CATEGORY) when WORD is in LIST of SPEC, else (unregistered).
Names come from the engine and `tmpl-extra-names'; see `tmpl--listed-p'."
  (if (tmpl--listed-p word spec list) (list category) '(unregistered)))

(defun tmpl--classify (st word state first)
  "Return (CATEGORY . NEXT-STATE) for WORD read after STATE.
FIRST is non-nil for the first word of a statement.  ST gives the
engine's lists.  Point is just after WORD."
  (let* ((spec (tmpl--state-spec st))
         (keywords (tmpl--fam st :keywords))
         (constants (tmpl--fam st :constants))
         (test-kw (plist-get spec :test-keyword)))
    (cond
     (first (tmpl--registered word spec 'tags 'tag))
     ((eq state 'dot) '(property))
     ((and (eq state 'pipe) (tmpl--fam st :pipe-filters))
      (tmpl--registered word spec 'filters 'filter))
     ((memq state '(is is-not))
      (cond ((and (eq state 'is) (string= word "not")) '(keyword . is-not))
            ((tmpl--listed-p word spec 'tests) '(test))
            ((member word constants) '(constant))
            (t '(unregistered))))
     ((member word keywords)
      (cons 'keyword (and (equal word test-kw) 'is)))
     ((member word constants) '(constant))
     ((member word (plist-get spec :builtins)) '(builtin))
     ((looking-at "[ \t]*(") '(function-call))
     ((tmpl--fam st :bare-calls) '(function-call))
     (t '(variable)))))

(defun tmpl--fontify-inside (st beg end first)
  "Color the inside BEG..END of an expression or statement for ST.
FIRST is non-nil when the first word is a statement tag."
  (let ((strings (tmpl--fam st :strings))
        (fields (tmpl--fam st :fields))
        (cinner (tmpl--fam st :comment-inner))
        (op-re (regexp-opt (tmpl--fam st :operators)))
        state)
    (goto-char beg)
    (while (progn (skip-chars-forward " \t\r\n" end) (< (point) end))
      (let ((pos (point)) (char (char-after)))
        (cond
         ((and cinner (string-prefix-p (car cinner)
                                       (buffer-substring-no-properties
                                        pos (min end (+ pos (length (car cinner)))))))
          (goto-char (if (search-forward (cdr cinner) end t) (point) end))
          (tmpl--put pos (point) 'comment)
          (setq state nil))
         ((memq char strings)
          (tmpl--skip-string char end)
          (tmpl--put pos (point) 'string)
          (setq state nil))
         ((and fields (eq char ?$))
          (forward-char 1)
          (skip-chars-forward "A-Za-z0-9_" end)
          (tmpl--put pos (point) 'variable)
          (setq state nil))
         ((and fields (eq char ?.)
               (not (memq (char-after (1+ pos)) '(?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9))))
          (forward-char 1)
          (skip-chars-forward "A-Za-z0-9_" end)
          (tmpl--put pos (point) 'property)
          (setq state nil))
         ((looking-at tmpl--number-re)
          (goto-char (min end (match-end 0)))
          (tmpl--put pos (point) 'number)
          (setq state nil))
         ((and (not (eq state 'dot))
               (looking-at (concat tmpl--ident-re "\\." tmpl--ident-re))
               (<= (match-end 0) end)
               (member (match-string-no-properties 0)
                       (plist-get (tmpl--state-spec st) :builtins)))
          ;; A dotted builtin such as `dbt.concat'.
          (goto-char (match-end 0))
          (tmpl--put pos (point) 'builtin)
          (setq state nil))
         ((looking-at tmpl--ident-re)
          (goto-char (min end (match-end 0)))
          (let ((class (tmpl--classify st (match-string-no-properties 0) state first)))
            (when (car class)
              (tmpl--put pos (point) (car class)))
            (setq state (cdr class))))
         ((looking-at op-re)
          (goto-char (min end (match-end 0)))
          (tmpl--put pos (point) 'operator)
          (setq state (and (equal (match-string 0) "|") 'pipe)))
         ((eq char ?.)
          (forward-char 1)
          (setq state 'dot))
         (t
          (forward-char 1)
          (setq state nil))))
      (setq first nil))))

(defun tmpl--fontify-delimited (st beg end)
  "Color the delimited region BEG..END of ST, one pair of delimiters."
  (goto-char beg)
  (looking-at (tmpl--state-open-re st))
  (let* ((delim (tmpl--delimiter st (match-string-no-properties 0)))
         (inner (tmpl--inner-start st beg (nth 0 delim)))
         (inner-end (tmpl--inner-end st inner end (nth 1 delim))))
    (tmpl--put beg inner 'delimiter)
    (tmpl--put inner-end end 'delimiter)
    (if (eq (nth 2 delim) 'comment)
        (tmpl--put inner inner-end 'comment)
      (tmpl--fontify-inside st inner inner-end (eq (nth 2 delim) 'stmt)))))

(defun tmpl--fontify (limit)
  "Font-lock matcher: color the template regions from point to LIMIT.
Always returns nil; the faces are set directly."
  (when tmpl--st
    ;; Hosts without syntactic fontification never propertize; symbol
    ;; motion over displayed text needs the region syntax all the same.
    (syntax-propertize limit)
    (let ((case-fold-search nil)
          (st tmpl--st))
      (save-excursion
        (save-match-data
          (dolist (region (tmpl--regions-in st (point) limit))
            (pcase-let ((`(,beg ,end ,kind ,open-end ,close-beg) region))
              (put-text-property beg end 'face 'tmpl-region-face)
              (if (eq kind 'block)
                  (progn
                    (tmpl--fontify-delimited st beg open-end)
                    (tmpl--put open-end close-beg 'comment)
                    (tmpl--fontify-delimited st close-beg end))
                (tmpl--fontify-delimited st beg end))))))))
  nil)

(defun tmpl--extend-region ()
  "Extend the font-lock region to whole template regions."
  (when tmpl--st
    (let ((first (tmpl--region-at tmpl--st font-lock-beg))
          (last (and (> font-lock-end (point-min))
                     (tmpl--region-at tmpl--st (1- font-lock-end))))
          changed)
      (when (and first (< (nth 0 first) font-lock-beg))
        (setq font-lock-beg (nth 0 first) changed t))
      (when (and last (> (nth 1 last) font-lock-end))
        (setq font-lock-end (nth 1 last) changed t))
      changed)))

(defconst tmpl--font-lock-keywords '((tmpl--fontify))
  "Font-lock keywords added by `tmpl-mode'.")

(defun tmpl--add-keywords ()
  "Append `tmpl--font-lock-keywords' to the buffer's font-lock keywords.
Run again from `font-lock-mode-hook': turning font-lock on after it was
off recomputes the keywords from the major mode's defaults."
  (when font-lock-mode
    (font-lock-add-keywords nil tmpl--font-lock-keywords 'append)
    (font-lock-flush)))

;;; Delimiter completion

(defun tmpl--electric-closers (open)
  "Return the closers `electric-pair-mode' may have added for typing OPEN.
They are the matching parens of OPEN's characters, innermost first."
  (if (bound-and-true-p electric-pair-mode)
      (concat (delq nil (mapcar #'matching-paren (reverse (string-to-list open)))))
    ""))

(defun tmpl--complete-delimiter ()
  "Complete an opening delimiter just typed with a space and its closer.
Runs from `post-self-insert-hook' after `electric-pair-mode', whose
closers after point it replaces.  Inside a template region nothing
happens."
  (when-let* ((st tmpl--st)
              (delim (seq-find
                      (lambda (d)
                        (let ((open (car d)))
                          (and (eq (char-before) (aref open (1- (length open))))
                               (<= (+ (point-min) (length open)) (point))
                               (string= open (buffer-substring-no-properties
                                              (- (point) (length open)) (point))))))
                      (tmpl--fam st :delimiters)))
              (start (- (point) (length (car delim))))
              ((not (seq-some (lambda (region) (< (nth 0 region) start))
                              (tmpl--regions-in st start (1+ start))))))
    (let ((closers (tmpl--electric-closers (car delim))))
      ;; `electric-pair-conservative-inhibit' may have paired only some.
      (while (and (> (length closers) 0) (not (looking-at (regexp-quote closers))))
        (setq closers (substring closers 0 -1)))
      (delete-char (length closers)))
    (insert " ")
    (save-excursion
      (insert " " (nth 1 delim)))))

;;; Block matching

(defcustom tmpl-block-match-delay 0.125
  "Idle seconds before the tags of the block at point are highlighted.
The default is that of `show-paren-delay'."
  :type 'number
  :group 'tmpl)

(defface tmpl-block-match-face '((t :inherit show-paren-match))
  "Face for the tags of the block at point."
  :group 'tmpl)

(defvar tmpl--block-timer nil
  "Idle timer running `tmpl-block-match-update', or nil.")

(defvar-local tmpl--block-overlays nil
  "Overlays of the highlighted block tags.")

(defun tmpl--tag-kind (st)
  "Return the delimiter kind whose regions hold statement tags for ST.
Families with statement delimiters keep tags there; others, such as
go-template, in expressions."
  (if (seq-some (lambda (d) (eq (nth 2 d) 'stmt)) (tmpl--fam st :delimiters))
      'stmt
    'expr))

(defun tmpl--block-tag-list (st)
  "Return the tags of ST's statements as (BEG END WORD SINGLE-P), in order.
BEG and END delimit the statement; SINGLE-P is non-nil for an opening
tag whose text marks it as having no end, such as `{% set x = 1 %}'."
  (let ((kind (tmpl--tag-kind st))
        (blocks (plist-get (tmpl--state-spec st) :blocks))
        tags)
    (save-excursion
      (save-match-data
        (dolist (entry (tmpl--entries st))
          (dolist (span (pcase entry
                          (`(,beg ,end block ,open-end ,close-beg)
                           (list (list beg open-end 'stmt) (list close-beg end 'stmt)))
                          (`(,beg ,end ,k) (list (list beg end k)))))
            (pcase-let ((`(,beg ,end ,k) span))
              (when (eq k kind)
                (goto-char beg)
                (looking-at (tmpl--state-open-re st))
                (let* ((word (car (tmpl--tag st beg end (match-string-no-properties 0))))
                       (single (nth 3 (assoc word blocks))))
                  (when word
                    (push (list beg end word
                                (and single
                                     (string-match-p
                                      single (buffer-substring-no-properties beg end))))
                          tags)))))))))
    (nreverse tags)))

(defun tmpl--block-role (st tag)
  "Return (ROLE . BLOCK) for TAG of ST, or nil when it is no block tag.
ROLE is `open', `middle' or `end'; BLOCK is the `:blocks' entry, for a
middle or end tag the first one naming it."
  (let ((blocks (plist-get (tmpl--state-spec st) :blocks))
        (word (nth 2 tag)))
    (cond
     ((assoc word blocks)
      (unless (nth 3 tag) (cons 'open (assoc word blocks))))
     ((seq-find (lambda (b) (equal (nth 2 b) word)) blocks)
      (cons 'end (seq-find (lambda (b) (equal (nth 2 b) word)) blocks)))
     ((seq-find (lambda (b) (member word (nth 1 b))) blocks)
      (cons 'middle (seq-find (lambda (b) (member word (nth 1 b))) blocks))))))

(defun tmpl--block-forward (st tags)
  "Return the tags of the block TAGS starts with, or nil when unclosed.
The first of TAGS opens the block, per the lists of ST.  Nested blocks
are skipped with a stack, so tags ending several kinds of block, as
go-template's `end', match by nesting."
  (let ((stack (list (cdr (tmpl--block-role st (car tags)))))
        (found (list (car tags)))
        done)
    (while (and (setq tags (cdr tags)) (not done) stack)
      (let* ((tag (car tags))
             (role (tmpl--block-role st tag)))
        (pcase (car role)
          ('open (push (cdr role) stack))
          ('end (if (not (equal (nth 2 tag) (nth 2 (car stack))))
                    (setq done 'broken)
                  (pop stack)
                  (unless stack
                    (push tag found)
                    (setq done t))))
          ('middle (when (and (null (cdr stack))
                              (member (nth 2 tag) (nth 1 (car stack))))
                     (push tag found))))))
    (and (eq done t) (nreverse found))))

(defun tmpl--block-opener (st before tag role)
  "Return the tag opening the block of TAG, or nil.
BEFORE lists the tags before TAG, nearest first; ROLE is TAG's role,
`middle' or `end', and ST gives the lists.  Blocks closed in between
are skipped with a stack."
  (let (ends)
    (catch 'opener
      (dolist (other before)
        (let ((block (tmpl--block-role st other)))
          (pcase (car block)
            ('end (push (nth 2 other) ends))
            ('open
             (cond (ends
                    (unless (equal (pop ends) (nth 2 (cdr block)))
                      (throw 'opener nil)))
                   ((if (eq role 'end)
                        (equal (nth 2 tag) (nth 2 (cdr block)))
                      (member (nth 2 tag) (nth 1 (cdr block))))
                    (throw 'opener other))
                   (t (throw 'opener nil)))))))
      nil)))

(defun tmpl--block-at (st pos)
  "Return the tags of the block with a tag at POS in ST's buffer, or nil."
  (let* ((tags (tmpl--block-tag-list st))
         (tail (seq-drop-while (lambda (tag) (<= (nth 1 tag) pos)) tags))
         (tag (car tail))
         (role (and tag (<= (nth 0 tag) pos) (car (tmpl--block-role st tag)))))
    (pcase role
      ('open (tmpl--block-forward st tail))
      ((or 'middle 'end)
       (let* ((before (reverse (seq-take-while (lambda (other) (not (eq other tag))) tags)))
              (opener (tmpl--block-opener st before tag role))
              (block (and opener (tmpl--block-forward st (memq opener tags)))))
         (and (memq tag block) block))))))

(defun tmpl-block-match-update ()
  "Highlight the tags of the block whose tag is at point.
Remove the highlight when point is on no block tag or the block does
not close."
  (mapc #'delete-overlay tmpl--block-overlays)
  (setq tmpl--block-overlays nil)
  (when tmpl--st
    (dolist (tag (tmpl--block-at tmpl--st (point)))
      (let ((overlay (make-overlay (nth 0 tag) (nth 1 tag))))
        (overlay-put overlay 'face 'tmpl-block-match-face)
        (push overlay tmpl--block-overlays)))))

(defun tmpl--block-timer-function ()
  "Update the block highlight in the current buffer when it uses tmpl."
  (when tmpl--st
    (tmpl-block-match-update)))

;;; Minor mode

(defun tmpl--enable ()
  "Set up `tmpl-mode' in the current buffer for `tmpl-engine'."
  (unless (tmpl-extra-names-p tmpl-extra-names)
    (user-error "`tmpl-extra-names' in %s is not an alist of (tags|filters|tests . STRINGS): %S"
                (buffer-name) tmpl-extra-names))
  (let* ((spec (tmpl-engine-spec tmpl-engine))
         (family (plist-get spec :family))
         (st (tmpl--state-create
              :engine tmpl-engine
              :spec spec
              :family family
              :open-re (regexp-opt (mapcar #'car (plist-get family :delimiters)))
              :host-syntax-function syntax-propertize-function
              :plp-local (local-variable-p 'parse-sexp-lookup-properties)
              :plp-value parse-sexp-lookup-properties)))
    (setq tmpl--st st)
    (if syntax-propertize-function
        (add-function :around (local 'syntax-propertize-function) #'tmpl--syntax-propertize)
      (setq-local syntax-propertize-function #'tmpl--syntax-alone))
    (setq-local parse-sexp-lookup-properties t)
    (add-hook 'syntax-propertize-extend-region-functions
              #'syntax-propertize-multiline 'append t)
    (add-hook 'font-lock-extend-region-functions #'tmpl--extend-region nil t)
    (add-hook 'before-change-functions #'tmpl--before-change nil t)
    (add-hook 'after-change-functions #'tmpl--after-change nil t)
    (add-hook 'font-lock-mode-hook #'tmpl--add-keywords nil t)
    ;; After `electric-pair-mode' (depth 50), whose closers it replaces.
    (add-hook 'post-self-insert-hook #'tmpl--complete-delimiter 90 t)
    ;; A new major mode or killing the buffer skips `tmpl-mode' -1.
    (add-hook 'change-major-mode-hook #'tmpl--teardown nil t)
    (add-hook 'kill-buffer-hook #'tmpl--teardown nil t)
    (unless tmpl--block-timer
      (setq tmpl--block-timer
            (run-with-idle-timer tmpl-block-match-delay t #'tmpl--block-timer-function)))
    (font-lock-add-keywords nil tmpl--font-lock-keywords 'append)
    (tmpl--update-treesit-ranges st)
    (syntax-ppss-flush-cache (point-min))
    (font-lock-flush)))

(defun tmpl--disable ()
  "Undo `tmpl--enable' in the current buffer."
  (let ((st tmpl--st))
    (font-lock-remove-keywords nil tmpl--font-lock-keywords)
    (remove-hook 'font-lock-mode-hook #'tmpl--add-keywords t)
    (remove-hook 'post-self-insert-hook #'tmpl--complete-delimiter t)
    (remove-hook 'change-major-mode-hook #'tmpl--teardown t)
    (remove-hook 'kill-buffer-hook #'tmpl--teardown t)
    (mapc #'delete-overlay tmpl--block-overlays)
    (setq tmpl--block-overlays nil)
    (remove-hook 'font-lock-extend-region-functions #'tmpl--extend-region t)
    (remove-hook 'before-change-functions #'tmpl--before-change t)
    (remove-hook 'after-change-functions #'tmpl--after-change t)
    (remove-hook 'syntax-propertize-extend-region-functions
                 #'syntax-propertize-multiline t)
    (if (tmpl--state-host-syntax-function st)
        (remove-function (local 'syntax-propertize-function) #'tmpl--syntax-propertize)
      (kill-local-variable 'syntax-propertize-function))
    (if (tmpl--state-plp-local st)
        (setq-local parse-sexp-lookup-properties (tmpl--state-plp-value st))
      (kill-local-variable 'parse-sexp-lookup-properties))
    (when-let* ((parser (tmpl--treesit-parser)))
      (treesit-parser-set-included-ranges parser nil))
    (setq tmpl--st nil)
    (when (and tmpl--block-timer
               (not (seq-some (lambda (buffer) (buffer-local-value 'tmpl--st buffer))
                              (buffer-list))))
      (cancel-timer tmpl--block-timer)
      (setq tmpl--block-timer nil))
    (with-silent-modifications
      (remove-text-properties (point-min) (point-max)
                              '(syntax-table nil syntax-multiline nil)))
    (syntax-ppss-flush-cache (point-min))
    (font-lock-flush)))

(defvar tmpl-mode)

(defun tmpl--teardown ()
  "Undo `tmpl-mode' before a new major mode or before the buffer dies.
Both reset the buffer without turning the mode off, which would leave
its overlays, syntax properties and idle timer behind."
  (when tmpl--st
    (tmpl--disable)
    (setq tmpl-mode nil)))

;;;###autoload
(define-minor-mode tmpl-mode
  "Color template regions of `tmpl-engine' over the host major mode."
  :group 'tmpl
  :lighter " Tmpl"
  (when tmpl--st
    (tmpl--disable))
  (when tmpl-mode
    (let ((enabled nil))
      (unwind-protect
          (progn
            (unless tmpl-engine
              (user-error "`tmpl-engine' is nil in %s" (buffer-name)))
            (tmpl--enable)
            (setq enabled t))
        (unless enabled
          (setq tmpl-mode nil))))))

(defun tmpl--activate ()
  "Turn `tmpl-mode' on or off per the engine `tmpl-resolve' gives.
Only file buffers are considered: `hack-local-variables-hook' also runs
for directory-local variables of buffers such as Dired's."
  (when buffer-file-name
    (let ((engine (tmpl-resolve)))
      (cond
       (engine
        (setq-local tmpl-engine engine)
        (unless (and tmpl-mode (eq (tmpl--state-engine tmpl--st) engine))
          (when tmpl-mode (tmpl-mode -1))
          (tmpl-mode 1)))
       (t
        ;; An explicit engine set where `tmpl-resolve' overrides it.
        (when tmpl-engine
          (setq-local tmpl-engine nil))
        (when tmpl-mode
          (tmpl-mode -1)))))))

(defvar tmpl--auto-mode-entry nil
  "The `auto-mode-alist' entry `tmpl-global-mode' added, or nil.")

;;;###autoload
(define-minor-mode tmpl-global-mode
  "Enable `tmpl-mode' in file buffers whose engine resolves to non-nil.
The engine is resolved once the file's local variables are in effect.
A file with a template suffix (see `tmpl-suffixes') opens in the mode
of its name without the suffix."
  :global t
  :group 'tmpl
  (when tmpl--auto-mode-entry
    (setq auto-mode-alist (delq tmpl--auto-mode-entry auto-mode-alist)
          tmpl--auto-mode-entry nil))
  (if tmpl-global-mode
      (progn
        ;; (REGEXP nil t): strip the match and look the rest up again.
        (setq tmpl--auto-mode-entry
              (list (concat "\\." (regexp-opt (tmpl-suffixes)) "\\'") nil t))
        (push tmpl--auto-mode-entry auto-mode-alist)
        (add-hook 'hack-local-variables-hook #'tmpl--activate))
    (remove-hook 'hack-local-variables-hook #'tmpl--activate)))

;;; dumb-jump

(defconst tmpl-dumb-jump-language "tmpl"
  "The dumb-jump language given to template extensions it has no language for.")

(defun tmpl--dumb-jump-languages (ext)
  "Return the dumb-jump languages assigned to extension EXT."
  (delete-dups
   (mapcar (lambda (entry) (plist-get entry :language))
           (seq-filter (lambda (entry) (equal (plist-get entry :ext) ext))
                       dumb-jump-language-file-exts))))

(defun tmpl--all-rule-extensions ()
  "Return the extensions of all rules in `tmpl-rules'."
  (delete-dups (mapcan (lambda (rule) (copy-sequence (tmpl-rule-extensions rule)))
                       tmpl-rules)))

(defun tmpl-dumb-jump-rules ()
  "Return the dumb-jump rules for the definitions of registered engines.
Each rule is attached to the languages dumb-jump gives the extensions of
the engine's detection rules, or to `tmpl-dumb-jump-language' for an
extension with none.  Nothing is changed."
  (let (rules)
    (dolist (rule tmpl-rules)
      (let* ((spec (tmpl-engine-spec (plist-get rule :engine)))
             (defs (append (plist-get (plist-get spec :family) :definitions)
                           (plist-get spec :definitions))))
        (dolist (ext (tmpl-rule-extensions rule))
          (let ((langs (or (tmpl--dumb-jump-languages ext)
                           (list tmpl-dumb-jump-language))))
            (dolist (lang langs)
              (dolist (def defs)
                (cl-pushnew (list :language lang :type (car def)
                                  :supports '("ag" "rg")
                                  :regex (cadr def))
                            rules :test #'equal)))))))
    (nreverse rules)))

(defun tmpl-dumb-jump-install ()
  "Register template extensions and definition rules with dumb-jump.
Extensions dumb-jump has no language for get `tmpl-dumb-jump-language'
in `dumb-jump-language-file-exts'; the rules of `tmpl-dumb-jump-rules'
are appended to `dumb-jump-find-rules'."
  (let ((rules (tmpl-dumb-jump-rules)))
    (dolist (ext (tmpl--all-rule-extensions))
      (unless (tmpl--dumb-jump-languages ext)
        (add-to-list 'dumb-jump-language-file-exts
                     (list :language tmpl-dumb-jump-language :ext ext
                           :agtype nil :rgtype nil)
                     t)))
    (dolist (rule rules)
      (add-to-list 'dumb-jump-find-rules rule t))))

(with-eval-after-load 'dumb-jump
  (tmpl-dumb-jump-install))

(provide 'tmpl)
;;; tmpl.el ends here
