;;; copilot-commit-core.el --- Pure functions for copilot-commit -*- lexical-binding: t; -*-

;;; Commentary:
;; Pure computation layer for copilot-commit.
;; Zero external dependencies -- no `(require 'copilot)'.

;;; Code:

;;; Customization

(defgroup copilot-commit nil
  "Generate commit messages via Copilot LSP."
  :prefix "copilot-commit-"
  :group 'copilot)

(defcustom copilot-commit-chunk-threshold 448800
  "Character count threshold for chunked diff processing.
When the staged diff exceeds this value, it is split into chunks,
each summarized separately before generating the final commit message.
This is the fallback value when the model's token limit is unknown.
Derived from 128k token model: (floor(128000 * 0.9) - 3000) * 4 = 448800."
  :type 'integer
  :group 'copilot-commit)

(defcustom copilot-commit-cache-ttl (* 48 3600)
  "TTL in seconds for cached model token limits.
When a cache entry is older than this value, it is considered expired.
The stale value is used as fallback while a probe refreshes the cache."
  :type 'integer
  :group 'copilot-commit)

(defcustom copilot-commit-chunk-concurrency 3
  "Maximum number of concurrent chunk summarize requests."
  :type 'integer
  :group 'copilot-commit)

(defcustom copilot-commit-prompt
  "Generate a Git commit message following Conventional Commits v1.0.0.

Format: <type>[optional scope]: <description>

Types: feat, fix, build, chore, ci, docs, perf, refactor, style, test
Breaking changes: append ! before : or add BREAKING CHANGE: footer.

Rules:
- Summary line: imperative, present tense, <=72 chars, no trailing period
- Body (optional): one blank line after summary, wrap at 72 chars, use bullet list with -
- Every point MUST describe a concrete change visible in the diff; NEVER add reasons, purposes, or motivations (e.g. \"improve readability\", \"maintain consistency\") unless explicitly stated in code comments or documentation
"
  "System prompt for commit message generation."
  :type 'string
  :group 'copilot-commit)

(defcustom copilot-commit-prompt-suffix ""
  "Suffix appended to every commit message request.
Can be a string or a function that returns a string.
Useful for adding language instructions or other rules."
  :type '(choice (string :tag "Static suffix")
                 (function :tag "Dynamic suffix function"))
  :group 'copilot-commit)

(defconst copilot-commit--output-contract
  "

<reminder>
ONLY return the commit message in a single markdown code block, NO OTHER PROSE!
If the message contains a code block, fence the outer block with more backticks.
```text
commit message goes here
```
</reminder>"
  "Output format appended to every commit message request.
`copilot-commit--extract-message' parses replies by this format, so it
is kept out of the customizable prompt.")

;;; Buffer helpers

(defun copilot-commit--input-region-end ()
  "Return the position of the end of the user input region.
This is the position just before the first line starting with `#'
\(the template comment block).  If no `#' line exists, return `point-max'."
  (save-excursion
    (goto-char (point-min))
    (if (re-search-forward "^#" nil t)
        (line-beginning-position)
      (point-max))))

(defun copilot-commit--common-affixes (old new)
  "Return (PREFIX . SUFFIX), the common prefix and suffix lengths of OLD and NEW.
The suffix never overlaps the prefix, so replacing the middle of OLD
with the middle of NEW turns OLD into NEW."
  (let* ((mismatch (compare-strings old nil nil new nil nil))
         (prefix (if (eq mismatch t) (length old) (1- (abs mismatch))))
         (limit (- (min (length old) (length new)) prefix))
         (suffix 0))
    (while (and (< suffix limit)
                (eq (aref old (- (length old) suffix 1))
                    (aref new (- (length new) suffix 1))))
      (setq suffix (1+ suffix)))
    (cons prefix suffix)))

;;; Prompt construction

(defun copilot-commit--prompt-suffix ()
  "Return the effective prompt suffix string."
  (if (functionp copilot-commit-prompt-suffix)
      (funcall copilot-commit-prompt-suffix)
    (or copilot-commit-prompt-suffix "")))

(defun copilot-commit--request-tail ()
  "Return the text ending every commit message request.
The output contract precedes the prompt suffix so that language
instructions in the suffix stay the last thing the model reads."
  (concat copilot-commit--output-contract (copilot-commit--prompt-suffix)))

(defun copilot-commit--build-prompt (diff status)
  "Build the full prompt from system prompt, DIFF and STATUS."
  (concat copilot-commit-prompt
          "\n\n"
          "<git_context>\n"
          "# Git Status Summary:\n"
          (if (string-empty-p status) ""
            (replace-regexp-in-string "^" "# " status))
          "\n\n<git_diff>\n"
          diff "\n"
          "</git_diff>\n"
          "</git_context>"
          (copilot-commit--request-tail)))

(defun copilot-commit--build-regenerate-message (instruction)
  "Build the regenerate request from user INSTRUCTION."
  (concat instruction (copilot-commit--request-tail)))

;;; Reply parsing

(defun copilot-commit--parse-text-block (reply)
  "Parse the text code block of REPLY into (CONTENT . CLOSED).
The block opens at the first line of three or more backticks followed
by the info string `text' and closes at the last line consisting of
exactly those backticks, so code blocks elsewhere in the reply do not
open it and code blocks nested in the message body survive.  Without a
closing line CONTENT runs to the end of REPLY and CLOSED is nil.
Return nil when the opening line has not arrived."
  (let ((case-fold-search nil))
    (when (string-match "^\\(`\\{3,\\}\\)text[ \t]*\n" reply)
      (let* ((start (match-end 0))
             (close-re (concat "^" (match-string 1 reply) "[ \t]*$"))
             (pos start)
             end)
        (while (string-match close-re reply pos)
          (setq end (match-beginning 0)
                pos (match-end 0)))
        (cons (substring reply start end) (and end t))))))

(defun copilot-commit--nonblank (text)
  "Return TEXT trimmed, or nil when it is blank."
  (let ((trimmed (string-trim text)))
    (unless (string-empty-p trimmed)
      trimmed)))

(defun copilot-commit--fences-closed-p (text)
  "Return non-nil when every code block opened in TEXT is closed.
A block opens at a line starting with three or more backticks and
closes at the next line consisting of exactly those backticks."
  (let ((case-fold-search nil)
        (open nil))
    (dolist (line (split-string text "\n"))
      (cond
       ((and open (string-match-p (concat "\\`" open "[ \t]*\\'") line))
        (setq open nil))
       ((and (not open) (string-match "\\`\\(`\\{3,\\}\\)" line))
        (setq open (match-string 1 line)))))
    (not open)))

(defun copilot-commit--extract-message (reply)
  "Return the commit message inside the closed text code block of REPLY.
Return nil when no closed block exists, its content is blank, or a code
block inside the content is left open."
  (pcase (copilot-commit--parse-text-block reply)
    (`(,content . t)
     (when (copilot-commit--fences-closed-p content)
       (copilot-commit--nonblank content)))))

(defun copilot-commit--streaming-message (reply)
  "Return the part of the commit message streamed so far in REPLY, or nil.
A trailing line of bare backticks, a closing line still arriving, is
dropped.  Return nil until the text code block has non-blank content."
  (when-let* ((block (copilot-commit--parse-text-block reply)))
    (copilot-commit--nonblank
     (string-trim-right (car block) "\\(?:\\`\\|\n\\)`+"))))

;;; Diff splitting

(defun copilot-commit--split-diff (diff threshold)
  "Split DIFF into chunks, each at most THRESHOLD characters.
Splits by file boundaries (diff --git), then by hunk boundaries
\(@@) if a single file diff exceeds THRESHOLD.  Uses greedy merging."
  (let ((file-diffs (copilot-commit--split-by-file diff)))
    (copilot-commit--greedy-merge file-diffs threshold)))

(defun copilot-commit--split-by-file (diff)
  "Split DIFF into a list of per-file diffs.
Each element starts with `diff --git'."
  (let ((parts '())
        (start 0))
    ;; Find subsequent "diff --git" at line beginnings (preceded by newline)
    (while (string-match "\ndiff --git " diff start)
      (let ((pos (1+ (match-beginning 0)))) ; skip the \n, point at "diff"
        (let ((segment (substring diff start pos)))
          (unless (string-empty-p (string-trim segment))
            (push segment parts)))
        (setq start pos)))
    (when (< start (length diff))
      (let ((segment (substring diff start)))
        (unless (string-empty-p (string-trim segment))
          (push segment parts))))
    (nreverse parts)))

(defun copilot-commit--split-by-hunk (file-diff)
  "Split FILE-DIFF into sub-chunks at hunk boundaries (@@).
The header (everything before the first @@) is prepended to each sub-chunk."
  (cond
   ;; Starts with @@ (no header)
   ((string-match "\\`@@" file-diff)
    (let ((hunks '())
          (pos 0))
      (while (string-match "\n@@" file-diff (1+ pos))
        (let ((hunk-end (1+ (match-beginning 0))))
          (push (substring file-diff pos hunk-end) hunks)
          (setq pos hunk-end)))
      (when (< pos (length file-diff))
        (push (substring file-diff pos) hunks))
      (nreverse hunks)))
   ;; Has header before first @@
   ((string-match "\n@@" file-diff)
    (let ((header (substring file-diff 0 (1+ (match-beginning 0))))
          (hunks '())
          (start (1+ (match-beginning 0))))
      (let ((pos start))
        (while (string-match "\n@@" file-diff (1+ pos))
          (let ((hunk-end (1+ (match-beginning 0))))
            (push (substring file-diff pos hunk-end) hunks)
            (setq pos hunk-end)))
        (when (< pos (length file-diff))
          (push (substring file-diff pos) hunks)))
      (mapcar (lambda (hunk) (concat header hunk))
              (nreverse hunks))))
   ;; No hunks found
   (t (list file-diff))))

(defun copilot-commit--greedy-merge (items threshold)
  "Greedily merge ITEMS into chunks, each at most THRESHOLD characters.
If a single item exceeds THRESHOLD, it is split by hunk boundaries."
  (let ((chunks '())
        (current ""))
    (dolist (item items)
      (cond
       ;; Single item exceeds threshold: sub-split by hunk
       ((> (length item) threshold)
        ;; Flush current chunk first
        (unless (string-empty-p (string-trim current))
          (push current chunks)
          (setq current ""))
        ;; Sub-split and merge hunks
        (let ((sub-items (copilot-commit--split-by-hunk item)))
          (dolist (sub sub-items)
            (if (or (string-empty-p current)
                    (<= (+ (length current) (length sub)) threshold))
                (setq current (concat current sub))
              (push current chunks)
              (setq current sub)))))
       ;; Adding item would exceed threshold: start new chunk
       ((and (not (string-empty-p (string-trim current)))
             (> (+ (length current) (length item)) threshold))
        (push current chunks)
        (setq current item))
       ;; Accumulate into current chunk
       (t
        (setq current (concat current item)))))
    ;; Flush remaining
    (unless (string-empty-p (string-trim current))
      (push current chunks))
    (nreverse chunks)))

;;; Chunked prompt construction

(defun copilot-commit--build-chunk-prompt (chunk index total)
  "Build summarize prompt for CHUNK at INDEX (0-based) of TOTAL chunks."
  (format "This is part %d of %d of a large git diff.
Summarize ONLY the changes in this part concisely as bullet points (one per file or logical change).
Keep your summary under 200 words. Do not generate a commit message.

<git_diff>
%s
</git_diff>" (1+ index) total chunk))

(defun copilot-commit--build-final-prompt (summaries status)
  "Build final commit message prompt from SUMMARIES and STATUS."
  (let ((summary-text (mapconcat #'identity summaries "\n\n")))
    (concat copilot-commit-prompt
            "\n\n"
            (format "The following are summaries of ALL changes in this commit, analyzed in %d parts:\n\n"
                    (length summaries))
            summary-text
            "\n\n"
            "<git_context>\n"
            "# Git Status Summary:\n"
            (if (string-empty-p status) ""
              (replace-regexp-in-string "^" "# " status))
            "\n</git_context>"
            (copilot-commit--request-tail))))

;;; Conversation turns

(defun copilot-commit--build-turns (history new-message)
  "Build turns vector from HISTORY and NEW-MESSAGE.
HISTORY is a list of (REQUEST . RESPONSE) pairs, oldest first when reversed.
NEW-MESSAGE is the new request to append."
  (let ((turns '()))
    ;; Add history turns (oldest first)
    (dolist (pair (reverse history))
      (push (list :request (car pair) :response (cdr pair) :turnId "") turns))
    ;; Add the new request
    (push (list :request new-message :response "" :turnId "") turns)
    (vconcat (nreverse turns))))

;;; Dynamic threshold

(defun copilot-commit--compute-chunk-threshold (max-tokens)
  "Compute chunk threshold in characters from MAX-TOKENS."
  (let* ((usable (floor (* max-tokens 0.9)))
         (for-chunk (- usable 3000)))
    (max 10000 (* for-chunk 4))))

(provide 'copilot-commit-core)
;;; copilot-commit-core.el ends here
