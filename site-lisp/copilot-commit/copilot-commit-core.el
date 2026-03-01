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
- Output ONLY the commit message text, no code fences or commentary
"
  "System prompt for commit message generation."
  :type 'string
  :group 'copilot-commit)

(defcustom copilot-commit-prompt-suffix ""
  "Suffix appended to the commit message prompt.
Can be a string or a function that returns a string.
Useful for adding language instructions or other rules."
  :type '(choice (string :tag "Static suffix")
                 (function :tag "Dynamic suffix function"))
  :group 'copilot-commit)

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

;;; Prompt construction

(defun copilot-commit--prompt-suffix ()
  "Return the effective prompt suffix string."
  (if (functionp copilot-commit-prompt-suffix)
      (funcall copilot-commit-prompt-suffix)
    (or copilot-commit-prompt-suffix "")))

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
          (copilot-commit--prompt-suffix)))

;;; Diff splitting

(defun copilot-commit--split-diff (diff threshold)
  "Split DIFF into chunks, each at most THRESHOLD characters.
Splits by file boundaries (diff --git), then by hunk boundaries
(@@) if a single file diff exceeds THRESHOLD.  Uses greedy merging."
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
            (copilot-commit--prompt-suffix))))

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
