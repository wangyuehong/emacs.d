;;; copilot-commit-test.el --- Tests for copilot-commit -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for copilot-commit-core (pure functions) and
;; copilot-commit (IO layer with mocks).

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'copilot-commit-core)
(require 'copilot-commit)

;;; Test data helpers

(defun cc-test--make-file-diff (filename &optional content)
  "Synthesize a diff for FILENAME with optional CONTENT."
  (let ((body (or content
                  (format "+added line in %s\n-removed line in %s\n"
                          filename filename))))
    (format "diff --git a/%s b/%s\nindex 1234567..abcdefg 100644\n--- a/%s\n+++ b/%s\n@@ -1,3 +1,3 @@\n%s"
            filename filename filename filename body)))

(defun cc-test--make-large-content (size)
  "Generate content of approximately SIZE characters."
  (let ((line "+this is a line of content for testing purposes only\n"))
    (let ((repeats (max 1 (/ size (length line)))))
      (apply #'concat (make-list repeats line)))))

(defun cc-test--make-large-file-diff (filename size num-hunks)
  "Generate a file diff for FILENAME of approximately SIZE chars with NUM-HUNKS hunks."
  (let* ((header (format "diff --git a/%s b/%s\nindex 1234567..abcdefg 100644\n--- a/%s\n+++ b/%s\n"
                         filename filename filename filename))
         (hunk-size (max 1 (/ (- size (length header)) num-hunks)))
         (hunks '()))
    (dotimes (i num-hunks)
      (let* ((hunk-header (format "@@ -%d,10 +%d,10 @@\n" (* i 10) (* i 10)))
             (body (cc-test--make-large-content (- hunk-size (length hunk-header)))))
        (push (concat hunk-header body) hunks)))
    (concat header (mapconcat #'identity (nreverse hunks) ""))))

;; ---------------------------------------------------------------------------
;;; split-by-file tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-split-by-file/single-file ()
  (let ((diff (cc-test--make-file-diff "foo.el")))
    (should (equal (length (copilot-commit--split-by-file diff)) 1))))

(ert-deftest cc-test-split-by-file/two-files ()
  (let* ((d1 (cc-test--make-file-diff "a.el"))
         (d2 (cc-test--make-file-diff "b.el"))
         (diff (concat d1 "\n" d2))
         (parts (copilot-commit--split-by-file diff)))
    (should (= (length parts) 2))
    (should (string-prefix-p "diff --git" (car parts)))
    (should (string-prefix-p "diff --git" (cadr parts)))))

(ert-deftest cc-test-split-by-file/three-files ()
  (let* ((diff (concat (cc-test--make-file-diff "a.el") "\n"
                       (cc-test--make-file-diff "b.el") "\n"
                       (cc-test--make-file-diff "c.el")))
         (parts (copilot-commit--split-by-file diff)))
    (should (= (length parts) 3))))

(ert-deftest cc-test-split-by-file/empty-string ()
  (should (null (copilot-commit--split-by-file ""))))

(ert-deftest cc-test-split-by-file/no-diff-header ()
  (let ((text "some random text without diff header"))
    (should (equal (copilot-commit--split-by-file text) (list text)))))

(ert-deftest cc-test-split-by-file/preserves-content ()
  (let* ((d1 (cc-test--make-file-diff "a.el"))
         (d2 (cc-test--make-file-diff "b.el"))
         (diff (concat d1 "\n" d2))
         (parts (copilot-commit--split-by-file diff)))
    (should (string= (apply #'concat parts) diff))))

(ert-deftest cc-test-split-by-file/trailing-newline ()
  (let* ((diff (concat (cc-test--make-file-diff "a.el") "\n\n"))
         (parts (copilot-commit--split-by-file diff)))
    (should (= (length parts) 1))
    (dolist (p parts)
      (should-not (string-empty-p (string-trim p))))))

(ert-deftest cc-test-split-by-file/diff-git-in-content ()
  (let* ((content "+line contains diff --git a/x b/x in middle\n")
         (diff (cc-test--make-file-diff "a.el" content))
         (parts (copilot-commit--split-by-file diff)))
    (should (= (length parts) 1))))

;; ---------------------------------------------------------------------------
;;; split-by-hunk tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-split-by-hunk/single-hunk ()
  (let ((diff (cc-test--make-file-diff "a.el")))
    (should (= (length (copilot-commit--split-by-hunk diff)) 1))))

(ert-deftest cc-test-split-by-hunk/two-hunks ()
  (let* ((diff (concat "diff --git a/f b/f\n--- a/f\n+++ b/f\n"
                        "@@ -1,3 +1,3 @@\n+line1\n"
                        "@@ -10,3 +10,3 @@\n+line2\n"))
         (parts (copilot-commit--split-by-hunk diff)))
    (should (= (length parts) 2))
    ;; Each part should have the header
    (dolist (p parts)
      (should (string-prefix-p "diff --git" p)))))

(ert-deftest cc-test-split-by-hunk/three-hunks ()
  (let* ((diff (concat "diff --git a/f b/f\n--- a/f\n+++ b/f\n"
                        "@@ -1,3 +1,3 @@\n+line1\n"
                        "@@ -10,3 +10,3 @@\n+line2\n"
                        "@@ -20,3 +20,3 @@\n+line3\n"))
         (parts (copilot-commit--split-by-hunk diff)))
    (should (= (length parts) 3))))

(ert-deftest cc-test-split-by-hunk/no-header ()
  (let* ((diff "@@ -1,3 +1,3 @@\n+line1\n@@ -10,3 +10,3 @@\n+line2\n")
         (parts (copilot-commit--split-by-hunk diff)))
    (should (= (length parts) 2))
    (should (string-prefix-p "@@" (car parts)))))

(ert-deftest cc-test-split-by-hunk/no-hunks ()
  (let ((diff "just some text without any hunk markers"))
    (should (equal (copilot-commit--split-by-hunk diff)
                   (list diff)))))

(ert-deftest cc-test-split-by-hunk/header-preserved ()
  (let* ((header "diff --git a/f b/f\n--- a/f\n+++ b/f\n")
         (diff (concat header
                       "@@ -1,3 +1,3 @@\n+line1\n"
                       "@@ -10,3 +10,3 @@\n+line2\n"))
         (parts (copilot-commit--split-by-hunk diff)))
    (dolist (p parts)
      (should (string-prefix-p header p)))))

(ert-deftest cc-test-split-by-hunk/content-integrity ()
  (let* ((header "diff --git a/f b/f\n--- a/f\n+++ b/f\n")
         (hunk1 "@@ -1,3 +1,3 @@\n+line1\n")
         (hunk2 "@@ -10,3 +10,3 @@\n+line2\n")
         (diff (concat header hunk1 hunk2))
         (parts (copilot-commit--split-by-hunk diff))
         ;; Strip header from each part and concat
         (bodies (mapconcat
                  (lambda (p) (substring p (length header)))
                  parts "")))
    (should (string= bodies (concat hunk1 hunk2)))))

(ert-deftest cc-test-split-by-hunk/at-sign-in-content ()
  (let* ((diff (concat "diff --git a/f b/f\n--- a/f\n+++ b/f\n"
                        "@@ -1,3 +1,3 @@\n"
                        "+line with @@ inside content\n"
                        "+another line\n"))
         (parts (copilot-commit--split-by-hunk diff)))
    ;; @@ not at line beginning should not split
    (should (= (length parts) 1))))

;; ---------------------------------------------------------------------------
;;; greedy-merge tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-greedy-merge/all-fit-in-one ()
  (let ((items (list "aaa" "bbb" "ccc")))
    (should (= (length (copilot-commit--greedy-merge items 100)) 1))))

(ert-deftest cc-test-greedy-merge/exact-threshold ()
  (let ((items (list "aaaaa" "bbbbb"))) ; 10 chars total
    (should (= (length (copilot-commit--greedy-merge items 10)) 1))))

(ert-deftest cc-test-greedy-merge/split-at-boundary ()
  (let* ((items (list "aaaaa" "bbbbb" "ccccc")) ; 5+5+5=15
         (chunks (copilot-commit--greedy-merge items 10)))
    (should (= (length chunks) 2))
    (should (string= (car chunks) "aaaaabbbbb"))
    (should (string= (cadr chunks) "ccccc"))))

(ert-deftest cc-test-greedy-merge/each-item-one-chunk ()
  (let* ((items (list "aaaaaaaaaa" "bbbbbbbbbb")) ; each 10
         (chunks (copilot-commit--greedy-merge items 10)))
    ;; First fits (empty + 10 = 10), second would be 20 > 10
    (should (= (length chunks) 2))))

(ert-deftest cc-test-greedy-merge/single-oversized-multi-hunk ()
  (let* ((big (cc-test--make-large-file-diff "big.el" 200 4))
         (items (list big))
         (chunks (copilot-commit--greedy-merge items 100)))
    ;; Should be split into multiple chunks via hunk splitting
    (should (> (length chunks) 1))))

(ert-deftest cc-test-greedy-merge/single-oversized-single-hunk ()
  (let* ((big (cc-test--make-large-file-diff "big.el" 200 1))
         (items (list big))
         (chunks (copilot-commit--greedy-merge items 100)))
    ;; Single hunk, cannot split further, stays as 1 chunk
    (should (= (length chunks) 1))))

(ert-deftest cc-test-greedy-merge/mixed-sizes ()
  (let* ((small1 "aaa")
         (big (cc-test--make-large-file-diff "big.el" 200 4))
         (small2 "bbb")
         (items (list small1 big small2))
         (chunks (copilot-commit--greedy-merge items 100)))
    ;; small1 flushed before big, big sub-split, small2 appended or new chunk
    (should (>= (length chunks) 3))))

(ert-deftest cc-test-greedy-merge/empty-list ()
  (should (null (copilot-commit--greedy-merge nil 100))))

(ert-deftest cc-test-greedy-merge/single-small-item ()
  (should (= (length (copilot-commit--greedy-merge (list "abc") 100)) 1)))

(ert-deftest cc-test-greedy-merge/flush-before-oversized ()
  (let* ((small "aaaa")
         (big (cc-test--make-large-file-diff "big.el" 200 3))
         (items (list small big))
         (chunks (copilot-commit--greedy-merge items 100)))
    ;; small should be flushed as its own chunk before big is sub-split
    (should (string= (car chunks) small))
    (should (> (length chunks) 2))))

;; ---------------------------------------------------------------------------
;;; split-diff integration tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-split-diff/small-no-split ()
  (let ((diff (cc-test--make-file-diff "small.el")))
    (should (= (length (copilot-commit--split-diff diff 50000)) 1))))

(ert-deftest cc-test-split-diff/multi-file-split ()
  (let* ((diffs (mapcar (lambda (i)
                          (cc-test--make-file-diff
                           (format "file%d.el" i)
                           (cc-test--make-large-content 300)))
                        '(1 2 3 4)))
         (diff (mapconcat #'identity diffs "\n"))
         (chunks (copilot-commit--split-diff diff 500)))
    (should (> (length chunks) 1))))

(ert-deftest cc-test-split-diff/single-large-file-multi-hunk ()
  (let* ((diff (cc-test--make-large-file-diff "big.el" 1000 5))
         (chunks (copilot-commit--split-diff diff 300)))
    (should (> (length chunks) 1))))

(ert-deftest cc-test-split-diff/single-file-single-hunk-oversized ()
  (let* ((diff (cc-test--make-large-file-diff "huge.el" 500 1))
         (chunks (copilot-commit--split-diff diff 100)))
    ;; Cannot split further, returns as single chunk
    (should (= (length chunks) 1))))

(ert-deftest cc-test-split-diff/empty-diff ()
  (should (null (copilot-commit--split-diff "" 1000))))

(ert-deftest cc-test-split-diff/all-chunks-have-diff-header ()
  (let* ((diff (cc-test--make-large-file-diff "big.el" 1000 5))
         (chunks (copilot-commit--split-diff diff 300)))
    (dolist (chunk chunks)
      (should (string-prefix-p "diff --git" chunk)))))

(ert-deftest cc-test-split-diff/realistic-diff ()
  (let* ((diffs (list (cc-test--make-file-diff "src/main.el"
                        (cc-test--make-large-content 200))
                      (cc-test--make-file-diff "src/utils.el"
                        (cc-test--make-large-content 150))
                      (cc-test--make-file-diff "test/main-test.el"
                        (cc-test--make-large-content 300))
                      (cc-test--make-file-diff "README.md"
                        (cc-test--make-large-content 100))))
         (diff (mapconcat #'identity diffs "\n"))
         (chunks (copilot-commit--split-diff diff 400)))
    (should (> (length chunks) 1))
    (dolist (chunk chunks)
      (should (string-prefix-p "diff --git" chunk)))))

;; ---------------------------------------------------------------------------
;;; build-prompt tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-build-prompt/contains-all-parts ()
  (let ((copilot-commit-prompt "test prompt")
        (copilot-commit-prompt-suffix ""))
    (let ((result (copilot-commit--build-prompt "diff content" "M file.el")))
      (should (string-match-p "test prompt" result))
      (should (string-match-p "<git_context>" result))
      (should (string-match-p "<git_diff>" result))
      (should (string-match-p "diff content" result)))))

(ert-deftest cc-test-build-prompt/empty-status ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix ""))
    (let ((result (copilot-commit--build-prompt "diff" "")))
      ;; Should not have "# " prefixed status lines
      (should-not (string-match-p "^# M " result)))))

(ert-deftest cc-test-build-prompt/suffix-string ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix "\n\nWrite in Japanese."))
    (let ((result (copilot-commit--build-prompt "diff" "status")))
      (should (string-match-p "Write in Japanese\\." result)))))

(ert-deftest cc-test-build-prompt/suffix-function ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix (lambda () "\n\nWrite in Chinese.")))
    (let ((result (copilot-commit--build-prompt "diff" "status")))
      (should (string-match-p "Write in Chinese\\." result)))))

(ert-deftest cc-test-build-prompt/suffix-empty ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix ""))
    (let ((result (copilot-commit--build-prompt "diff" "status")))
      (should (string-suffix-p "</git_context>" result)))))

;; ---------------------------------------------------------------------------
;;; build-chunk-prompt tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-build-chunk-prompt/first-chunk ()
  (let ((result (copilot-commit--build-chunk-prompt "chunk" 0 3)))
    (should (string-match-p "part 1 of 3" result))))

(ert-deftest cc-test-build-chunk-prompt/last-chunk ()
  (let ((result (copilot-commit--build-chunk-prompt "chunk" 2 3)))
    (should (string-match-p "part 3 of 3" result))))

(ert-deftest cc-test-build-chunk-prompt/contains-diff ()
  (let ((result (copilot-commit--build-chunk-prompt "my diff content" 0 1)))
    (should (string-match-p "<git_diff>" result))
    (should (string-match-p "my diff content" result))))

;; ---------------------------------------------------------------------------
;;; build-final-prompt tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-build-final-prompt/contains-summaries ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix ""))
    (let ((result (copilot-commit--build-final-prompt
                   '("summary A" "summary B") "status")))
      (should (string-match-p "summary A" result))
      (should (string-match-p "summary B" result)))))

(ert-deftest cc-test-build-final-prompt/contains-status ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix ""))
    (let ((result (copilot-commit--build-final-prompt
                   '("sum") "M file.el")))
      (should (string-match-p "<git_context>" result))
      (should (string-match-p "# M file.el" result)))))

(ert-deftest cc-test-build-final-prompt/prompt-suffix ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix "\n\nWrite in Japanese."))
    (let ((result (copilot-commit--build-final-prompt '("sum") "")))
      (should (string-match-p "Write in Japanese\\." result)))))

(ert-deftest cc-test-build-final-prompt/part-count ()
  (let ((copilot-commit-prompt "prompt")
        (copilot-commit-prompt-suffix ""))
    (let ((result (copilot-commit--build-final-prompt
                   '("a" "b" "c") "")))
      (should (string-match-p "3 parts" result)))))

;; ---------------------------------------------------------------------------
;;; build-turns tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-build-turns/no-history ()
  (let ((turns (copilot-commit--build-turns nil "hello")))
    (should (vectorp turns))
    (should (= (length turns) 1))
    (should (equal (plist-get (aref turns 0) :request) "hello"))))

(ert-deftest cc-test-build-turns/with-history ()
  ;; History is stored most-recent-first; build-turns reverses to oldest-first
  (let ((turns (copilot-commit--build-turns
                '(("req2" . "resp2") ("req1" . "resp1")) "new")))
    (should (= (length turns) 3))
    ;; Oldest first after reverse
    (should (equal (plist-get (aref turns 0) :request) "req1"))
    (should (equal (plist-get (aref turns 1) :request) "req2"))
    (should (equal (plist-get (aref turns 2) :request) "new"))))

(ert-deftest cc-test-build-turns/returns-vector ()
  (let ((turns (copilot-commit--build-turns nil "msg")))
    (should (vectorp turns))))

(ert-deftest cc-test-build-turns/new-message-last ()
  (let* ((turns (copilot-commit--build-turns '(("r" . "s")) "final"))
         (last-turn (aref turns (1- (length turns)))))
    (should (equal (plist-get last-turn :request) "final"))
    (should (equal (plist-get last-turn :response) ""))))

;; ---------------------------------------------------------------------------
;;; input-region-end tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-input-region-end/with-comment ()
  (with-temp-buffer
    (insert "message text\n\n# comment line\n# another\n")
    (let ((pos (copilot-commit--input-region-end)))
      (should (= pos (save-excursion
                       (goto-char (point-min))
                       (re-search-forward "^#")
                       (line-beginning-position)))))))

(ert-deftest cc-test-input-region-end/no-comment ()
  (with-temp-buffer
    (insert "message text\nno hash lines\n")
    (should (= (copilot-commit--input-region-end) (point-max)))))

(ert-deftest cc-test-input-region-end/comment-at-start ()
  (with-temp-buffer
    (insert "# first line is comment\nmore\n")
    (should (= (copilot-commit--input-region-end) (point-min)))))

(ert-deftest cc-test-input-region-end/multiple-comments ()
  (with-temp-buffer
    (insert "text\n# first comment\n# second comment\n")
    (let ((pos (copilot-commit--input-region-end)))
      ;; Should return the position of the FIRST # line
      (goto-char pos)
      (should (looking-at "# first comment")))))

;; ---------------------------------------------------------------------------
;;; handle-progress-1 state machine tests (IO/mock)
;; ---------------------------------------------------------------------------

(defmacro cc-test--with-buf (&rest body)
  "Run BODY with a temporary buffer bound to `buf', cleaned up afterwards."
  (declare (indent 0))
  `(let ((buf (generate-new-buffer " *cc-test*")))
     (unwind-protect
         (progn ,@body)
       (when (buffer-live-p buf)
         (kill-buffer buf)))))

(defmacro cc-test--with-progress-buf (&rest body)
  "Set up a temporary buffer with copilot-commit buffer-local state, run BODY."
  (declare (indent 0))
  `(cc-test--with-buf
     (with-current-buffer buf
       (setq copilot-commit--streaming-p nil
             copilot-commit--accumulated nil
             copilot-commit--history nil
             copilot-commit--chunks nil
             copilot-commit--chunk-index 0
             copilot-commit--summaries nil
             copilot-commit--chunks-completed 0
             copilot-commit--chunks-inflight 0
             copilot-commit--phase nil
             copilot-commit--git-status nil
             copilot-commit--conversation-id nil)
       (insert "\n\n# template comment\n"))
     ,@body))

(ert-deftest cc-test-handle-progress-1/begin-sets-streaming ()
  (cc-test--with-progress-buf
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "prompt" nil nil))))
      (copilot-commit--handle-progress-1
       token (list :kind "begin"))
      (with-current-buffer buf
        (should (eq copilot-commit--streaming-p t))))))

(ert-deftest cc-test-handle-progress-1/report-accumulates ()
  (cc-test--with-progress-buf
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "prompt" nil nil))))
      (copilot-commit--handle-progress-1
       token (list :kind "report" :editAgentRounds (vector (list :roundId 1 :reply "hello "))))
      (copilot-commit--handle-progress-1
       token (list :kind "report" :editAgentRounds (vector (list :roundId 1 :reply "world"))))
      (with-current-buffer buf
        (should (string= copilot-commit--accumulated "hello world"))
        ;; Buffer should be updated (not in summarizing phase)
        (goto-char (point-min))
        (should (string-match-p "hello world" (buffer-string)))))))

(ert-deftest cc-test-handle-progress-1/report-silent-during-summarizing ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--phase 'summarizing))
    (let* ((token "tok1")
           (acc-ref (cons nil ""))
           (copilot-commit--active-requests
            (list (list token buf "prompt" 0 acc-ref)))
           (original-content (with-current-buffer buf (buffer-string))))
      (copilot-commit--handle-progress-1
       token (list :kind "report" :editAgentRounds (vector (list :roundId 1 :reply "chunk summary"))))
      ;; Content accumulated in acc-ref, not buffer-local accumulated
      (should (string= (cdr acc-ref) "chunk summary"))
      (with-current-buffer buf
        ;; Buffer content should NOT be updated during summarizing
        (should (string= (buffer-string) original-content))))))

(ert-deftest cc-test-handle-progress-1/report-nil-reply-noop ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--accumulated "pre-existing"))
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "prompt" nil nil))))
      (copilot-commit--handle-progress-1
       token (list :kind "report" :editAgentRounds (vector (list :roundId 1 :reply ""))))
      (with-current-buffer buf
        (should (string= copilot-commit--accumulated "pre-existing"))))))

(ert-deftest cc-test-cancel/clears-buffer-progress ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--streaming-p t
            copilot-commit--available t)
      ;; Simulate progress text in user input region
      (copilot-commit--update-input-region buf "Generating..."))
    (let ((copilot-commit--active-requests nil))
      (cl-letf (((symbol-function 'copilot-commit--destroy-conversation)
                 (lambda (_id) nil)))
        (with-current-buffer buf
          (copilot-commit-cancel)
          ;; User input region should be empty (only "\n\n" before template)
          (goto-char (point-min))
          (should-not (string-match-p "Generating"
                                      (buffer-substring (point-min)
                                                        (copilot-commit--input-region-end)))))))))

(ert-deftest cc-test-handle-progress-1/end-normal-clears-minibuffer ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--accumulated "final message"))
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "the prompt" nil nil))))
      (message "Copilot commit: generating...")
      (copilot-commit--handle-progress-1
       token (list :kind "end"))
      (should (null (current-message))))))

(ert-deftest cc-test-handle-progress-1/end-finalizing-clears-minibuffer ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--phase 'finalizing
            copilot-commit--accumulated "commit message"))
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "final prompt" nil nil))))
      (message "Copilot commit: generating...")
      (copilot-commit--handle-progress-1
       token (list :kind "end"))
      (should (null (current-message))))))

(ert-deftest cc-test-handle-progress-1/end-normal-saves-history ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--accumulated "final message"))
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "the prompt" nil nil))))
      (copilot-commit--handle-progress-1
       token (list :kind "end"))
      (with-current-buffer buf
        (should (equal (car copilot-commit--history)
                       '("the prompt" . "final message")))
        (should (null copilot-commit--streaming-p)))
      ;; Token should be cleaned up
      (should (null (assoc token copilot-commit--active-requests))))))

(ert-deftest cc-test-handle-progress-1/end-summarize-dispatches-more ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--phase 'summarizing
            copilot-commit--chunks '("chunk0" "chunk1" "chunk2")
            copilot-commit--summaries (make-vector 3 nil)
            copilot-commit--chunk-index 1
            copilot-commit--chunks-inflight 1
            copilot-commit--chunks-completed 0))
    (let* ((token "tok1")
           (acc-ref (cons nil "chunk 0 summary"))
           (copilot-commit--active-requests
            (list (list token buf "prompt" 0 acc-ref)))
           (dispatch-called nil))
      (cl-letf (((symbol-function 'copilot-commit--dispatch-chunks)
                 (lambda (_buf) (setq dispatch-called t))))
        (copilot-commit--handle-progress-1
         token (list :kind "end"))
        (with-current-buffer buf
          (should (string= (aref copilot-commit--summaries 0) "chunk 0 summary"))
          (should (= copilot-commit--chunks-completed 1))
          (should (= copilot-commit--chunks-inflight 0)))
        (should dispatch-called)))))

(ert-deftest cc-test-handle-progress-1/end-summarize-last-chunk ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--phase 'summarizing
            copilot-commit--chunks '("c0" "c1" "c2")
            copilot-commit--summaries (vector "sum0" "sum1" nil)
            copilot-commit--chunk-index 3
            copilot-commit--chunks-completed 2
            copilot-commit--chunks-inflight 1))
    (let* ((token "tok1")
           (acc-ref (cons nil "last summary"))
           (copilot-commit--active-requests
            (list (list token buf "prompt" 2 acc-ref)))
           (final-called nil))
      (cl-letf (((symbol-function 'copilot-commit--send-final-prompt)
                 (lambda (_buf) (setq final-called t))))
        (copilot-commit--handle-progress-1
         token (list :kind "end"))
        (with-current-buffer buf
          (should (string= (aref copilot-commit--summaries 2) "last summary"))
          (should (= copilot-commit--chunks-completed 3)))
        (should final-called)))))

(ert-deftest cc-test-handle-progress-1/end-finalizing ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--phase 'finalizing
            copilot-commit--accumulated "commit message"))
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "final prompt" nil nil))))
      (copilot-commit--handle-progress-1
       token (list :kind "end"))
      (with-current-buffer buf
        (should (equal (car copilot-commit--history)
                       '("final prompt" . "commit message")))
        (should (null copilot-commit--phase))
        (should (null copilot-commit--chunks))
        (should (= copilot-commit--chunk-index 0))))))

(ert-deftest cc-test-handle-progress-1/end-with-result ()
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--accumulated "partial"))
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "prompt" nil nil))))
      (copilot-commit--handle-progress-1
       token (list :kind "end" :result "final result"))
      (with-current-buffer buf
        ;; result should override accumulated
        (should (string= (cdar copilot-commit--history) "final result"))))))

(ert-deftest cc-test-handle-progress-1/unknown-token ()
  (cc-test--with-progress-buf
    (let ((copilot-commit--active-requests
           (list (list "tok1" buf "prompt" nil nil))))
      ;; Different token, should be a no-op
      (copilot-commit--handle-progress-1
       "unknown-token" (list :kind "begin"))
      (with-current-buffer buf
        (should (null copilot-commit--streaming-p))))))

(ert-deftest cc-test-handle-progress-1/dead-buffer ()
  (let ((buf (generate-new-buffer " *cc-dead*")))
    (kill-buffer buf)
    (let ((copilot-commit--active-requests
           (list (list "tok1" buf "prompt" nil nil))))
      ;; Should not error on dead buffer
      (copilot-commit--handle-progress-1
       "tok1" (list :kind "begin"))
      (copilot-commit--handle-progress-1
       "tok1" (list :kind "report" :editAgentRounds (vector (list :roundId 1 :reply "x"))))
      (copilot-commit--handle-progress-1
       "tok1" (list :kind "end")))))

(ert-deftest cc-test-handle-progress-1/end-summarize-stores-at-correct-index ()
  "Summary is stored at the correct vector index regardless of completion order."
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--phase 'summarizing
            copilot-commit--chunks '("c0" "c1" "c2")
            copilot-commit--summaries (make-vector 3 nil)
            copilot-commit--chunk-index 3
            copilot-commit--chunks-completed 0
            copilot-commit--chunks-inflight 3))
    ;; Complete chunk 2 first (out of order)
    (let* ((token "tok2")
           (acc-ref (cons nil "summary for chunk 2"))
           (copilot-commit--active-requests
            (list (list token buf "prompt" 2 acc-ref))))
      (cl-letf (((symbol-function 'copilot-commit--dispatch-chunks)
                 (lambda (_buf) nil)))
        (copilot-commit--handle-progress-1
         token (list :kind "end"))
        (with-current-buffer buf
          (should (string= (aref copilot-commit--summaries 2) "summary for chunk 2"))
          (should (null (aref copilot-commit--summaries 0)))
          (should (null (aref copilot-commit--summaries 1)))
          (should (= copilot-commit--chunks-completed 1))
          (should (= copilot-commit--chunks-inflight 2)))))))

;; ---------------------------------------------------------------------------
;;; update-input-region tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-update-input-region/replaces-before-comment ()
  (cc-test--with-buf
    (with-current-buffer buf
      (insert "old content\n\n# comment\n# more\n"))
    (copilot-commit--update-input-region buf "new content")
    (with-current-buffer buf
      (goto-char (point-min))
      (should (string-match-p "new content" (buffer-string)))
      (should (string-match-p "# comment" (buffer-string))))))

(ert-deftest cc-test-update-input-region/preserves-comments ()
  (cc-test--with-buf
    (with-current-buffer buf
      (insert "old\n\n# keep this\n# and this\n"))
    (copilot-commit--update-input-region buf "replaced")
    (with-current-buffer buf
      (should (string-match-p "# keep this" (buffer-string)))
      (should (string-match-p "# and this" (buffer-string))))))

(ert-deftest cc-test-update-input-region/dead-buffer-no-error ()
  (let ((buf (generate-new-buffer " *cc-dead*")))
    (kill-buffer buf)
    ;; Should not error
    (copilot-commit--update-input-region buf "content")))

;; ---------------------------------------------------------------------------
;;; start-chunked-generation fallback test
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-start-chunked/single-chunk-falls-back ()
  "When split produces only 1 chunk, fall back to single-turn generation."
  (cc-test--with-buf
    (let ((single-turn-called nil)
          (dispatch-called nil))
      (with-current-buffer buf
        (insert "\n\n# template\n")
        (setq copilot-commit--history nil
              copilot-commit--chunks nil
              copilot-commit--phase nil))
      (cl-letf (((symbol-function 'copilot-commit--start-generation)
                 (lambda (_prompt _buf) (setq single-turn-called t)))
                ((symbol-function 'copilot-commit--dispatch-chunks)
                 (lambda (_buf) (setq dispatch-called t))))
        (copilot-commit--start-chunked-generation
         (cc-test--make-file-diff "small.el") "M small.el" 50000 buf))
      (should single-turn-called)
      (should-not dispatch-called)
      ;; Buffer should show "Generating..." feedback
      (with-current-buffer buf
        (should (string-match-p "Generating\\.\\.\\." (buffer-string)))))))

;; ---------------------------------------------------------------------------
;;; Git helper mock tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-git/staged-diff-with-changes ()
  (cl-letf (((symbol-function 'vc-root-dir) (lambda () "/tmp/"))
            ((symbol-function 'call-process)
             (lambda (_program _infile _destination _display &rest _args)
               (insert "diff --git a/f b/f\n+added\n")
               0)))
    (let ((result (copilot-commit--get-staged-diff)))
      (should (stringp result))
      (should (string-match-p "diff --git" result)))))

(ert-deftest cc-test-git/staged-diff-empty ()
  (cl-letf (((symbol-function 'vc-root-dir) (lambda () "/tmp/"))
            ((symbol-function 'call-process)
             (lambda (_program _infile _destination _display &rest _args)
               0)))
    (should (null (copilot-commit--get-staged-diff)))))

;; ---------------------------------------------------------------------------
;;; compute-chunk-threshold tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-compute-chunk-threshold/normal ()
  "128000 tokens -> (floor(128000 * 0.9) - 3000) * 4 = 448800."
  (should (= (copilot-commit--compute-chunk-threshold 128000) 448800)))

(ert-deftest cc-test-compute-chunk-threshold/small-clamps ()
  "4000 tokens -> (floor(4000 * 0.9) - 3000) * 4 = 2400, clamped to 10000."
  (should (= (copilot-commit--compute-chunk-threshold 4000) 10000)))

(ert-deftest cc-test-compute-chunk-threshold/zero-clamps ()
  "0 tokens -> clamped to 10000."
  (should (= (copilot-commit--compute-chunk-threshold 0) 10000)))

;; ---------------------------------------------------------------------------
;;; effective-threshold tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-effective-threshold/with-cache ()
  "With cached token limit, returns computed value."
  (let ((copilot-commit--model-token-limits
         `(("default" 128000 . ,(float-time))))
        (copilot-commit-model nil)
        (copilot-commit-cache-ttl (* 48 3600)))
    (should (= (copilot-commit--effective-threshold) 448800))))

(ert-deftest cc-test-effective-threshold/without-cache ()
  "Without cache, returns default and triggers probe."
  (let ((copilot-commit--model-token-limits nil)
        (copilot-commit-model nil)
        (copilot-commit-chunk-threshold 448800)
        (probe-called nil))
    (cl-letf (((symbol-function 'copilot-commit--probe-token-limit)
               (lambda () (setq probe-called t))))
      (should (= (copilot-commit--effective-threshold) 448800))
      (should probe-called))))

(ert-deftest cc-test-effective-threshold/expired-cache ()
  "Expired cache uses stale value as fallback and triggers probe."
  (let ((copilot-commit--model-token-limits
         `(("default" 128000 . ,(- (float-time) 200000))))
        (copilot-commit-model nil)
        (copilot-commit-cache-ttl (* 48 3600))
        (probe-called nil))
    (cl-letf (((symbol-function 'copilot-commit--probe-token-limit)
               (lambda () (setq probe-called t))))
      (should (= (copilot-commit--effective-threshold) 448800))
      (should probe-called))))

;; ---------------------------------------------------------------------------
;;; dispatch-chunks tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-dispatch-chunks/respects-concurrency ()
  "Only dispatches up to concurrency limit."
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--chunks '("c0" "c1" "c2" "c3" "c4")
            copilot-commit--chunk-index 0
            copilot-commit--chunks-inflight 0
            copilot-commit--phase 'summarizing))
    (let ((copilot-commit-chunk-concurrency 2)
          (sent-indices '()))
      (cl-letf (((symbol-function 'copilot-commit--send-chunk)
                 (lambda (_buf idx) (push idx sent-indices))))
        (copilot-commit--dispatch-chunks buf)
        (with-current-buffer buf
          (should (= copilot-commit--chunk-index 2))
          (should (= copilot-commit--chunks-inflight 2)))
        (should (equal (sort sent-indices #'<) '(0 1)))))))

(ert-deftest cc-test-dispatch-chunks/sends-remaining-on-completion ()
  "After a chunk completes, dispatch sends the next pending chunk."
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--chunks '("c0" "c1" "c2" "c3")
            copilot-commit--chunk-index 4
            copilot-commit--chunks-inflight 1
            copilot-commit--chunks-completed 3
            copilot-commit--phase 'summarizing))
    (let ((copilot-commit-chunk-concurrency 2)
          (sent-indices '()))
      (cl-letf (((symbol-function 'copilot-commit--send-chunk)
                 (lambda (_buf idx) (push idx sent-indices))))
        (copilot-commit--dispatch-chunks buf)
        ;; All chunks already dispatched, nothing more to send
        (with-current-buffer buf
          (should (= copilot-commit--chunk-index 4)))
        (should (null sent-indices))))))

;; ---------------------------------------------------------------------------
;;; report caches token limit tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-handle-progress-1/report-caches-token-limit ()
  "Report event with contextSize caches the token limit when no cache exists."
  (cc-test--with-progress-buf
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "prompt" nil nil)))
           (copilot-commit--model-token-limits nil)
           (copilot-commit-model nil)
           (copilot-commit-cache-ttl (* 48 3600)))
      (copilot-commit--handle-progress-1
       token (list :kind "report"
                   :contextSize (list :totalTokenLimit 128000)
                   :editAgentRounds [(:roundId 1 :reply "")]))
      (let ((entry (cdr (assoc "default" copilot-commit--model-token-limits))))
        (should (= (car entry) 128000))
        (should (numberp (cdr entry)))))))

(ert-deftest cc-test-handle-progress-1/report-updates-expired-cache ()
  "Report event updates cache entry when expired."
  (cc-test--with-progress-buf
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "prompt" nil nil)))
           (copilot-commit--model-token-limits
            `(("default" 64000 . ,(- (float-time) 200000))))
           (copilot-commit-model nil)
           (copilot-commit-cache-ttl (* 48 3600)))
      (copilot-commit--handle-progress-1
       token (list :kind "report"
                   :contextSize (list :totalTokenLimit 128000)
                   :editAgentRounds [(:roundId 1 :reply "")]))
      (let ((entry (cdr (assoc "default" copilot-commit--model-token-limits))))
        (should (= (car entry) 128000))))))

(ert-deftest cc-test-handle-progress-1/report-skips-valid-cache ()
  "Report event does not update cache when still valid."
  (cc-test--with-progress-buf
    (let* ((token "tok1")
           (copilot-commit--active-requests
            (list (list token buf "prompt" nil nil)))
           (original-time (float-time))
           (copilot-commit--model-token-limits
            `(("default" 64000 . ,original-time)))
           (copilot-commit-model nil)
           (copilot-commit-cache-ttl (* 48 3600)))
      (copilot-commit--handle-progress-1
       token (list :kind "report"
                   :contextSize (list :totalTokenLimit 128000)
                   :editAgentRounds [(:roundId 1 :reply "")]))
      (let ((entry (cdr (assoc "default" copilot-commit--model-token-limits))))
        ;; Cache should NOT be updated since it's still valid
        (should (= (car entry) 64000))
        (should (= (cdr entry) original-time))))))

;; ---------------------------------------------------------------------------
;;; concurrent summarize end-to-end tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-concurrent-summarize/out-of-order-completion ()
  "Chunks completing out of order still produce correctly ordered summaries."
  (cc-test--with-progress-buf
    (with-current-buffer buf
      (setq copilot-commit--phase 'summarizing
            copilot-commit--chunks '("c0" "c1" "c2")
            copilot-commit--summaries (make-vector 3 nil)
            copilot-commit--chunk-index 3
            copilot-commit--chunks-completed 0
            copilot-commit--chunks-inflight 3))
    (let* ((acc0 (cons nil "sum0"))
           (acc1 (cons nil "sum1"))
           (acc2 (cons nil "sum2"))
           (copilot-commit--active-requests
            (list (list "t0" buf "p0" 0 acc0)
                  (list "t1" buf "p1" 1 acc1)
                  (list "t2" buf "p2" 2 acc2)))
           (final-called nil))
      (cl-letf (((symbol-function 'copilot-commit--send-final-prompt)
                 (lambda (_buf) (setq final-called t)))
                ((symbol-function 'copilot-commit--dispatch-chunks)
                 (lambda (_buf) nil)))
        ;; Complete in order: 2, 0, 1
        (copilot-commit--handle-progress-1 "t2" (list :kind "end"))
        (should-not final-called)
        (copilot-commit--handle-progress-1 "t0" (list :kind "end"))
        (should-not final-called)
        (copilot-commit--handle-progress-1 "t1" (list :kind "end"))
        (should final-called)
        (with-current-buffer buf
          (should (equal (append copilot-commit--summaries nil)
                         '("sum0" "sum1" "sum2"))))))))

;; ---------------------------------------------------------------------------
;;; cache-valid-p tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-cache-valid-p/no-cache ()
  "Returns nil when no cache entry exists."
  (let ((copilot-commit--model-token-limits nil)
        (copilot-commit-model nil)
        (copilot-commit-cache-ttl (* 48 3600)))
    (should-not (copilot-commit--cache-valid-p))))

(ert-deftest cc-test-cache-valid-p/valid ()
  "Returns non-nil when cache entry exists and is not expired."
  (let ((copilot-commit--model-token-limits
         `(("default" 128000 . ,(float-time))))
        (copilot-commit-model nil)
        (copilot-commit-cache-ttl (* 48 3600)))
    (should (copilot-commit--cache-valid-p))))

(ert-deftest cc-test-cache-valid-p/expired ()
  "Returns nil when cache entry is expired."
  (let ((copilot-commit--model-token-limits
         `(("default" 128000 . ,(- (float-time) 200000))))
        (copilot-commit-model nil)
        (copilot-commit-cache-ttl (* 48 3600)))
    (should-not (copilot-commit--cache-valid-p))))

(ert-deftest cc-test-cache-valid-p/custom-model ()
  "Uses custom model key when copilot-commit-model is set."
  (let ((copilot-commit--model-token-limits
         `(("gpt-4" 128000 . ,(float-time))))
        (copilot-commit-model "gpt-4")
        (copilot-commit-cache-ttl (* 48 3600)))
    (should (copilot-commit--cache-valid-p))))

;; ---------------------------------------------------------------------------
;;; cache-token-limit format tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-cache-token-limit/new-entry ()
  "Creates cache entry with (max-tokens . timestamp) format."
  (let ((copilot-commit--model-token-limits nil)
        (copilot-commit-model nil))
    (copilot-commit--cache-token-limit 128000)
    (let ((entry (cdr (assoc "default" copilot-commit--model-token-limits))))
      (should (= (car entry) 128000))
      (should (numberp (cdr entry))))))

(ert-deftest cc-test-cache-token-limit/update-entry ()
  "Updates existing cache entry with new value and timestamp."
  (let ((copilot-commit--model-token-limits
         `(("default" 64000 . ,(- (float-time) 100))))
        (copilot-commit-model nil))
    (copilot-commit--cache-token-limit 128000)
    (let ((entry (cdr (assoc "default" copilot-commit--model-token-limits))))
      (should (= (car entry) 128000))
      ;; Timestamp should be recent
      (should (< (- (float-time) (cdr entry)) 2)))))

;; ---------------------------------------------------------------------------
;;; UI feedback tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-start-chunked/shows-initial-progress ()
  "Chunked generation shows 0/N progress in buffer."
  (cc-test--with-buf
    (with-current-buffer buf
      (insert "\n\n# template\n")
      (setq copilot-commit--history nil
            copilot-commit--chunks nil
            copilot-commit--phase nil))
    (cl-letf (((symbol-function 'copilot-commit--start-generation)
               (lambda (_prompt _buf) nil))
              ((symbol-function 'copilot-commit--dispatch-chunks)
               (lambda (_buf) nil)))
      (let* ((diffs (mapcar (lambda (i)
                              (cc-test--make-file-diff
                               (format "file%d.el" i)
                               (cc-test--make-large-content 80)))
                            '(1 2 3)))
             (diff (mapconcat #'identity diffs "\n")))
        (copilot-commit--start-chunked-generation diff "M files" 100 buf))
      (with-current-buffer buf
        (goto-char (point-min))
        (should (string-match-p "Analyzing changes (0/" (buffer-string)))))))

;; ---------------------------------------------------------------------------
;;; probe cleanup tests
;; ---------------------------------------------------------------------------

(ert-deftest cc-test-handle-progress-1/report-cleans-up-probe ()
  "Report event for a probe (nil buf) caches token limit and removes the entry."
  (let* ((token "probe-tok")
         (copilot-commit--active-requests
          (list (list token nil nil nil nil)))
         (copilot-commit--model-token-limits nil)
         (copilot-commit-model nil)
         (copilot-commit-cache-ttl (* 48 3600)))
    (copilot-commit--handle-progress-1
     token (list :kind "report"
                 :contextSize (list :totalTokenLimit 64000)
                 :editAgentRounds [(:roundId 1 :reply "")]))
    (should (null (assoc token copilot-commit--active-requests)))
    ;; Token limit should be cached
    (let ((entry (cdr (assoc "default" copilot-commit--model-token-limits))))
      (should (= (car entry) 64000)))))

(provide 'copilot-commit-test)
;;; copilot-commit-test.el ends here
