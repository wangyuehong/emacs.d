;;; copilot-commit.el --- Generate commit messages via Copilot LSP -*- lexical-binding: t; -*-

;;; Commentary:
;; Generate Conventional Commits messages using the Copilot LSP chat API.
;; Requires the `copilot' package for LSP connection management.

;;; Code:

(require 'copilot)
(require 'copilot-commit-core)

(defvar copilot--ignore-response)
(defvar copilot--request-handlers)

;;; Dependency check

(defconst copilot-commit--required-functions
  '(copilot--connection-alivep
    copilot--async-request
    copilot--dbind
    copilot-on-notification
    copilot-on-request)
  "Copilot internal functions/macros required by copilot-commit.")

(defconst copilot-commit--required-variables
  '(copilot--ignore-response)
  "Copilot internal variables required by copilot-commit.")

(defvar copilot-commit--available t
  "Non-nil when all required copilot internals are available.")

(defun copilot-commit--check-deps ()
  "Check that all required copilot internals are available.
Sets `copilot-commit--available' and returns the list of missing symbols."
  (let ((missing (append
                  (cl-remove-if #'fboundp copilot-commit--required-functions)
                  (cl-remove-if #'boundp copilot-commit--required-variables))))
    (setq copilot-commit--available (null missing))
    missing))

(let ((missing (copilot-commit--check-deps)))
  (when missing
    (display-warning
     'copilot-commit
     (format "copilot-commit disabled: copilot package missing required internals: %s"
             (mapconcat #'symbol-name missing ", "))
     :error)))

;;; Customization (IO-specific)

(defcustom copilot-commit-model nil
  "Model ID for commit message generation.
When nil, the server decides which model to use."
  :type '(choice (const :tag "Server default" nil)
                 (string :tag "Model ID"))
  :group 'copilot-commit)

;;; Internal state

(defvar copilot-commit--model-token-limits nil
  "Alist of (model-key max-tokens . timestamp) for cached token limits.")

(defvar copilot-commit--active-requests nil
  "Alist of active requests for progress routing.
Each entry is (token buffer prompt chunk-index acc-ref).
CHUNK-INDEX is the chunk index for summarize requests (nil otherwise).
ACC-REF is a cons cell (nil . \"\") for accumulating content.")

(defvar-local copilot-commit--conversation-id nil
  "Current conversation ID for this commit buffer.")

(defvar-local copilot-commit--streaming-p nil
  "Non-nil when streaming is in progress.")

(defvar-local copilot-commit--accumulated nil
  "Accumulated streaming content.")

(defvar-local copilot-commit--history nil
  "Conversation history as a list of (REQUEST . RESPONSE) pairs.
Most recent pair is at the front.")

(defvar-local copilot-commit--chunks nil
  "List of diff chunks to be summarized.")

(defvar-local copilot-commit--chunk-index 0
  "Index of the next chunk to dispatch.")

(defvar-local copilot-commit--summaries nil
  "Vector of collected summaries from chunk processing.")

(defvar-local copilot-commit--chunks-completed 0
  "Number of chunks that have completed summarization.")

(defvar-local copilot-commit--chunks-inflight 0
  "Number of chunk summarize requests currently in flight.")

(defvar-local copilot-commit--phase nil
  "Current chunked generation phase: nil, `summarizing', or `finalizing'.")

(defvar-local copilot-commit--git-status nil
  "Cached git status string used during chunked generation.")

;;; Logging

(defun copilot-commit--log (format-string &rest args)
  "Log a message with FORMAT-STRING and ARGS to *copilot-commit-log* buffer."
  (with-current-buffer (get-buffer-create "*copilot-commit-log*")
    (goto-char (point-max))
    (insert (format-time-string "[%H:%M:%S] ")
            (apply #'format format-string args) "\n")))

;;; Git helpers

(defun copilot-commit--get-staged-diff ()
  "Return staged diff as a string, or nil if empty."
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (with-temp-buffer
      (call-process "git" nil t nil
                    "--no-pager" "diff" "--cached" "--no-color")
      (let ((output (string-trim (buffer-string))))
        (unless (string-empty-p output)
          output)))))

(defun copilot-commit--get-git-status ()
  "Return short git status as a string."
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (with-temp-buffer
      (call-process "git" nil t nil
                    "status" "--short" "--branch" "--untracked-files=no")
      (string-trim (buffer-string)))))

;;; Token limit cache

(defun copilot-commit--cache-token-limit (max-tokens)
  "Cache MAX-TOKENS with current timestamp for the current model."
  (let* ((key (or copilot-commit-model "default"))
         (entry (assoc key copilot-commit--model-token-limits)))
    (if entry
        (setcdr entry (cons max-tokens (float-time)))
      (push (cons key (cons max-tokens (float-time)))
            copilot-commit--model-token-limits))))

(defun copilot-commit--cache-valid-p ()
  "Return non-nil if the cache entry for the current model exists and is not expired."
  (let* ((key (or copilot-commit-model "default"))
         (entry (cdr (assoc key copilot-commit--model-token-limits))))
    (and entry
         (< (- (float-time) (cdr entry)) copilot-commit-cache-ttl))))

(defun copilot-commit--effective-threshold ()
  "Return the effective chunk threshold.
Uses cached token limit if valid, expired cache as fallback with probe,
or `copilot-commit-chunk-threshold' when no cache exists."
  (let* ((key (or copilot-commit-model "default"))
         (entry (cdr (assoc key copilot-commit--model-token-limits))))
    (cond
     ;; Valid cache: use computed value
     ((and entry (copilot-commit--cache-valid-p))
      (copilot-commit--compute-chunk-threshold (car entry)))
     ;; Expired cache: use stale value as fallback, trigger probe
     (entry
      (copilot-commit--probe-token-limit)
      (copilot-commit--compute-chunk-threshold (car entry)))
     ;; No cache: use default, trigger probe
     (t
      (copilot-commit--probe-token-limit)
      copilot-commit-chunk-threshold))))

(defun copilot-commit--probe-token-limit ()
  "Send a minimal conversation to discover the model's token limit.
The result is cached for future use via the progress begin handler."
  (when (and copilot-commit--available
             (copilot--connection-alivep)
             ;; Don't probe if one is already in flight
             (not (cl-some (lambda (e) (null (nth 1 e)))
                           copilot-commit--active-requests)))
    (let* ((token (copilot-commit--generate-token))
           (turns (copilot-commit--build-turns nil "hi"))
           (params (list :workDoneToken token
                         :turns turns
                         :capabilities (list :skills (vector "current-editor")
                                             :allSkills t)
                         :source "panel")))
      (when copilot-commit-model
        (setq params (plist-put params :model copilot-commit-model)))
      (copilot-commit--log "probe-token-limit: token=%s" token)
      ;; Register a probe entry (nil buffer means probe-only, no commit buffer)
      (push (list token nil nil nil nil) copilot-commit--active-requests)
      (copilot--async-request
       'conversation/create params
       :success-fn (lambda (result)
                     (let ((conv-id (plist-get result :conversationId)))
                       (copilot-commit--log "probe: model=%s conv=%s"
                                            (plist-get result :modelName) conv-id)
                       ;; Destroy immediately, we only need the progress report event
                       (copilot-commit--destroy-conversation conv-id)))
       :error-fn (lambda (err)
                   (copilot-commit--cleanup-request token)
                   (copilot-commit--log "probe failed: %S" err))))))

;;; Streaming display

(defun copilot-commit--update-input-region (buf content)
  "Replace the user input region in BUF with CONTENT.
Only modifies text before the first `#' comment line;
template comments, scissor line, and diff are never touched."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (save-excursion
          (let ((end (copilot-commit--input-region-end)))
            (copilot-commit--log "update-input-region: end=%d content-len=%d"
                                 end (length content))
            (delete-region (point-min) end)
            (goto-char (point-min))
            (insert content "\n\n")))))))

;;; LSP conversation management

(defvar copilot-commit--token-counter 0
  "Counter for generating unique tokens.")

(defun copilot-commit--generate-token ()
  "Generate a unique token for workDoneToken."
  (format "copilot-commit-%s-%d" (float-time)
          (cl-incf copilot-commit--token-counter)))

(defun copilot-commit--create-conversation (turns token buf callback)
  "Create a new conversation with TURNS, using TOKEN for progress.
BUF is the commit buffer for error handling.
TURNS is a vector of turn objects (from `copilot-commit--build-turns').
CALLBACK is called with the conversation ID on success."
  (let ((params (list :workDoneToken token
                      :turns turns
                      :capabilities (list :skills (vector "current-editor")
                                          :allSkills t)
                      :source "panel")))
    (when copilot-commit-model
      (setq params (plist-put params :model copilot-commit-model)))
    (copilot--async-request
     'conversation/create params
     :success-fn (lambda (result)
                   (copilot-commit--log "conversation: model=%s conv=%s"
                                        (plist-get result :modelName)
                                        (plist-get result :conversationId))
                   (funcall callback (plist-get result :conversationId)))
     :error-fn (lambda (err)
                 (copilot-commit--cleanup-request token)
                 (when (buffer-live-p buf)
                   (with-current-buffer buf
                     (let ((phase copilot-commit--phase)
                           (chunk-idx copilot-commit--chunk-index)
                           (total (length copilot-commit--chunks)))
                       (setq copilot-commit--streaming-p nil)
                       (copilot-commit--reset-chunk-state)
                       (copilot-commit--log "conversation creation failed: %S" err)
                       (if (eq phase 'summarizing)
                           (message "Copilot commit: failed during chunk %d/%d summarization: %S"
                                    (1+ chunk-idx) total err)
                         (message "Copilot commit: conversation creation failed: %S" err)))))))))

(defun copilot-commit--destroy-conversation (conv-id)
  "Destroy conversation CONV-ID."
  (when conv-id
    (copilot--async-request
     'conversation/destroy (list :conversationId conv-id)
     :success-fn copilot--ignore-response
     :error-fn copilot--ignore-response)))

;;; Progress handler

(defun copilot-commit--cleanup-request (token)
  "Remove TOKEN from active requests."
  (setq copilot-commit--active-requests
        (assoc-delete-all token copilot-commit--active-requests)))

(defun copilot-commit--handle-progress-1 (token value)
  "Core progress handling logic with TOKEN and VALUE already destructured."
  (let ((entry (assoc token copilot-commit--active-requests)))
    (when entry
      (let* ((buf (nth 1 entry))
             (prompt (nth 2 entry))
             (chunk-index (nth 3 entry))
             (acc-ref (nth 4 entry))
             (kind (plist-get value :kind))
             (phase (when (buffer-live-p buf)
                      (buffer-local-value 'copilot-commit--phase buf))))
        (copilot-commit--log "progress: kind=%s token=%s phase=%s chunk-index=%S"
                             kind token phase chunk-index)
        (cond
         ((equal kind "begin")
          (when (buffer-live-p buf)
            (with-current-buffer buf
              (setq copilot-commit--streaming-p t))))

         ((equal kind "report")
          ;; Extract and cache token limit from contextSize (only when missing or expired)
          (let* ((context-size (plist-get value :contextSize))
                 (total-limit (when context-size
                                (plist-get context-size :totalTokenLimit))))
            (when (and total-limit (not (copilot-commit--cache-valid-p)))
              (copilot-commit--cache-token-limit total-limit)
              (copilot-commit--log "cached token limit: model=%s max-tokens=%d"
                                   (or copilot-commit-model "default") total-limit)))
          ;; For probe requests (nil buf), clean up after caching
          (when (and (not buf) entry)
            (copilot-commit--cleanup-request token))
          (let* ((rounds (plist-get value :editAgentRounds))
                 (reply (when (and (vectorp rounds) (> (length rounds) 0))
                          (plist-get (aref rounds 0) :reply))))
            (when (and reply (not (string-empty-p reply)))
              (cond
               ;; Summarizing phase: accumulate in per-request acc-ref
               ((and acc-ref (eq phase 'summarizing))
                (setcdr acc-ref (concat (cdr acc-ref) reply)))
               ;; Non-summarizing: accumulate in buffer-local and update display
               ((buffer-live-p buf)
                (with-current-buffer buf
                  (setq copilot-commit--accumulated
                        (concat (or copilot-commit--accumulated "") reply))
                  (copilot-commit--update-input-region
                   buf (or copilot-commit--accumulated ""))))))))

         ((equal kind "end")
          (cond
           ;; Summarizing phase: store summary at correct index
           ((and (buffer-live-p buf) (eq phase 'summarizing) chunk-index)
            (with-current-buffer buf
              ;; Get final content from end event or acc-ref
              (let* ((result (plist-get value :result))
                     (rounds (plist-get value :editAgentRounds))
                     (round-reply (when (and (vectorp rounds)
                                             (> (length rounds) 0))
                                    (plist-get (aref rounds 0) :reply)))
                     (end-content (or result round-reply))
                     (final (if (and end-content (not (string-empty-p end-content)))
                                end-content
                              (cdr acc-ref))))
                (aset copilot-commit--summaries chunk-index (or final "")))
              (cl-incf copilot-commit--chunks-completed)
              (cl-decf copilot-commit--chunks-inflight)
              (let ((total (length copilot-commit--chunks)))
                (copilot-commit--log "chunk %d/%d summarized (completed=%d inflight=%d)"
                                     (1+ chunk-index) total
                                     copilot-commit--chunks-completed
                                     copilot-commit--chunks-inflight)
                (copilot-commit--show-chunk-progress
                 buf copilot-commit--chunks-completed total)
                (if (= copilot-commit--chunks-completed total)
                    (progn
                      (setq copilot-commit--streaming-p nil)
                      (copilot-commit--send-final-prompt buf))
                  (copilot-commit--dispatch-chunks buf)))))

           ;; Finalizing or normal: update buffer and save history
           ((buffer-live-p buf)
            (with-current-buffer buf
              (let* ((result (plist-get value :result))
                     (rounds (plist-get value :editAgentRounds))
                     (round-reply (when (and (vectorp rounds)
                                             (> (length rounds) 0))
                                    (plist-get (aref rounds 0) :reply)))
                     (final (or result round-reply)))
                (when (and final (not (string-empty-p final)))
                  (setq copilot-commit--accumulated final)))
              (setq copilot-commit--streaming-p nil)
              (push (cons prompt (or copilot-commit--accumulated ""))
                    copilot-commit--history)
              (copilot-commit--update-input-region
               buf (or copilot-commit--accumulated ""))
              (when (eq phase 'finalizing)
                (setq copilot-commit--phase nil
                      copilot-commit--chunks nil
                      copilot-commit--chunk-index 0))
              (message nil))))
          (copilot-commit--cleanup-request token)))))))

(defun copilot-commit--handle-progress (msg)
  "Handle `$/progress' notification MSG for commit generation."
  (copilot--dbind (token value) msg
    (copilot-commit--handle-progress-1 token value)))

;; Register handlers only when deps are available
(when copilot-commit--available
  (copilot-on-notification '$/progress #'copilot-commit--handle-progress))

;;; Context request handler

(defun copilot-commit--handle-context (msg)
  "Handle `conversation/context' request.
Returns nil for all skills since commit buffer has no editor context."
  (copilot-commit--log "context requested: %S" msg)
  nil)

;; Only register if no handler exists yet (avoid overriding copilot-lsp-chat)
(when (and copilot-commit--available
           (not (gethash 'conversation/context copilot--request-handlers)))
  (copilot-on-request 'conversation/context #'copilot-commit--handle-context))

;;; Chunked generation

(defun copilot-commit--show-chunk-progress (buf completed total)
  "Show chunk progress in BUF: Analyzing changes (COMPLETED/TOTAL)..."
  (copilot-commit--update-input-region
   buf (format "Analyzing changes (%d/%d)..." completed total)))

(defun copilot-commit--reset-chunk-state ()
  "Reset all chunked generation state variables."
  (setq copilot-commit--chunks nil
        copilot-commit--chunk-index 0
        copilot-commit--summaries nil
        copilot-commit--phase nil
        copilot-commit--git-status nil
        copilot-commit--chunks-completed 0
        copilot-commit--chunks-inflight 0))

(defun copilot-commit--dispatch-chunks (buf)
  "Dispatch pending chunks up to concurrency limit in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let ((total (length copilot-commit--chunks)))
        (while (and (< copilot-commit--chunk-index total)
                    (< copilot-commit--chunks-inflight
                       copilot-commit-chunk-concurrency))
          (copilot-commit--send-chunk buf copilot-commit--chunk-index)
          (cl-incf copilot-commit--chunk-index)
          (cl-incf copilot-commit--chunks-inflight))))))

(defun copilot-commit--send-chunk (buf index)
  "Send chunk at INDEX for summarization in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((chunks copilot-commit--chunks)
             (total (length chunks))
             (chunk (nth index chunks))
             (prompt (copilot-commit--build-chunk-prompt chunk index total))
             (token (copilot-commit--generate-token))
             (turns (copilot-commit--build-turns nil prompt))
             (acc-ref (cons nil "")))
        (copilot-commit--log "send-chunk: %d/%d token=%s" (1+ index) total token)
        (setq copilot-commit--streaming-p t)
        (push (list token buf prompt index acc-ref)
              copilot-commit--active-requests)
        (copilot-commit--create-conversation
         turns token buf
         (lambda (conv-id)
           (copilot-commit--log "chunk %d conversation: %S" (1+ index) conv-id)
           ;; Destroy after progress completes; conversation is no longer needed
           ;; since chunk summaries don't maintain conversation history
           (copilot-commit--destroy-conversation conv-id)))))))

(defun copilot-commit--send-final-prompt (buf)
  "Send the final commit message generation request in BUF."
  (when (buffer-live-p buf)
    (with-current-buffer buf
      (let* ((summaries-list (append copilot-commit--summaries nil))
             (prompt (copilot-commit--build-final-prompt
                      summaries-list
                      copilot-commit--git-status)))
        (copilot-commit--log "send-final-prompt: %d summaries"
                             (length summaries-list))
        (setq copilot-commit--phase 'finalizing)
        (copilot-commit--update-input-region buf "Generating commit message...")
        (copilot-commit--start-generation prompt buf)))))

(defun copilot-commit--start-chunked-generation (diff status threshold buf)
  "Start chunked diff processing for DIFF with STATUS in BUF.
THRESHOLD is the chunk size limit in characters."
  (let ((chunks (copilot-commit--split-diff diff threshold)))
    (if (<= (length chunks) 1)
        ;; Only 1 chunk after split: fall back to single-turn
        (let ((prompt (copilot-commit--build-prompt diff status)))
          (with-current-buffer buf
            (setq copilot-commit--history nil))
          (copilot-commit--update-input-region buf "Generating...")
          (message "Copilot commit: generating...")
          (copilot-commit--start-generation prompt buf))
      ;; Multiple chunks: start chunked flow
      (with-current-buffer buf
        (setq copilot-commit--history nil
              copilot-commit--chunks chunks
              copilot-commit--chunk-index 0
              copilot-commit--summaries (make-vector (length chunks) nil)
              copilot-commit--chunks-completed 0
              copilot-commit--chunks-inflight 0
              copilot-commit--phase 'summarizing
              copilot-commit--git-status status))
      (copilot-commit--log "start-chunked: %d chunks" (length chunks))
      (copilot-commit--show-chunk-progress buf 0 (length chunks))
      (message "Copilot commit: analyzing large diff (%d parts)..." (length chunks))
      (copilot-commit--dispatch-chunks buf))))

;;; Interactive commands

(defun copilot-commit--start-generation (prompt buf)
  "Start a new conversation with PROMPT in BUF.
Handles common setup: resetting state, creating conversation,
and registering progress tracking."
  (with-current-buffer buf
    (let* ((token (copilot-commit--generate-token))
           (turns (copilot-commit--build-turns
                   copilot-commit--history prompt)))
      (copilot-commit--log "start-generation: token=%s history-len=%d"
                           token (length copilot-commit--history))
      ;; Destroy old conversation if any
      (when copilot-commit--conversation-id
        (copilot-commit--destroy-conversation copilot-commit--conversation-id)
        (setq copilot-commit--conversation-id nil))
      (setq copilot-commit--accumulated nil
            copilot-commit--streaming-p t)
      ;; Register active request with prompt for history
      (push (list token buf prompt nil nil) copilot-commit--active-requests)
      (copilot-commit--create-conversation
       turns token buf
       (lambda (conv-id)
         (when (buffer-live-p buf)
           (with-current-buffer buf
             (setq copilot-commit--conversation-id conv-id))))))))

;;;###autoload
(defun copilot-commit-insert-message ()
  "Generate and insert a commit message for staged changes."
  (interactive)
  (cond
   ((not copilot-commit--available)
    (message "Copilot commit: disabled due to missing copilot internals"))
   ((not (copilot--connection-alivep))
    (message "Copilot commit: LSP connection is not active"))
   (copilot-commit--streaming-p
    (message "Copilot commit: generation already in progress"))
   (t
    (let ((diff (copilot-commit--get-staged-diff)))
      (if (not diff)
          (message "Copilot commit: no staged changes")
        (let ((status (copilot-commit--get-git-status))
              (threshold (copilot-commit--effective-threshold)))
          (copilot-commit--reset-chunk-state)
          (if (> (length diff) threshold)
              ;; Large diff: chunked generation
              (copilot-commit--start-chunked-generation diff status threshold (current-buffer))
            ;; Normal: single-turn generation
            (let ((prompt (copilot-commit--build-prompt diff status)))
              (setq copilot-commit--history nil)
              (copilot-commit--update-input-region (current-buffer) "Generating...")
              (message "Copilot commit: generating...")
              (copilot-commit--start-generation prompt (current-buffer))))))))))

;;;###autoload
(defun copilot-commit-regenerate-message ()
  "Regenerate commit message with additional instructions.
When cached summaries exist from a previous chunked generation,
reuse them instead of re-analyzing the diff."
  (interactive)
  (cond
   ((not copilot-commit--available)
    (message "Copilot commit: disabled due to missing copilot internals"))
   ((not copilot-commit--history)
    ;; No previous generation, fall back to initial generation
    (copilot-commit-insert-message))
   ((not (copilot--connection-alivep))
    (message "Copilot commit: LSP connection is not active"))
   (copilot-commit--streaming-p
    (message "Copilot commit: generation already in progress"))
   (t
    (let* ((instruction (read-string "Additional instructions: "))
           (message-text (if (string-empty-p instruction)
                             "Please regenerate"
                           instruction)))
      (if copilot-commit--summaries
          ;; Chunked generation: rebuild final prompt with summaries + instruction
          (let ((prompt (copilot-commit--build-final-prompt
                         (append copilot-commit--summaries nil)
                         copilot-commit--git-status)))
            ;; Replace the first history entry (the final prompt) to include instruction
            (when copilot-commit--history
              (setcar (car copilot-commit--history) prompt))
            (copilot-commit--update-input-region (current-buffer) "Regenerating...")
            (message "Copilot commit: regenerating...")
            (copilot-commit--start-generation message-text (current-buffer)))
        ;; Normal single-turn regenerate
        (copilot-commit--update-input-region (current-buffer) "Regenerating...")
        (message "Copilot commit: regenerating...")
        (copilot-commit--start-generation message-text (current-buffer)))))))

(defun copilot-commit-streaming-p ()
  "Return non-nil if commit message generation is in progress."
  copilot-commit--streaming-p)

;;;###autoload
(defun copilot-commit-cancel ()
  "Cancel ongoing commit message generation."
  (interactive)
  (cond
   ((not copilot-commit--available)
    (message "Copilot commit: disabled due to missing copilot internals"))
   ((not copilot-commit--streaming-p)
    (message "Copilot commit: no generation in progress"))
   (t
    (setq copilot-commit--streaming-p nil)
    ;; Remove all requests for this buffer
    (let ((buf (current-buffer)))
      (setq copilot-commit--active-requests
            (cl-remove-if (lambda (entry) (eq (cadr entry) buf))
                          copilot-commit--active-requests)))
    ;; Destroy the conversation
    (when copilot-commit--conversation-id
      (copilot-commit--destroy-conversation copilot-commit--conversation-id)
      (setq copilot-commit--conversation-id nil))
    ;; Clean chunked state
    (copilot-commit--reset-chunk-state)
    ;; Clear progress text from buffer
    (copilot-commit--update-input-region (current-buffer) "")
    (message "Copilot commit: cancelled"))))

(provide 'copilot-commit)
;;; copilot-commit.el ends here
