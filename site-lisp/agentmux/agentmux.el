;;; agentmux.el --- Interact with AI agents via tmux -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Wang Yuehong

;; Author: Wang Yuehong <wangyuehong@gmail.com>
;; URL: https://github.com/wangyuehong/emacs.d
;; Version: 0.1.0
;; Package-Requires: ((emacs "30.1") (transient "0.4.0") (emamux "20200315"))
;; Keywords: tools, ai, tmux

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:
;; Agentmux provides Emacs integration for AI agent CLIs running in tmux.
;;
;; Supports any CLI-based AI agent (Claude Code, Gemini CLI, etc.).
;; Configure display name via `agentmux-agent-name'.
;;
;; Features:
;; - Send commands with file context (path, line number, region content)
;; - Send file reference (path:line or path:start-end) directly
;; - Quick digit input (0-9) for menu selections
;; - Menu navigation mode for multi-option interactions
;; - Stage only mode to send without executing (customizable key)
;;
;; Requirements:
;; - tmux must be running with an active session
;; - Target pane should have an AI agent CLI (e.g., Claude Code)
;;
;; Usage:
;;   (require 'agentmux)
;;   (global-set-key (kbd "C-c c") 'agentmux-transient)
;;   ;; First use: M-x agentmux-set-target to select tmux pane
;;
;; Note: This package overrides some emamux functions via advice to improve
;; pane/window selection display. Use `unload-feature' to remove cleanly.

;;; Code:

(require 'code-ref-core)
(require 'emamux)
(require 'seq)
(require 'transient)

;;; Customization

(defgroup agentmux nil
  "Interact with AI agents via tmux."
  :group 'tools
  :prefix "agentmux-")

(defcustom agentmux-agent-name "agentmux"
  "Display name for agent in prompts and messages."
  :type 'string
  :group 'agentmux)

(defcustom agentmux-context-path-style 'git
  "Context path format style.
Values:
  \\='git - Relative to Git repository root
  \\='project - Relative to Emacs project root
  \\='absolute - Absolute path
  \\='filename - Filename only"
  :type '(choice (const :tag "Git relative" git)
           (const :tag "Project relative" project)
           (const :tag "Absolute" absolute)
           (const :tag "Filename only" filename))
  :group 'agentmux)

(defcustom agentmux-context-include-line t
  "Whether to include line number in context."
  :type 'boolean
  :group 'agentmux)

(defcustom agentmux-context-include-content nil
  "Whether to include content in context.
When non-nil, includes the selected region or current line as a code block."
  :type 'boolean
  :group 'agentmux)

(defcustom agentmux-minibuffer-initial-height 3
  "Initial height of minibuffer for multiline input.
Valid range is 1 to 30 lines."
  :type '(integer :match (lambda (_widget value) (<= 1 value 30)))
  :group 'agentmux)

(defcustom agentmux-prompt-max-location-length 40
  "Maximum length of location string in prompt.
Longer paths will be truncated with ellipsis.
Minimum is 4 (3 for ellipsis + 1 char)."
  :type '(integer :match (lambda (_widget value) (>= value 4)))
  :group 'agentmux)

(defcustom agentmux-send-no-enter-key (kbd "C-<return>")
  "Key binding for stage only (send without Enter) in minibuffer."
  :type 'key-sequence
  :group 'agentmux)

(defcustom agentmux-agent-cli-commands
  '("claude" "codex")
  "Executable basenames identifying an Agent CLI.
Matched against the kernel `p_comm' (the exec basename, immutable after
exec) of each process in a pane's descendant tree. Claude Code and
similar Node apps rewrite their `process.title' to a version string, so
tmux `pane_current_command' and `process-attributes' `comm' report the
rewritten value; kernel `p_comm' preserves the real executable name.
Panes whose tree contains a process matching this list are preferred as
default target over other non-Emacs panes."
  :type '(repeat string)
  :group 'agentmux)

(defvar agentmux-input-history nil
  "History of inputs sent to agent.")

;;; Emamux advice for better display and defaults

(defconst agentmux--field-sep "\x1f"
  "ASCII Unit Separator used between metadata fields in tmux format strings.
Chosen because it cannot legitimately appear in window names or commands.")

(defun agentmux--completion-table-preserving-order (candidates)
  "Return a completion table that yields CANDIDATES in their input order.
Required because narrowing UIs (vertico, ivy, …) re-sort candidates by
default and would defeat agentmux's deliberate ranking."
  (lambda (string pred action)
    (if (eq action 'metadata)
      '(metadata
         (display-sort-function . identity)
         (cycle-sort-function . identity))
      (complete-with-action action candidates string pred))))

(defun agentmux--emacs-pane-id ()
  "Return the tmux pane ID (e.g., `%19') containing Emacs, or nil.
Treats an empty `TMUX_PANE' value as absent so callers cannot accidentally
issue tmux queries against a phantom pane."
  (let ((pane (getenv "TMUX_PANE")))
    (and pane (not (string-empty-p pane)) pane)))

(defun agentmux--emacs-window-id ()
  "Return the tmux window ID (e.g., `@8') containing Emacs, or nil.
Returns nil only when Emacs is not running inside tmux (`TMUX_PANE'
unset or empty). Signals `user-error' when `TMUX_PANE' is set but the
lookup fails (e.g., the originating pane has been killed) — never falls
back silently to a guessed window."
  (when-let* ((pane (agentmux--emacs-pane-id)))
    (with-temp-buffer
      (condition-case err
        (progn
          (emamux:tmux-run-command t "display-message" "-p" "-t" pane
            "-F" "#{window_id}")
          (let ((out (string-trim (buffer-string))))
            (when (string-empty-p out)
              (user-error "tmux returned empty window id for pane %s" pane))
            out))
        ;; Let upstream user-errors (including the empty-output branch
        ;; above) propagate verbatim so we don't double-wrap their text.
        (user-error (signal (car err) (cdr err)))
        (error
          (user-error "Failed to query Emacs window id (TMUX_PANE=%s): %s"
            pane (error-message-string err)))))))

(defun agentmux--process-maps ()
  "Return (CHILDREN-MAP . COMM-MAP) built from `ps -axo pid=,ppid=,comm='.
Uses `ps' (not `process-attributes') because on macOS the latter reads
`pbi_name' which is rewritten by `setproctitle'; `ps -o comm=' reads
kernel `p_comm' which is immutable after `exec'.
Signals `user-error' when `ps' fails — never returns empty maps as a
silent fallback that would mis-classify every pane as non-agent."
  (let ((children (make-hash-table :test 'eql :size 512))
         (comm     (make-hash-table :test 'eql :size 512)))
    (with-temp-buffer
      (let ((rc (process-file "ps" nil t nil "-axo" "pid=,ppid=,comm=")))
        (unless (zerop rc)
          (user-error "ps exited with status %d while detecting agent panes" rc)))
      (dolist (line (split-string (buffer-string) "\n" t))
        (let* ((fields (split-string (string-trim line) nil t))
                (pid   (string-to-number (nth 0 fields)))
                (ppid  (string-to-number (nth 1 fields)))
                (c     (file-name-nondirectory
                         (mapconcat #'identity (nthcdr 2 fields) " "))))
          (push pid (gethash ppid children))
          (puthash pid c comm))))
    (cons children comm)))

(defun agentmux--pid-tree-has-agent-p (root children-map comm-map)
  "Return non-nil if ROOT or any descendant is an Agent CLI.
The visited set guards against pathological ps snapshots with cycles."
  (let ((stack   (list root))
         (visited (make-hash-table :test 'eql)))
    (catch 'found
      (while stack
        (let ((pid (pop stack)))
          (unless (gethash pid visited)
            (puthash pid t visited)
            (let ((c (gethash pid comm-map)))
              (when (and c (member c agentmux-agent-cli-commands))
                (throw 'found t)))
            (dolist (child (gethash pid children-map))
              (push child stack)))))
      nil)))

(defun agentmux--compute-agent-panes (entries)
  "Return hash set of pane-ids from ENTRIES whose tree contains an Agent CLI.
Each ENTRY is a plist with `:pane-id' and `:pane-pid'. Precomputing the
set before sort keeps the priority function pure and avoids repeated
BFS walks inside the sort comparator."
  (let* ((maps     (agentmux--process-maps))
          (children (car maps))
          (comm     (cdr maps))
          (agents   (make-hash-table :test 'equal :size (length entries))))
    (dolist (e entries)
      (when (agentmux--pid-tree-has-agent-p
              (plist-get e :pane-pid) children comm)
        (puthash (plist-get e :pane-id) t agents)))
    agents))

(defun agentmux--emamux-get-window ()
  "Get tmux windows ordered for target selection.
Sort priority: Emacs-hosting window first (when Emacs runs in tmux),
then the tmux-active window, then the rest in their tmux-reported order."
  (with-temp-buffer
    (emamux:tmux-run-command t "list-windows" "-t" emamux:session
      "-F" (concat "#{window_id}" agentmux--field-sep
             "#{window_index}: #{window_name} [#{window_id}]#{?window_active,*,}"))
    (let* ((lines (split-string (buffer-string) "\n" t))
            (entries (mapcar (lambda (line)
                               (let ((fields (split-string line agentmux--field-sep)))
                                 (cons (nth 0 fields) (nth 1 fields))))
                       lines))
            (emacs-win (agentmux--emacs-window-id)))
      (mapcar #'cdr
        (sort entries
          :key (lambda (e)
                 (cond
                   ((and emacs-win (equal (car e) emacs-win)) 0)
                   ((string-suffix-p "*" (cdr e)) 1)
                   (t 2)))
          :lessp #'<)))))

(defun agentmux--emamux-get-pane ()
  "Get tmux panes ordered for target selection.
Excludes Emacs's own pane entirely (preventing self-send and avoiding
visual noise). Sort priority among the remainder: panes whose process
tree runs an Agent CLI first, then the tmux-active pane, then others."
  (with-temp-buffer
    (emamux:tmux-run-command t "list-panes"
      "-t" (concat emamux:session ":" emamux:window)
      "-F" (concat "#{pane_id}" agentmux--field-sep
             "#{pane_pid}" agentmux--field-sep
             "#{pane_index}: #{pane_current_command} [#{pane_id}]#{?pane_active,*,}"))
    (let* ((lines (split-string (buffer-string) "\n" t))
            (entries (mapcar (lambda (line)
                               (let ((fields (split-string line agentmux--field-sep)))
                                 (list :pane-id  (nth 0 fields)
                                   :pane-pid (string-to-number (nth 1 fields))
                                   :display  (nth 2 fields))))
                       lines))
            (emacs-pane (agentmux--emacs-pane-id))
            (visible (if emacs-pane
                       (seq-remove
                         (lambda (e) (equal (plist-get e :pane-id) emacs-pane))
                         entries)
                       entries))
            (agents (when (> (length visible) 1)
                      (agentmux--compute-agent-panes visible))))
      (mapcar (lambda (e) (plist-get e :display))
        (sort visible
          :key (lambda (e)
                 (cond
                   ((and agents (gethash (plist-get e :pane-id) agents)) 0)
                   ((string-suffix-p "*" (plist-get e :display)) 1)
                   (t 2)))
          :lessp #'<)))))

(defun agentmux--emamux-read-parameter-window ()
  "Read window preserving agentmux's candidate order. Return the window index."
  (let ((candidates (emamux:get-window)))
    (unless candidates
      (user-error "No tmux windows available"))
    (let ((selected (if (= (length candidates) 1)
                      (car candidates)
                      (completing-read
                        "Window: "
                        (agentmux--completion-table-preserving-order candidates)
                        nil t))))
      (car (split-string selected ":")))))

(defun agentmux--emamux-read-parameter-pane ()
  "Read pane preserving agentmux's candidate order. Return the pane index."
  (let ((candidates (emamux:get-pane)))
    (unless candidates
      (user-error "No tmux panes available"))
    (let ((selected (if (= (length candidates) 1)
                      (car candidates)
                      (completing-read
                        "Input pane: "
                        (agentmux--completion-table-preserving-order candidates)
                        nil t))))
      (car (split-string selected ":")))))

(advice-add 'emamux:get-window :override #'agentmux--emamux-get-window)
(advice-add 'emamux:get-pane :override #'agentmux--emamux-get-pane)
(advice-add 'emamux:read-parameter-window :override #'agentmux--emamux-read-parameter-window)
(advice-add 'emamux:read-parameter-pane :override #'agentmux--emamux-read-parameter-pane)

(defun agentmux-unload-function ()
  "Remove advice added by agentmux when unloading the feature."
  (advice-remove 'emamux:get-window #'agentmux--emamux-get-window)
  (advice-remove 'emamux:get-pane #'agentmux--emamux-get-pane)
  (advice-remove 'emamux:read-parameter-window #'agentmux--emamux-read-parameter-window)
  (advice-remove 'emamux:read-parameter-pane #'agentmux--emamux-read-parameter-pane)
  nil)

;;; Core tmux interaction

(defconst agentmux--paste-buffer-name "agentmux-private"
  "Named tmux paste buffer used by agentmux to ferry text into the agent.
Carries an `agentmux-' prefix to avoid colliding with user-managed
buffers, and is always paired with `paste-buffer -d' so it is removed
immediately after use — the default paste stack is never touched.")

(defun agentmux--tmux-call-or-error (label &rest argv)
  "Run tmux ARGV; wrap any failure as a `user-error' tagged LABEL.
Existing `user-error' signals propagate verbatim so their messages are
not double-wrapped."
  (condition-case err
    (apply #'emamux:tmux-run-command nil argv)
    (user-error (signal (car err) (cdr err)))
    (error (user-error "Failed to %s: %s"
             label (error-message-string err)))))

(defun agentmux--ensure-parameters ()
  "Ensure emamux parameters are set.
Signals `user-error' if user cancels the setup or parameters are invalid."
  (unless (emamux:set-parameters-p)
    (emamux:set-parameters))
  (unless (emamux:set-parameters-p)
    (user-error "Tmux target not configured. Use M-x agentmux-set-target")))

(defun agentmux--send-text (text &optional no-enter)
  "Send TEXT to agent. Submit with Enter unless NO-ENTER is non-nil.

Issues at most two tmux calls regardless of TEXT length: one bracketed
paste (`set-buffer' + `paste-buffer -p -d' chained inside a single tmux
invocation) and one separate `send-keys Enter' for submission. Empty
TEXT skips the paste call.

The named buffer `agentmux--paste-buffer-name' is always coupled with
`paste-buffer -d' so it is deleted on use and the default paste stack
remains untouched.

Bracketed paste (`-p') makes the agent treat the entire payload as in-
input bytes — multi-line text lands as in-input newlines, not as
separate submitted messages. CRLF and bare CR are normalised to LF
before paste. The pattern `\\r\\n\\|\\r' must list `\\r\\n' first so a
bare `\\r' branch does not steal the match and split a CRLF into two
replacements."
  (agentmux--ensure-parameters)
  (let* ((target     (emamux:target-session))
          (normalized (replace-regexp-in-string "\r\n\\|\r" "\n" text t t)))
    (unless (string-empty-p normalized)
      (agentmux--tmux-call-or-error "paste to tmux"
        "set-buffer" "-b" agentmux--paste-buffer-name normalized
        ";"
        "paste-buffer" "-p" "-d"
        "-b" agentmux--paste-buffer-name
        "-t" target))
    (unless no-enter
      (agentmux--tmux-call-or-error "submit to tmux"
        "send-keys" "-t" target "Enter"))))

(defun agentmux--send-keys (key)
  "Send KEY to agent pane.
Signals `user-error' if tmux command fails."
  (agentmux--ensure-parameters)
  (agentmux--tmux-call-or-error "send key to tmux"
    "send-keys" "-t" (emamux:target-session) key))

;;; File context helpers

(defun agentmux--format-location-with-bounds (path-style include-line bounds)
  "Format location string with pre-computed BOUNDS.
PATH-STYLE: \\='git, \\='project, \\='absolute, \\='filename.
INCLUDE-LINE: whether to include line number.
BOUNDS: plist with :start and :end positions.
Returns nil if variable `buffer-file-name' is nil.
Return format: PATH, PATH:LINE, or PATH:START-END."
  (when buffer-file-name
    (let* ((path (cref--get-path-by-style path-style))
            (start-line (line-number-at-pos (plist-get bounds :start) t))
            (end-line (line-number-at-pos (plist-get bounds :end) t)))
      (cond
        ((not include-line) path)
        ((= start-line end-line) (format "%s:%d" path start-line))
        (t (format "%s:%d-%d" path start-line end-line))))))

(defun agentmux--format-content-with-bounds (bounds)
  "Format content with pre-computed BOUNDS as code fence.
BOUNDS: plist with :start and :end positions.
Returns region or current line wrapped in Markdown code fence."
  (when buffer-file-name
    (cref--get-region-content-with-fence bounds)))

(defun agentmux--context-parts ()
  "Return context parts as (location . content) cons or nil.
Both location and content come from the same bounds to ensure consistency.
Content is only included when `agentmux-context-include-content' is non-nil."
  (when buffer-file-name
    (let* ((bounds (cref--get-region-or-line))
            (location (agentmux--format-location-with-bounds
                        agentmux-context-path-style
                        agentmux-context-include-line
                        bounds))
            (content (when agentmux-context-include-content
                       (agentmux--format-content-with-bounds bounds))))
      (when location
        (cons location content)))))

;;; Prompt formatting

(defconst agentmux--ellipsis "..."
  "Ellipsis string used for truncation.")

(defun agentmux--truncate-location (location)
  "Truncate LOCATION if longer than `agentmux-prompt-max-location-length'."
  (let ((max-len agentmux-prompt-max-location-length)
         (ellipsis-len (length agentmux--ellipsis)))
    (if (> (length location) max-len)
      (concat agentmux--ellipsis
        (substring location (- (length location) (- max-len ellipsis-len))))
      location)))

(defun agentmux--make-prompt (location content)
  "Make prompt string from LOCATION and CONTENT.
LOCATION and CONTENT can be nil."
  (let ((suffix (cond
                  ((and location content)
                    (format "[%s + content]" (agentmux--truncate-location location)))
                  (location
                    (format "[%s]" (agentmux--truncate-location location)))
                  (t nil))))
    (if suffix
      (format "%s %s: " agentmux-agent-name suffix)
      (format "%s: " agentmux-agent-name))))

;;; Multiline input

(defun agentmux--minibuffer-submit-no-enter ()
  "Exit minibuffer with stage-only flag via throw.
Also adds input to history before exiting."
  (interactive)
  (let ((input (minibuffer-contents)))
    (when (and minibuffer-history-variable
            (not (string-empty-p input)))
      (add-to-history minibuffer-history-variable input))
    (throw 'agentmux-stage-only (cons input t))))

(defun agentmux--make-minibuffer-map ()
  "Create minibuffer keymap with current `agentmux-send-no-enter-key'."
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    (define-key map agentmux-send-no-enter-key #'agentmux--minibuffer-submit-no-enter)
    map))

(defun agentmux--format-key-description (key)
  "Format KEY description in short form.
Replaces <return> with RET for brevity."
  (string-replace "<return>" "RET" (key-description key)))

(defun agentmux--read-multiline-string (prompt &optional history)
  "Read a string from the minibuffer with multi-line support.
PROMPT is the prompt to display. HISTORY is the history list symbol to use.
Returns (TEXT . NO-ENTER-P) cons cell.
NO-ENTER-P is t if user pressed `agentmux-send-no-enter-key'."
  (let* ((key-hint (format "(%s: stage only)"
                     (agentmux--format-key-description agentmux-send-no-enter-key)))
          (full-prompt (concat prompt key-hint " ")))
    (catch 'agentmux-stage-only
      (cons (minibuffer-with-setup-hook
              (lambda ()
                (setq-local resize-mini-windows 'grow-only)
                ;; Pre-check whether the resize is permissible under the
                ;; current frame layout (mini-window first appearance,
                ;; tight frame, etc.) so we never call `window-resize'
                ;; when it would signal — that's an explicit condition
                ;; branch, not error suppression. When skipped,
                ;; `resize-mini-windows' grow-only still expands the
                ;; minibuffer to fit user input.
                (let ((delta (max 0 (- agentmux-minibuffer-initial-height 1))))
                  (when (and (> delta 0)
                          (window-resizable-p (selected-window) delta))
                    (window-resize (selected-window) delta))))
              (read-from-minibuffer full-prompt nil
                (agentmux--make-minibuffer-map) nil history))
        nil))))

(defun agentmux--validate-command (cmd)
  "Validate CMD is not empty, signal error if it is."
  (when (string-empty-p (string-trim cmd))
    (user-error "Command cannot be empty")))

;;; Interactive commands

;;;###autoload
(defun agentmux-set-target ()
  "Set target tmux pane for agent."
  (interactive)
  (emamux:set-parameters)
  (if (emamux:set-parameters-p)
    (message "%s target: %s" agentmux-agent-name (emamux:target-session))
    (message "%s target not set" agentmux-agent-name)))

;;;###autoload
(defun agentmux-send-command ()
  "Send command to agent (without context)."
  (interactive)
  (pcase-let* ((`(,cmd . ,no-enter)
                 (agentmux--read-multiline-string
                   (format "%s: " agentmux-agent-name)
                   'agentmux-input-history)))
    (agentmux--validate-command cmd)
    (agentmux--send-text cmd no-enter)
    (deactivate-mark)))

;;;###autoload
(defun agentmux-send-command-with-context ()
  "Send command with file:line context."
  (interactive)
  (pcase-let* ((`(,location . ,content) (agentmux--context-parts))
                (prompt (agentmux--make-prompt location content))
                (`(,cmd . ,no-enter)
                  (agentmux--read-multiline-string prompt 'agentmux-input-history)))
    (agentmux--validate-command cmd)
    (let ((full-context (when location
                          (if content
                            (format "%s\n%s" location content)
                            location))))
      (if full-context
        (agentmux--send-text (format "%s\n%s" full-context cmd) no-enter)
        (agentmux--send-text cmd no-enter)))
    (deactivate-mark)))

;;;###autoload
(defun agentmux-send-file-ref ()
  "Send current file location (path:line) to agent.
Uses `agentmux-context-path-style' for path format.
With active region, sends path:start-end."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer has no file"))
  (let ((location (agentmux--format-location-with-bounds
                    agentmux-context-path-style t
                    (cref--get-region-or-line))))
    (agentmux--send-text (format "\n%s\n" location) t)
    (deactivate-mark)))

;;;###autoload
(defun agentmux-send-file-path-only ()
  "Send current file path to agent.
Uses `agentmux-context-path-style' for path format.
Does not include line number or content."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer has no file"))
  (let ((path (cref--get-path-by-style agentmux-context-path-style)))
    (agentmux--send-text (format "\n%s\n" path) t)))

;;; Quick responses

(defun agentmux-send-return ()
  "Send Enter to agent (confirm)."
  (interactive)
  (agentmux--send-keys "Enter"))

(defun agentmux-send-escape ()
  "Send Escape to agent (cancel)."
  (interactive)
  (agentmux--send-keys "Escape"))

(defun agentmux--send-digit (digit)
  "Send DIGIT to agent (stage-only) and display message.
Routed through `agentmux--send-text' so that the digit travels the same
named-paste-buffer path as bulk text — accepts the extra round-trip in
exchange for one uniform send pipeline (no parallel `send-keys -l' code
path to keep in sync)."
  (agentmux--send-text digit t)
  (message "Sent: %s" digit))

(defun agentmux-send-digit ()
  "Read a digit (0-9) and send to agent."
  (interactive)
  (let ((char (read-char (format "Send digit to %s (0-9): " agentmux-agent-name))))
    (if (<= ?0 char ?9)
      (agentmux--send-digit (char-to-string char))
      (user-error "Expected digit 0-9, got: %c" char))))

(dolist (n '(1 2 3 4))
  (let ((digit (number-to-string n)))
    (defalias (intern (format "agentmux-send-%d" n))
      (lambda () (interactive)
        (agentmux--send-digit digit))
      (format "Send digit %s to agent." digit))))

;;; Menu navigation mode

(defun agentmux--menu-navigate (key symbol)
  "Send KEY to navigate menu and display SYMBOL."
  (agentmux--send-keys key)
  (message "%s" symbol))

(dolist (spec '(("Up" "↑" up)
                 ("Down" "↓" down)
                 ("Left" "←" left)
                 ("Right" "→" right)))
  (let ((key (nth 0 spec))
         (symbol (nth 1 spec))
         (name (nth 2 spec)))
    (defalias (intern (format "agentmux-menu-%s" name))
      (lambda () (interactive)
        (agentmux--menu-navigate key symbol))
      (format "Send %s arrow to navigate menu." key))))

(defun agentmux-menu-confirm ()
  "Send Enter to confirm selection."
  (interactive)
  (agentmux-send-return)
  (message "Confirmed"))

(defun agentmux-menu-cancel ()
  "Send Escape to cancel."
  (interactive)
  (agentmux-send-escape)
  (message "Cancelled"))

(defun agentmux-menu-input ()
  "Input custom text and send."
  (interactive)
  (let ((input (read-string "Input: ")))
    (unless (string-empty-p input)
      (agentmux--send-text input)
      (message "Sent: %s" input))))

;;;###autoload
(transient-define-prefix agentmux-menu-mode ()
  "Menu navigation mode for AI agent.
Navigate with hjkl or arrow keys, RET to confirm, ESC to cancel."
  :transient-suffix 'transient--do-stay
  [["Navigate"
     ("k" "↑ Up" agentmux-menu-up)
     ("j" "↓ Down" agentmux-menu-down)
     ("h" "← Left" agentmux-menu-left)
     ("l" "→ Right" agentmux-menu-right)
     ("<up>" "" agentmux-menu-up :if (lambda () nil))
     ("<down>" "" agentmux-menu-down :if (lambda () nil))
     ("<left>" "" agentmux-menu-left :if (lambda () nil))
     ("<right>" "" agentmux-menu-right :if (lambda () nil))]

    ["Action"
      ("RET" "Enter" agentmux-menu-confirm :transient nil)
      ("<escape>" "Escape" agentmux-menu-cancel :transient nil)
      ("i" "Input..." agentmux-menu-input)
      ("q" "Quit" transient-quit-one)]])

;;; Transient menu

(defun agentmux--path-style-description ()
  "Return description for current path style."
  (format "Path: %s" agentmux-context-path-style))

(defun agentmux--project-differs-from-git-p ()
  "Return non-nil if Emacs project root differs from Git root."
  (when-let* ((git-root (cref--get-git-root))
               (proj-root (cref--get-project-root)))
    (not (file-equal-p git-root proj-root))))

(defun agentmux--path-style-candidates ()
  "Return available path style candidates."
  (if (agentmux--project-differs-from-git-p)
    '("git" "project" "absolute" "filename")
    '("git" "absolute" "filename")))

(transient-define-infix agentmux--infix-path-style ()
  :class 'transient-lisp-variable
  :variable 'agentmux-context-path-style
  :description #'agentmux--path-style-description
  :reader (lambda (&rest _)
            (intern (completing-read "Path style: "
                      (agentmux--path-style-candidates)))))

(transient-define-infix agentmux--infix-include-line ()
  :class 'transient-lisp-variable
  :variable 'agentmux-context-include-line
  :description (lambda () (format "Line: %s" (if agentmux-context-include-line "yes" "no")))
  :reader (lambda (&rest _) (not agentmux-context-include-line)))

(transient-define-infix agentmux--infix-include-content ()
  :class 'transient-lisp-variable
  :variable 'agentmux-context-include-content
  :description (lambda () (format "Content: %s" (if agentmux-context-include-content "yes" "no")))
  :reader (lambda (&rest _) (not agentmux-context-include-content)))

;;;###autoload
(transient-define-prefix agentmux-transient ()
  "Agent commands via tmux."
  ["Options"
    ("-p" agentmux--infix-path-style)
    ("-l" agentmux--infix-include-line)
    ("-c" agentmux--infix-include-content)]

  [["Send"
     ("s" "Command" agentmux-send-command)
     ("x" "Command + context" agentmux-send-command-with-context)
     ("p" "File:line" agentmux-send-file-ref)
     ("P" "File path" agentmux-send-file-path-only)]

    ["Quick"
      ("1" "1" agentmux-send-1)
      ("2" "2" agentmux-send-2)
      ("3" "3" agentmux-send-3)
      ("4" "4" agentmux-send-4)]

    ["Reply"
      ("RET" "Enter" agentmux-send-return)
      ("<escape>" "Escape" agentmux-send-escape)
      ("k" "0-9" agentmux-send-digit)]

    ["Mode"
      ("m" "Menu mode" agentmux-menu-mode)]

    ["Action"
      ("t" "Set target" agentmux-set-target)
      ("q" "Quit" transient-quit-one)]]
  (interactive)
  (setq agentmux-context-path-style 'git
        agentmux-context-include-line t
        agentmux-context-include-content nil)
  (transient-setup 'agentmux-transient))

(provide 'agentmux)
;;; agentmux.el ends here
