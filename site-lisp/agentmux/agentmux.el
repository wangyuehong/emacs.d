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
;; - Fix errors at point using flymake diagnostics
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

(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostics "flymake")
(declare-function help-at-pt-kbd-string "help-at-pt")

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
Longer paths will be truncated with ellipsis. Minimum is 4 (3 for ellipsis + 1 char)."
  :type '(integer :match (lambda (_widget value) (>= value 4)))
  :group 'agentmux)

(defcustom agentmux-send-no-enter-key (kbd "C-<return>")
  "Key binding for stage only (send without Enter) in minibuffer."
  :type 'key-sequence
  :group 'agentmux)

(defvar agentmux-input-history nil
  "History of inputs sent to agent.")

;;; Emamux advice for better display and defaults

(defun agentmux--reorder-by-active (items active-first)
  "Reorder ITEMS by active marker (*).
If ACTIVE-FIRST, put active item first; otherwise last."
  (let* ((activep (lambda (x) (string-suffix-p "*" x)))
          (active (seq-find activep items))
          (others (seq-remove activep items)))
    (cond
      ((null active) others)
      (active-first (cons active others))
      (t (append others (list active))))))

(defun agentmux--emamux-get-window ()
  "Get tmux windows with name, active window first."
  (with-temp-buffer
    (emamux:tmux-run-command t "list-windows" "-t" emamux:session
      "-F" "#{window_index}: #{window_name}#{?window_active,*,}")
    (agentmux--reorder-by-active (split-string (buffer-string) "\n" t) t)))

(defun agentmux--emamux-get-pane ()
  "Get tmux panes with command, non-active panes first."
  (with-temp-buffer
    (emamux:tmux-run-command t "list-panes"
      "-t" (concat emamux:session ":" emamux:window)
      "-F" "#{pane_index}: #{pane_current_command}#{?pane_active,*,}")
    (agentmux--reorder-by-active (split-string (buffer-string) "\n" t) nil)))

(defun agentmux--emamux-read-parameter-pane ()
  "Read pane with better display, return pane index only."
  (let ((candidates (emamux:get-pane)))
    (unless candidates
      (user-error "No tmux panes available"))
    (let ((selected (if (= (length candidates) 1)
                      (car candidates)
                      (emamux:completing-read "Input pane: " candidates))))
      (car (split-string selected ":")))))

(advice-add 'emamux:get-window :override #'agentmux--emamux-get-window)
(advice-add 'emamux:get-pane :override #'agentmux--emamux-get-pane)
(advice-add 'emamux:read-parameter-pane :override #'agentmux--emamux-read-parameter-pane)

(defun agentmux-unload-function ()
  "Remove advice added by agentmux when unloading the feature."
  (advice-remove 'emamux:get-window #'agentmux--emamux-get-window)
  (advice-remove 'emamux:get-pane #'agentmux--emamux-get-pane)
  (advice-remove 'emamux:read-parameter-pane #'agentmux--emamux-read-parameter-pane)
  nil)

;;; Core tmux interaction

(defun agentmux--ensure-parameters ()
  "Ensure emamux parameters are set.
Signals `user-error' if user cancels the setup or parameters are invalid."
  (unless (emamux:set-parameters-p)
    (emamux:set-parameters))
  (unless (emamux:set-parameters-p)
    (user-error "Tmux target not configured. Use M-x agentmux-set-target")))

(defun agentmux--send-text (text &optional no-enter)
  "Send TEXT to agent via tmux buffer.
If NO-ENTER is non-nil, do not send Enter after text."
  (agentmux--ensure-parameters)
  (let ((target (emamux:target-session)))
    (condition-case err
        (progn
          (with-temp-buffer
            (insert text)
            (unless (zerop (call-process-region (point-min) (point-max)
                                                "tmux" nil nil nil "load-buffer" "-"))
              (error "tmux load-buffer failed")))
          (emamux:tmux-run-command nil "paste-buffer" "-d" "-t" target)
          (unless no-enter
            (agentmux--send-keys "Enter")))
      (error (user-error "Failed to send to tmux: %s" (error-message-string err))))))

(defun agentmux--send-keys (key)
  "Send KEY to agent pane.
Signals `user-error' if tmux command fails."
  (agentmux--ensure-parameters)
  (condition-case err
    (emamux:tmux-run-command nil "send-keys" "-t" (emamux:target-session) key)
    (error (user-error "Failed to send to tmux: %s" (error-message-string err)))))

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
Both location and content are computed from the same bounds to ensure consistency.
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

(defun agentmux--format-context ()
  "Format full context (location + content if enabled)."
  (when-let* ((parts (agentmux--context-parts)))
    (let ((location (car parts))
           (content (cdr parts)))
      (if content
        (format "%s\n%s" location content)
        location))))

;;; Error handling

(defun agentmux--get-flymake-diagnostics-on-line ()
  "Get all flymake diagnostics on the current line."
  (when (and (bound-and-true-p flymake-mode)
          (fboundp 'flymake-diagnostics))
    (flymake-diagnostics (line-beginning-position) (line-end-position))))

(defun agentmux--format-flymake-diagnostics (diagnostics)
  "Format flymake DIAGNOSTICS for display to agent.
Returns nil if DIAGNOSTICS is empty or all texts are nil."
  (let* (;; Filter out nil/empty diagnostic texts
          (texts (seq-remove #'string-empty-p
                   (seq-map (lambda (d)
                              (or (flymake-diagnostic-text d) ""))
                     diagnostics)))
          (n (length texts)))
    (cond
      ((= n 0) nil)
      ((= n 1) (car texts))
      ;; Show all if 3 or fewer errors for complete context
      ((<= n 3)
        (format "(%d errors: %s)" n (string-join texts "; ")))
      ;; Show first 2 with count if more than 3 to avoid overwhelming output
      (t
        (format "(%d errors including: %s; ...)"
          n (string-join (take 2 texts) "; "))))))

(defun agentmux--format-errors-at-point ()
  "Format errors at point for agent."
  (cond
    ((bound-and-true-p flymake-mode)
      (agentmux--format-flymake-diagnostics (agentmux--get-flymake-diagnostics-on-line)))
    (t
      (when-let* ((help-text (help-at-pt-kbd-string)))
        (substring-no-properties help-text)))))

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
                (ignore-errors
                  (window-resize (selected-window)
                    (max 0 (- agentmux-minibuffer-initial-height 1)))))
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
(defun agentmux-send-file-path ()
  "Send current file path to agent.
Uses `agentmux-context-path-style' for path format.
Does not include line number or content."
  (interactive)
  (unless buffer-file-name
    (user-error "Buffer has no file"))
  (let ((path (cref--get-path-by-style agentmux-context-path-style)))
    (agentmux--send-text (format "\n%s\n" path) t)))

;;;###autoload
(defun agentmux-fix-error-at-point ()
  "Ask agent to fix error at point."
  (interactive)
  (let* ((context (agentmux--format-context))
          (errors (agentmux--format-errors-at-point)))
    (unless errors
      (user-error "No errors at point"))
    (let ((cmd (if context
                 (format "Please fix the error at %s:\n%s" context errors)
                 (format "Please fix this error:\n%s" errors))))
      (agentmux--send-text cmd))
    (deactivate-mark)))

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
  "Send DIGIT to agent and display message."
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
Navigate with hjkl or arrow keys, confirm with y, cancel with n."
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
      ("y" "Confirm" agentmux-menu-confirm :transient nil)
      ("n" "Cancel" agentmux-menu-cancel :transient nil)
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
     ("p" "File path" agentmux-send-file-path)]

    ["Fix"
     ("f" "Fix error at point" agentmux-fix-error-at-point)]

    ["Digits"
      ("1" "1" agentmux-send-1)
      ("2" "2" agentmux-send-2)
      ("3" "3" agentmux-send-3)
      ("4" "4" agentmux-send-4)]

    ["More"
      ("k" "0-9" agentmux-send-digit)]

    ["Confirm"
      ("y" "Enter" agentmux-send-return)
      ("n" "Escape" agentmux-send-escape)
      ("m" "Menu mode" agentmux-menu-mode)]

    ["Target"
      ("t" "Set target pane" agentmux-set-target)]]
  (interactive)
  (setq agentmux-context-path-style 'git
        agentmux-context-include-line t
        agentmux-context-include-content nil)
  (transient-setup 'agentmux-transient))

(provide 'agentmux)
;;; agentmux.el ends here
