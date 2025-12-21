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
;; Supports any CLI-based AI agent. Default configuration is for Claude Code.
;; Configure via `agentmux-agent-name'.
;;
;; Features:
;; - Send commands with file context
;; - Fix errors at point using flymake diagnostics
;; - Quick digit input (0-9) for menu selections
;; - Menu navigation mode for multi-option interactions
;;
;; Usage:
;;   (require 'agentmux)
;;   (global-set-key (kbd "C-c c") 'agentmux-transient)

;;; Code:

(require 'emamux)
(require 'seq)
(require 'transient)

(declare-function flymake-diagnostic-text "flymake")
(declare-function flymake-diagnostics "flymake")
(declare-function vc-git-root "vc-git")

;;; Customization

(defgroup agentmux nil
  "Interact with AI agents via tmux."
  :group 'tools)

(defcustom agentmux-agent-name "Claude Code"
  "Display name for agent in prompts and messages."
  :type 'string
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
  (let* ((candidates (emamux:get-pane))
         (selected (if (= (length candidates) 1)
                       (car candidates)
                     (emamux:completing-read "Input pane: " candidates))))
    (car (split-string selected ":"))))

(advice-add 'emamux:get-window :override #'agentmux--emamux-get-window)
(advice-add 'emamux:get-pane :override #'agentmux--emamux-get-pane)
(advice-add 'emamux:read-parameter-pane :override #'agentmux--emamux-read-parameter-pane)

;;; Core tmux interaction

(defun agentmux--ensure-parameters ()
  "Ensure emamux parameters are set."
  (unless (emamux:set-parameters-p)
    (emamux:set-parameters)))

(defun agentmux--send-text (text &optional no-enter)
  "Send TEXT to agent via emamux.
If NO-ENTER is non-nil, do not send Enter after text."
  (agentmux--send-keys text)
  (unless no-enter
    (agentmux--send-keys "Enter")))

(defun agentmux--send-keys (key)
  "Send KEY to agent pane."
  (agentmux--ensure-parameters)
  (emamux:tmux-run-command nil "send-keys" "-t" (emamux:target-session) key))

;;; File context helpers

(defun agentmux--project-root ()
  "Get project root directory."
  (or (vc-git-root default-directory)
      default-directory))

(defun agentmux--relative-path ()
  "Get relative path of current file from project root."
  (when buffer-file-name
    (file-relative-name buffer-file-name (agentmux--project-root))))

(defun agentmux--format-context ()
  "Format file context as path:line or path:start-end."
  (when-let* ((path (agentmux--relative-path)))
    (if (use-region-p)
        (let* ((start (line-number-at-pos (region-beginning) t))
               (end (save-excursion
                      (goto-char (region-end))
                      (when (and (bolp) (not (bobp)))
                        (backward-char))
                      (line-number-at-pos (point) t))))
          (if (= start end)
              (format "%s:%d" path start)
            (format "%s:%d-%d" path start end)))
      (format "%s:%d" path (line-number-at-pos (point) t)))))

;;; Error handling

(defun agentmux--get-flymake-diagnostics-on-line ()
  "Get all flymake diagnostics on the current line."
  (when (and (bound-and-true-p flymake-mode)
             (fboundp 'flymake-diagnostics))
    (flymake-diagnostics (line-beginning-position) (line-end-position))))

(defun agentmux--format-flymake-diagnostics (diagnostics)
  "Format flymake DIAGNOSTICS for display to agent."
  (let ((n (length diagnostics)))
    (cond
     ((= n 0) nil)
     ((= n 1) (flymake-diagnostic-text (car diagnostics)))
     ((<= n 3)
      (format "(%d errors: %s)"
              n (mapconcat #'flymake-diagnostic-text diagnostics "; ")))
     (t
      (format "(%d errors including: %s; ...)"
              n (mapconcat #'flymake-diagnostic-text (seq-take diagnostics 2) "; "))))))

(defun agentmux--format-errors-at-point ()
  "Format errors at point for agent."
  (cond
   ((bound-and-true-p flymake-mode)
    (agentmux--format-flymake-diagnostics (agentmux--get-flymake-diagnostics-on-line)))
   (t
    (when-let* ((help-text (help-at-pt-kbd-string)))
      (substring-no-properties help-text)))))

;;; Multiline input

(defun agentmux--read-multiline-string (prompt &optional history)
  "Read a string from the minibuffer with multi-line support.
PROMPT is the prompt to display. Shift+Return inserts a newline.
HISTORY is the history list symbol to use."
  (let ((map (copy-keymap minibuffer-local-map)))
    (define-key map (kbd "S-<return>") #'newline)
    (define-key map (kbd "S-RET") #'newline)
    (read-from-minibuffer prompt nil map nil history)))

(defun agentmux--validate-command (cmd)
  "Validate CMD is not empty, signal error if it is."
  (when (string-empty-p (string-trim cmd))
    (user-error "Command cannot be empty")))

;;; Interactive commands

(defun agentmux-set-target ()
  "Set target tmux pane for agent."
  (interactive)
  (emamux:set-parameters)
  (message "%s target: %s" agentmux-agent-name (emamux:target-session)))

(defun agentmux-send-command ()
  "Send command to agent (without context)."
  (interactive)
  (let ((cmd (agentmux--read-multiline-string
              (format "%s: " agentmux-agent-name)
              'agentmux-input-history)))
    (agentmux--validate-command cmd)
    (agentmux--send-text cmd)))

(defun agentmux-send-command-with-context ()
  "Send command with file:line context."
  (interactive)
  (let* ((context (agentmux--format-context))
         (prompt (if context
                     (format "%s [%s]: " agentmux-agent-name context)
                   (format "%s: " agentmux-agent-name)))
         (cmd (agentmux--read-multiline-string prompt 'agentmux-input-history)))
    (agentmux--validate-command cmd)
    (if context
        (agentmux--send-text (format "File context: %s\n%s" context cmd))
      (agentmux--send-text cmd))
    (deactivate-mark)))

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
      (agentmux--send-text cmd))))

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

(transient-define-prefix agentmux-menu-mode ()
  "Menu navigation mode for Claude Code.
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

;;;###autoload
(transient-define-prefix agentmux-transient ()
  "Agent commands via tmux."
  [["Send"
    ("s" "Command" agentmux-send-command)
    ("x" "Command + context" agentmux-send-command-with-context)
    ("f" "Fix error" agentmux-fix-error-at-point)]

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
    ("t" "Set target pane" agentmux-set-target)]])

(provide 'agentmux)
;;; agentmux.el ends here
