;;; agentmux-test.el --- Tests for agentmux -*- lexical-binding: t; -*-

;;; Commentary:
;; ERT tests for agentmux. tmux interaction is mocked at the
;; `emamux:tmux-run-command' boundary; process tree queries are mocked at
;; the `process-file' boundary; tmux env detection is mocked at the
;; `agentmux--emacs-pane-id' / `agentmux--emacs-window-id' boundary.
;;
;; Each test docstring cites the SPEC AC it covers, when applicable.

;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'agentmux)

;;; Helpers

(defmacro agentmux-test-with-target (target &rest body)
  "Bind emamux session/window/pane defvars from TARGET like \"sess:1.0\"."
  (declare (indent 1))
  `(let* ((parts (split-string ,target "[:.]"))
          (emamux:session (nth 0 parts))
          (emamux:window  (nth 1 parts))
          (emamux:pane    (nth 2 parts)))
     ,@body))

(defmacro agentmux-test-with-tmux (handler &rest body)
  "Stub `emamux:tmux-run-command' so each call invokes HANDLER.
HANDLER receives the argv list and returns either a string (inserted into
the current buffer when output destination is t) or nil. All call argvs
accumulate in `agentmux-test--tmux-calls' (oldest-first)."
  (declare (indent 1))
  `(let ((agentmux-test--tmux-calls nil))
     (cl-letf (((symbol-function 'emamux:tmux-run-command)
                (lambda (output &rest args)
                  (setq agentmux-test--tmux-calls
                        (append agentmux-test--tmux-calls
                                (list (cons output args))))
                  (let ((response (funcall ,handler args)))
                    (when (and (eq output t) (stringp response))
                      (insert response)))
                  0)))
       ,@body)))

(defun agentmux-test--ps-output (rows)
  "Render ROWS as `ps -axo pid=,ppid=,comm=' style output.
Each ROW is (PID PPID COMM)."
  (mapconcat (lambda (row)
               (format "%d %d %s" (nth 0 row) (nth 1 row) (nth 2 row)))
             rows "\n"))

(defmacro agentmux-test-with-ps (rows &rest body)
  "Stub `process-file' so a call for `ps' returns ROWS."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'process-file)
              (lambda (program &optional _infile buffer _display &rest _args)
                (when (and (string= program "ps") (eq buffer t))
                  (insert (agentmux-test--ps-output ,rows)))
                0)))
     ,@body))

(defun agentmux-test--all-args (calls)
  "Flatten all argv tokens across CALLS."
  (apply #'append (mapcar #'cdr calls)))

(defun agentmux-test--flag-value (args flag)
  "Return the argv element immediately following FLAG in ARGS, or nil."
  (let ((pos (cl-position flag args :test #'equal)))
    (and pos (nth (1+ pos) args))))

;; ---------------------------------------------------------------------------
;;; Pure helpers: truncate-location
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-truncate-location/short-unchanged ()
  (let ((agentmux-prompt-max-location-length 40))
    (should (string= (agentmux--truncate-location "short/path.el")
                     "short/path.el"))))

(ert-deftest agentmux-test-truncate-location/exact-unchanged ()
  (let ((agentmux-prompt-max-location-length 5))
    (should (string= (agentmux--truncate-location "abcde") "abcde"))))

(ert-deftest agentmux-test-truncate-location/long-keeps-tail ()
  (let* ((agentmux-prompt-max-location-length 10)
         (result (agentmux--truncate-location "/very/long/path/file.el")))
    (should (= (length result) 10))
    (should (string-prefix-p "..." result))
    (should (string-suffix-p "file.el" result))))

;; ---------------------------------------------------------------------------
;;; Pure helpers: make-prompt
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-make-prompt/no-location ()
  (let ((agentmux-agent-name "claude"))
    (should (string= (agentmux--make-prompt nil nil) "claude: "))))

(ert-deftest agentmux-test-make-prompt/location-only ()
  (let ((agentmux-agent-name "claude")
        (agentmux-prompt-max-location-length 40))
    (should (string= (agentmux--make-prompt "src/foo.el:42" nil)
                     "claude [src/foo.el:42]: "))))

(ert-deftest agentmux-test-make-prompt/location-and-content ()
  (let ((agentmux-agent-name "claude")
        (agentmux-prompt-max-location-length 40))
    (should (string= (agentmux--make-prompt "src/foo.el:42" "```code```")
                     "claude [src/foo.el:42 + content]: "))))

(ert-deftest agentmux-test-make-prompt/long-location-truncated ()
  (let ((agentmux-agent-name "agent")
        (agentmux-prompt-max-location-length 12))
    (let ((result (agentmux--make-prompt "/long/path/file.el:99" nil)))
      (should (string-match-p "\\[\\.\\.\\." result))
      (should (string-suffix-p ": " result)))))

;; ---------------------------------------------------------------------------
;;; Pure helpers: format-key-description, validate-command
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-format-key-description/return-becomes-RET ()
  (should (string-match-p "RET"
                          (agentmux--format-key-description (kbd "C-<return>")))))

(ert-deftest agentmux-test-format-key-description/no-return ()
  (should (string= (agentmux--format-key-description (kbd "C-c"))
                   "C-c")))

(ert-deftest agentmux-test-validate-command/empty-errors ()
  (should-error (agentmux--validate-command "") :type 'user-error))

(ert-deftest agentmux-test-validate-command/whitespace-errors ()
  (should-error (agentmux--validate-command "   \t  ") :type 'user-error))

(ert-deftest agentmux-test-validate-command/non-empty-silent ()
  (should (null (agentmux--validate-command "do something"))))

;; ---------------------------------------------------------------------------
;;; ensure-parameters (US-0060)
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-ensure-parameters/already-set-noop ()
  (let ((emamux:session "s") (emamux:window "1") (emamux:pane "0")
        (set-called nil))
    (cl-letf (((symbol-function 'emamux:set-parameters)
               (lambda () (setq set-called t))))
      (agentmux--ensure-parameters)
      (should-not set-called))))

(ert-deftest agentmux-test-ensure-parameters/not-set-errors ()
  "AC-0060-0020: signals user-error when target stays unconfigured."
  (let ((emamux:session nil) (emamux:window nil) (emamux:pane nil))
    (cl-letf (((symbol-function 'emamux:set-parameters) (lambda () nil)))
      (should-error (agentmux--ensure-parameters) :type 'user-error))))

;; ---------------------------------------------------------------------------
;;; send-text (US-0010)
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-send-text/single-line ()
  "AC-0010-0010: single-line text loaded into the private paste buffer
then bracket-pasted, with a separate Enter for submission."
  (agentmux-test-with-target "sess:1.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "hello")
      (should (equal agentmux-test--tmux-calls
                     `((nil "set-buffer" "-b" ,agentmux--paste-buffer-name "hello"
                        ";"
                        "paste-buffer" "-p" "-d"
                        "-b" ,agentmux--paste-buffer-name
                        "-t" "sess:1.0")
                       (nil "send-keys" "-t" "sess:1.0" "Enter")))))))

(ert-deftest agentmux-test-send-text/multiline-paste-with-newlines ()
  "AC-0010-0020: multi-line text rides through bracketed paste in one
payload — no per-line splitting, no per-line tmux subcommands."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "a\nb\nc")
      (let* ((paste-args (cdr (nth 0 agentmux-test--tmux-calls)))
             (data       (agentmux-test--flag-value paste-args "-b")))
        (should (string= data agentmux--paste-buffer-name))
        ;; The DATA token follows the buffer name in `set-buffer'.
        (should (string= (nth 3 paste-args) "a\nb\nc"))))))

(ert-deftest agentmux-test-send-text/special-chars-preserved ()
  "AC-0010-0030: literal bytes are stuffed into the paste buffer
verbatim — `;', `~', `\\', `#{...}', CJK, emoji and friends."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (let ((payload "hi; ls #{foo} ~user \\n 漢字 😀"))
        (agentmux--send-text payload)
        (let ((paste-args (cdr (nth 0 agentmux-test--tmux-calls))))
          (should (string= (nth 3 paste-args) payload)))))))

(ert-deftest agentmux-test-send-text/no-enter-stage-only ()
  "AC-0010-0040: stage-only fires the paste call but skips the Enter call."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "stage me" t)
      (should (= (length agentmux-test--tmux-calls) 1))
      (should-not (member "Enter"
                          (agentmux-test--all-args agentmux-test--tmux-calls))))))

(ert-deftest agentmux-test-send-text/exactly-two-calls-for-non-empty ()
  "AC-0010-0100: non-empty + non-stage triggers exactly 2 tmux calls
regardless of line count (200-line payload as the witness)."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (let ((payload (mapconcat #'number-to-string (number-sequence 1 200) "\n")))
        (agentmux--send-text payload)
        (should (= (length agentmux-test--tmux-calls) 2))))))

(ert-deftest agentmux-test-send-text/paste-and-enter-never-chained ()
  "AC-0010-0100: paste call must not chain `send-keys Enter' inside its
own argv. Implementation experiment: chaining triggers a Claude Code
`useInput'/`usePaste' race that drops the trailing Enter, leaving the
pasted text stuck in the input box."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "line1\nline2")
      (let* ((paste-args (cdr (nth 0 agentmux-test--tmux-calls)))
             (enter-args (cdr (nth 1 agentmux-test--tmux-calls))))
        ;; The paste call itself must NOT contain Enter or send-keys.
        (should-not (member "Enter" paste-args))
        (should-not (member "send-keys" paste-args))
        ;; The submit call is a separate, dedicated send-keys Enter.
        (should (equal enter-args
                       '("send-keys" "-t" "T:0.0" "Enter")))))))

(ert-deftest agentmux-test-send-text/uses-private-named-paste-buffer ()
  "AC-0010-0060: the agentmux-private paste buffer is named (not the
default stack) and is `-d'-deleted on use, so the user's paste stack
is not touched."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "anything\nmultiline")
      (let* ((paste-args (cdr (nth 0 agentmux-test--tmux-calls))))
        ;; Both set-buffer and paste-buffer reference the named buffer.
        (should (= (cl-count agentmux--paste-buffer-name paste-args
                              :test #'equal)
                   2))
        ;; -d (delete after paste) is present.
        (should (member "-d" paste-args))
        ;; -p (bracketed paste) is present so multi-line stays in-input.
        (should (member "-p" paste-args))
        ;; No invocation that would touch the default paste stack
        ;; (i.e. set-buffer or paste-buffer without `-b').
        (dolist (call agentmux-test--tmux-calls)
          (let ((args (cdr call)))
            (when (or (member "set-buffer" args)
                      (member "paste-buffer" args))
              (should (member "-b" args)))))))))

(ert-deftest agentmux-test-send-text/empty-stage-noop ()
  "AC-0010-0070: empty + stage-only triggers no tmux call at all."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "" t)
      (should (null agentmux-test--tmux-calls)))))

(ert-deftest agentmux-test-send-text/empty-non-stage-sends-only-enter ()
  "AC-0010-0080: empty + non-stage skips paste, sends only Enter."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "")
      (should (equal agentmux-test--tmux-calls
                     '((nil "send-keys" "-t" "T:0.0" "Enter"))))
      (let ((args (cdar agentmux-test--tmux-calls)))
        (should-not (member "set-buffer" args))
        (should-not (member "paste-buffer" args))))))

(ert-deftest agentmux-test-send-text/crlf-normalized ()
  "AC-0010-0090: CRLF in the input collapses to LF in the paste payload —
no bare CR ever reaches the agent."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "a\r\nb")
      (let ((paste-args (cdr (nth 0 agentmux-test--tmux-calls))))
        (should (string= (nth 3 paste-args) "a\nb"))
        (should-not (string-match-p "\r" (nth 3 paste-args)))))))

(ert-deftest agentmux-test-send-text/bare-cr-normalized ()
  "AC-0010-0090: bare CR in the input normalises to LF in the paste payload."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-text "a\rb")
      (let ((paste-args (cdr (nth 0 agentmux-test--tmux-calls))))
        (should (string= (nth 3 paste-args) "a\nb"))))))

(ert-deftest agentmux-test-send-text/tmux-error-becomes-user-error ()
  "Generic `error' from tmux is wrapped with a labelled `user-error'."
  (agentmux-test-with-target "T:0.0"
    (cl-letf (((symbol-function 'emamux:tmux-run-command)
               (lambda (&rest _) (error "tmux exploded"))))
      (let ((err (should-error (agentmux--send-text "hi") :type 'user-error)))
        (should (string-match-p "Failed to paste to tmux"
                                (error-message-string err)))
        (should (string-match-p "tmux exploded"
                                (error-message-string err)))))))

(ert-deftest agentmux-test-send-text/tmux-user-error-passes-through ()
  "A `user-error' from tmux propagates verbatim — no double wrapping."
  (agentmux-test-with-target "T:0.0"
    (cl-letf (((symbol-function 'emamux:tmux-run-command)
               (lambda (&rest _) (user-error "no tmux session"))))
      (let ((err (should-error (agentmux--send-text "hi") :type 'user-error)))
        (let ((msg (error-message-string err)))
          (should (string-match-p "no tmux session" msg))
          (should-not (string-match-p "Failed to paste" msg)))))))

;; ---------------------------------------------------------------------------
;;; send-keys (US-0040)
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-send-keys/passes-key-as-arg ()
  "AC-0040-0010: keypress reaches pane verbatim, not via the text path."
  (agentmux-test-with-target "T:0.0"
    (agentmux-test-with-tmux (lambda (_) nil)
      (agentmux--send-keys "Escape")
      (should (equal agentmux-test--tmux-calls
                     '((nil "send-keys" "-t" "T:0.0" "Escape")))))))

(ert-deftest agentmux-test-send-keys/error-becomes-user-error ()
  (agentmux-test-with-target "T:0.0"
    (cl-letf (((symbol-function 'emamux:tmux-run-command)
               (lambda (&rest _) (error "boom"))))
      (should-error (agentmux--send-keys "Enter") :type 'user-error))))

;; ---------------------------------------------------------------------------
;;; pid-tree-has-agent-p
;; ---------------------------------------------------------------------------

(defun agentmux-test--build-maps (rows)
  "Build (CHILDREN-MAP . COMM-MAP) from ROWS of (PID PPID COMM)."
  (let ((children (make-hash-table :test 'eql))
        (comm     (make-hash-table :test 'eql)))
    (dolist (row rows)
      (let ((pid (nth 0 row)) (ppid (nth 1 row)) (c (nth 2 row)))
        (push pid (gethash ppid children))
        (puthash pid c comm)))
    (cons children comm)))

(ert-deftest agentmux-test-pid-tree/root-is-agent ()
  (let* ((agentmux-agent-cli-commands '("claude"))
         (m (agentmux-test--build-maps '((100 1 "claude")))))
    (should (agentmux--pid-tree-has-agent-p 100 (car m) (cdr m)))))

(ert-deftest agentmux-test-pid-tree/descendant-is-agent ()
  (let* ((agentmux-agent-cli-commands '("claude"))
         (m (agentmux-test--build-maps
             '((100 1 "zsh") (200 100 "node") (300 200 "claude")))))
    (should (agentmux--pid-tree-has-agent-p 100 (car m) (cdr m)))))

(ert-deftest agentmux-test-pid-tree/no-agent ()
  (let* ((agentmux-agent-cli-commands '("claude"))
         (m (agentmux-test--build-maps
             '((100 1 "zsh") (200 100 "vim")))))
    (should-not (agentmux--pid-tree-has-agent-p 100 (car m) (cdr m)))))

(ert-deftest agentmux-test-pid-tree/cycle-safe ()
  "Visited set must guard against pathological ps snapshots with cycles."
  (let* ((agentmux-agent-cli-commands '("claude"))
         (m (agentmux-test--build-maps
             '((100 200 "a") (200 100 "b")))))
    (should-not (agentmux--pid-tree-has-agent-p 100 (car m) (cdr m)))))

(ert-deftest agentmux-test-pid-tree/comm-basename-matched ()
  "Comm map values are already basenames; matching is exact, not substring."
  (let* ((agentmux-agent-cli-commands '("claude"))
         (m (agentmux-test--build-maps '((100 1 "claudette")))))
    (should-not (agentmux--pid-tree-has-agent-p 100 (car m) (cdr m)))))

;; ---------------------------------------------------------------------------
;;; process-maps
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-process-maps/parses-rows ()
  (agentmux-test-with-ps '((100 1 "launchd") (200 100 "zsh") (300 200 "claude"))
    (let* ((maps (agentmux--process-maps))
           (children (car maps))
           (comm (cdr maps)))
      (should (string= (gethash 300 comm) "claude"))
      (should (member 200 (gethash 100 children)))
      (should (member 300 (gethash 200 children))))))

(ert-deftest agentmux-test-process-maps/comm-with-spaces ()
  "Multi-token comm fields after pid/ppid join back to a path basename."
  (cl-letf (((symbol-function 'process-file)
             (lambda (program &optional _i buffer &rest _r)
               (when (and (string= program "ps") (eq buffer t))
                 (insert "100 1 /usr/local/bin/claude code\n"))
               0)))
    (let ((comm (cdr (agentmux--process-maps))))
      (should (string= (gethash 100 comm) "claude code")))))

(ert-deftest agentmux-test-process-maps/ps-failure-fail-fast ()
  "Fail-fast principle: ps non-zero exit must not return empty maps as a
silent fallback (which would mis-classify all panes as non-agent)."
  (cl-letf (((symbol-function 'process-file) (lambda (&rest _) 1)))
    (should-error (agentmux--process-maps) :type 'user-error)))

;; ---------------------------------------------------------------------------
;;; emacs-window-id (US-0070 boundary)
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-emacs-window-id/no-tmux-pane ()
  "AC-0070-0040: outside tmux, return nil so callers degrade."
  (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () nil)))
    (should (null (agentmux--emacs-window-id)))))

(ert-deftest agentmux-test-emacs-window-id/tmux-success ()
  (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () "%19")))
    (agentmux-test-with-tmux (lambda (_) "@8\n")
      (should (string= (agentmux--emacs-window-id) "@8")))))

(ert-deftest agentmux-test-emacs-window-id/tmux-error-fail-fast ()
  "AC-0070-0060: TMUX_PANE set but tmux query failure must user-error,
never silently fall back to the no-tmux path. The wrapped user-error
adds a `Failed to query Emacs window id' label so the user sees both
the contextual prefix and the original tmux message."
  (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () "%19"))
            ((symbol-function 'emamux:tmux-run-command)
             (lambda (&rest _) (error "tmux missing"))))
    (let ((err (should-error (agentmux--emacs-window-id) :type 'user-error)))
      (should (string-match-p "Failed to query Emacs window id"
                              (error-message-string err))))))

(ert-deftest agentmux-test-emacs-window-id/empty-output-passes-through ()
  "AC-0070-0060: an internally-raised `user-error' (empty tmux output)
must propagate verbatim — no double-wrapping by the outer handler."
  (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () "%19")))
    (agentmux-test-with-tmux (lambda (_) "\n")
      (let ((err (should-error (agentmux--emacs-window-id) :type 'user-error)))
        (let ((msg (error-message-string err)))
          (should (string-match-p "tmux returned empty window id" msg))
          ;; Must NOT carry the outer "Failed to query…" wrapper prefix.
          (should-not (string-match-p "Failed to query Emacs window id"
                                      msg)))))))

;; ---------------------------------------------------------------------------
;;; emamux-get-window (US-0070)
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-get-window/emacs-window-first ()
  "AC-0070-0010: Emacs-hosting window leads the candidate list."
  (cl-letf (((symbol-function 'agentmux--emacs-window-id) (lambda () "@2")))
    (agentmux-test-with-target "sess:1.0"
      (agentmux-test-with-tmux
          (lambda (_)
            (concat "@1" agentmux--field-sep "1: shell [@1]\n"
                    "@2" agentmux--field-sep "2: emacs [@2]*\n"
                    "@3" agentmux--field-sep "3: web [@3]\n"))
        (let ((result (agentmux--emamux-get-window)))
          (should (= (length result) 3))
          (should (string= (car result) "2: emacs [@2]*")))))))

(ert-deftest agentmux-test-get-window/no-emacs-active-then-others ()
  "AC-0070-0040: without an Emacs window, sort by tmux-active first."
  (cl-letf (((symbol-function 'agentmux--emacs-window-id) (lambda () nil)))
    (agentmux-test-with-target "sess:1.0"
      (agentmux-test-with-tmux
          (lambda (_)
            (concat "@1" agentmux--field-sep "1: shell [@1]\n"
                    "@2" agentmux--field-sep "2: emacs [@2]*\n"
                    "@3" agentmux--field-sep "3: web [@3]\n"))
        (let ((result (agentmux--emamux-get-window)))
          (should (string= (car result) "2: emacs [@2]*")))))))

;; ---------------------------------------------------------------------------
;;; emamux-get-pane (US-0070)
;; ---------------------------------------------------------------------------

(defun agentmux-test--list-panes-output (rows)
  "Render ROWS as the format string `agentmux--emamux-get-pane' expects.
ROW: (PANE-ID PANE-PID INDEX CMD ACTIVE-P)."
  (mapconcat
   (lambda (row)
     (let* ((id (nth 0 row)) (pid (nth 1 row)) (idx (nth 2 row))
            (cmd (nth 3 row)) (act (nth 4 row)))
       (concat id agentmux--field-sep
               (number-to-string pid) agentmux--field-sep
               (format "%d: %s [%s]%s" idx cmd id (if act "*" "")))))
   rows "\n"))

(ert-deftest agentmux-test-get-pane/emacs-pane-excluded ()
  "AC-0070-0020: Emacs's own pane is removed from the candidate list."
  (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () "%19")))
    (agentmux-test-with-target "sess:1.0"
      (agentmux-test-with-tmux
          (lambda (_)
            (agentmux-test--list-panes-output
             '(("%19" 100 0 "emacs" t)
               ("%20" 200 1 "zsh" nil))))
        (let ((result (agentmux--emamux-get-pane)))
          (should (= (length result) 1))
          (should (string= (car result) "1: zsh [%20]"))
          (dolist (entry result)
            (should-not (string-match-p "emacs" entry))))))))

(ert-deftest agentmux-test-get-pane/agent-promoted-among-visible ()
  "AC-0070-0030: among visible panes (Emacs already excluded), agent CLI
panes lead, then tmux-active, then others."
  (let ((agentmux-agent-cli-commands '("claude")))
    (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () "%19")))
      (agentmux-test-with-target "sess:1.0"
        (agentmux-test-with-ps '((100 1 "emacs")
                                 (200 1 "zsh")
                                 (300 1 "claude"))
          (agentmux-test-with-tmux
              (lambda (_)
                (agentmux-test--list-panes-output
                 '(("%19" 100 0 "emacs" t)
                   ("%20" 200 1 "zsh" nil)
                   ("%21" 300 2 "claude" nil))))
            (let ((result (agentmux--emamux-get-pane)))
              (should (= (length result) 2))
              (should (string-match-p "claude" (car result)))
              (should (string-match-p "zsh" (cadr result)))
              (dolist (entry result)
                (should-not (string-match-p "emacs" entry))))))))))

(ert-deftest agentmux-test-get-pane/single-visible-skips-ps ()
  "Fast path: when Emacs exclusion leaves ≤1 visible pane, skip ps."
  (let ((ps-called nil))
    (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () "%19"))
              ((symbol-function 'process-file)
               (lambda (program &rest _)
                 (when (string= program "ps") (setq ps-called t))
                 0)))
      (agentmux-test-with-target "sess:1.0"
        (agentmux-test-with-tmux
            (lambda (_)
              (agentmux-test--list-panes-output
               '(("%19" 100 0 "emacs" t)
                 ("%20" 200 1 "zsh" nil))))
          (agentmux--emamux-get-pane)
          (should-not ps-called))))))

(ert-deftest agentmux-test-get-pane/no-emacs-pane-shows-all ()
  "AC-0070-0040: without TMUX_PANE, no exclusion happens and active leads."
  (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () nil)))
    (agentmux-test-with-target "sess:1.0"
      (agentmux-test-with-tmux
          (lambda (_)
            (agentmux-test--list-panes-output
             '(("%20" 200 0 "shell" t)
               ("%21" 300 1 "top" nil))))
        (let ((result (agentmux--emamux-get-pane)))
          (should (= (length result) 2))
          (should (string-match-p "shell" (car result)))
          (should (string-match-p "top" (cadr result))))))))

(ert-deftest agentmux-test-get-pane/no-emacs-pane-agent-still-promoted ()
  "AC-0070-0040 + AC-0070-0030: without TMUX_PANE, Agent CLI promotion
still applies to all candidate panes (it has no Emacs-pane dependency)."
  (let ((agentmux-agent-cli-commands '("claude")))
    (cl-letf (((symbol-function 'agentmux--emacs-pane-id) (lambda () nil)))
      (agentmux-test-with-target "sess:1.0"
        (agentmux-test-with-ps '((200 1 "zsh") (300 1 "claude"))
          (agentmux-test-with-tmux
              (lambda (_)
                (agentmux-test--list-panes-output
                 '(("%20" 200 0 "zsh" t)
                   ("%21" 300 1 "claude" nil))))
            (let ((result (agentmux--emamux-get-pane)))
              (should (= (length result) 2))
              ;; Agent CLI wins over the tmux-active marker.
              (should (string-match-p "claude" (car result)))
              (should (string-match-p "zsh" (cadr result))))))))))

;; ---------------------------------------------------------------------------
;;; completion-table-preserving-order (US-0070 AC-0070-0070)
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-completion-table/preserves-order-via-metadata ()
  "AC-0070-0070: metadata declares identity sort so vertico/ivy do not
re-rank the candidates."
  (let* ((candidates '("0: emacs [%19]*" "1: zsh [%20]"))
         (table (agentmux--completion-table-preserving-order candidates))
         (meta  (funcall table "" nil 'metadata)))
    (should (eq (car meta) 'metadata))
    (should (eq (cdr (assq 'display-sort-function meta)) #'identity))
    (should (eq (cdr (assq 'cycle-sort-function meta)) #'identity))))

(ert-deftest agentmux-test-completion-table/delegates-completion ()
  "Non-metadata actions delegate to `complete-with-action'.
Order is intentionally not asserted here — the preceding test covers
`display-sort-function' / `cycle-sort-function' identity wiring."
  (let* ((candidates '("alpha" "beta" "gamma"))
         (table (agentmux--completion-table-preserving-order candidates)))
    (should (equal (all-completions "" table nil) candidates))
    (should (equal (try-completion "al" table) "alpha"))))

;; ---------------------------------------------------------------------------
;;; read-parameter-window / read-parameter-pane
;; ---------------------------------------------------------------------------

(ert-deftest agentmux-test-read-window/single-auto-select ()
  (cl-letf (((symbol-function 'emamux:get-window) (lambda () '("2: emacs"))))
    (should (string= (agentmux--emamux-read-parameter-window) "2"))))

(ert-deftest agentmux-test-read-window/multi-prompts-with-preserved-order ()
  "AC-0070-0070: completing-read receives a metadata-bearing table."
  (let (got-collection)
    (cl-letf (((symbol-function 'emamux:get-window)
               (lambda () '("2: emacs" "0: shell")))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq got-collection collection)
                 (car (all-completions "" collection nil)))))
      (agentmux--emamux-read-parameter-window)
      (let ((meta (funcall got-collection "" nil 'metadata)))
        (should (eq (cdr (assq 'display-sort-function meta)) #'identity))))))

(ert-deftest agentmux-test-read-window/empty-errors ()
  (cl-letf (((symbol-function 'emamux:get-window) (lambda () nil)))
    (should-error (agentmux--emamux-read-parameter-window) :type 'user-error)))

(ert-deftest agentmux-test-read-pane/single-auto-select ()
  (cl-letf (((symbol-function 'emamux:get-pane) (lambda () '("3: claude"))))
    (should (string= (agentmux--emamux-read-parameter-pane) "3"))))

(ert-deftest agentmux-test-read-pane/multi-prompts-with-preserved-order ()
  "AC-0070-0070: completing-read receives a metadata-bearing table."
  (let (got-collection)
    (cl-letf (((symbol-function 'emamux:get-pane)
               (lambda () '("1: claude" "0: shell")))
              ((symbol-function 'completing-read)
               (lambda (_prompt collection &rest _)
                 (setq got-collection collection)
                 (car (all-completions "" collection nil)))))
      (agentmux--emamux-read-parameter-pane)
      (let ((meta (funcall got-collection "" nil 'metadata)))
        (should (eq (cdr (assq 'display-sort-function meta)) #'identity))))))

(ert-deftest agentmux-test-read-pane/empty-errors ()
  (cl-letf (((symbol-function 'emamux:get-pane) (lambda () nil)))
    (should-error (agentmux--emamux-read-parameter-pane) :type 'user-error)))

(provide 'agentmux-test)
;;; agentmux-test.el ends here
