;;; terminal-browser-embed.el --- Browser inside a TUI Emacs window -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Yuehong Wang <wangyuehong@gmail.com>

;;; Commentary:
;; Display a URL through terminal-browser and Kitty graphics in an Emacs window.

;;; Code:
(require 'cl-lib)
(require 'json)
(require 'subr-x)
(require 'term/xterm)
(require 'xt-mouse)

(defvar evil-buffer-regexps)

(with-eval-after-load 'evil
  (add-to-list 'evil-buffer-regexps
               '("^\\*Terminal browser\\*\\(<[0-9]+>\\)?$" . nil)))

(cl-defstruct my/tb-browser
  origin point buffer window frame environment tmux cell size image directory
  socket server connection child relay timer mouse-owned log stderr
  (graphics "") (pending "") visible stopping on-close)

(defvar-local my/tb-browser-state nil
  "State owned by this browser buffer.")

(defvar my/tb-browsers nil
  "Active embedded browsers.")

;; Kitty v0.43.1 gen/rowcolumn-diacritics.txt, derived from Unicode 6.0.0.
;; Subsequent columns inherit their indices, as specified by the protocol.
(defconst my/tb-rows
  [#x0305 #x030d #x030e #x0310 #x0312 #x033d #x033e #x033f
   #x0346 #x034a #x034b #x034c #x0350 #x0351 #x0352 #x0357
   #x035b #x0363 #x0364 #x0365 #x0366 #x0367 #x0368 #x0369
   #x036a #x036b #x036c #x036d #x036e #x036f #x0483 #x0484
   #x0485 #x0486 #x0487 #x0592 #x0593 #x0594 #x0595 #x0597
   #x0598 #x0599 #x059c #x059d #x059e #x059f #x05a0 #x05a1
   #x05a8 #x05a9 #x05ab #x05ac #x05af #x05c4 #x0610 #x0611
   #x0612 #x0613 #x0614 #x0615 #x0616 #x0617 #x0657 #x0658
   #x0659 #x065a #x065b #x065d #x065e #x06d6 #x06d7 #x06d8
   #x06d9 #x06da #x06db #x06dc #x06df #x06e0 #x06e1 #x06e2
   #x06e4 #x06e7 #x06e8 #x06eb #x06ec #x0730 #x0732 #x0733
   #x0735 #x0736 #x073a #x073d #x073f #x0740 #x0741 #x0743
   #x0745 #x0747 #x0749 #x074a #x07eb #x07ec #x07ed #x07ee
   #x07ef #x07f0 #x07f1 #x07f3 #x0816 #x0817 #x0818 #x0819
   #x081b #x081c #x081d #x081e #x081f #x0820 #x0821 #x0822
   #x0823 #x0825 #x0826 #x0827 #x0829 #x082a #x082b #x082c
   #x082d #x0951 #x0953 #x0954 #x0f82 #x0f83 #x0f86 #x0f87
   #x135d #x135e #x135f #x17dd #x193a #x1a17 #x1a75 #x1a76
   #x1a77 #x1a78 #x1a79 #x1a7a #x1a7b #x1a7c #x1b6b #x1b6d
   #x1b6e #x1b6f #x1b70 #x1b71 #x1b72 #x1b73 #x1cd0 #x1cd1
   #x1cd2 #x1cda #x1cdb #x1ce0 #x1dc0 #x1dc1 #x1dc3 #x1dc4
   #x1dc5 #x1dc6 #x1dc7 #x1dc8 #x1dc9 #x1dcb #x1dcc #x1dd1
   #x1dd2 #x1dd3 #x1dd4 #x1dd5 #x1dd6 #x1dd7 #x1dd8 #x1dd9
   #x1dda #x1ddb #x1ddc #x1ddd #x1dde #x1ddf #x1de0 #x1de1
   #x1de2 #x1de3 #x1de4 #x1de5 #x1de6 #x1dfe #x20d0 #x20d1
   #x20d4 #x20d5 #x20d6 #x20d7 #x20db #x20dc #x20e1 #x20e7
   #x20e9 #x20f0 #x2cef #x2cf0 #x2cf1 #x2de0 #x2de1 #x2de2
   #x2de3 #x2de4 #x2de5 #x2de6 #x2de7 #x2de8 #x2de9 #x2dea
   #x2deb #x2dec #x2ded #x2dee #x2def #x2df0 #x2df1 #x2df2
   #x2df3 #x2df4 #x2df5 #x2df6 #x2df7 #x2df8 #x2df9 #x2dfa
   #x2dfb #x2dfc #x2dfd #x2dfe #x2dff #xa66f #xa67c #xa67d
   #xa6f0 #xa6f1 #xa8e0 #xa8e1 #xa8e2 #xa8e3 #xa8e4 #xa8e5
   #xa8e6 #xa8e7 #xa8e8 #xa8e9 #xa8ea #xa8eb #xa8ec #xa8ed
   #xa8ee #xa8ef #xa8f0 #xa8f1 #xaab0 #xaab2 #xaab3 #xaab7
   #xaab8 #xaabe #xaabf #xaac1 #xfe20 #xfe21 #xfe22 #xfe23
   #xfe24 #xfe25 #xfe26 #x10a0f #x10a38 #x1d185 #x1d186 #x1d187
   #x1d188 #x1d189 #x1d1aa #x1d1ab #x1d1ac #x1d1ad #x1d242 #x1d243 #x1d244]
  "Kitty placeholder row indices supported by the preview.")
(defun my/tb-terminal-sequence (sequence tmux)
  "Wrap SEQUENCE in TMUX passthrough when non-nil."
  (if tmux
      (concat "\ePtmux;" (replace-regexp-in-string "\e" "\e\e" sequence t t) "\e\\")
    sequence))

(defun my/tb-frame-environment (frame)
  "Return FRAME's client environment, or the direct Emacs environment."
  (or (frame-parameter frame 'environment)
      (if (daemonp)
          (user-error "Terminal browser cannot determine this client frame's environment")
        process-environment)))
(cl-defmethod my/tb-send ((state my/tb-browser) message)
  "Send JSON MESSAGE to STATE's connected browser."
  (unless (process-live-p (my/tb-browser-connection state))
    (user-error "Terminal browser is not connected"))
  (process-send-string (my/tb-browser-connection state)
                       (concat (json-serialize message) "\n")))

(cl-defmethod my/tb-measure ((state my/tb-browser))
  "Measure STATE's window in terminal cells."
  (let ((window (my/tb-browser-window state)))
    (vector (max 1 (1- (window-body-width window)))
            (min (length my/tb-rows) (max 1 (window-body-height window))))))

(cl-defmethod my/tb-size-message ((state my/tb-browser) type)
  "Return a TYPE message describing STATE's current window."
  (let ((size (my/tb-measure state)) (cell (my/tb-browser-cell state)))
    (setf (my/tb-browser-size state) size)
    (list :type type :cols (aref size 0) :rows (aref size 1)
          :width (* (aref size 0) (aref cell 0))
          :height (* (aref size 1) (aref cell 1)) :cell cell)))

;; AC-0030-0010: Emacs must preserve the first placeholder's combining marks.
(cl-defmethod my/tb-place ((state my/tb-browser) message)
  "Populate STATE's display with validated MESSAGE placeholders."
  (let ((cols (alist-get 'cols message)) (rows (alist-get 'rows message))
        (image (alist-get 'imageId message)))
    (unless (and (integerp cols) (> cols 0) (<= cols (aref (my/tb-browser-size state) 0))
                 (integerp rows) (> rows 0) (<= rows (length my/tb-rows))
                 (<= rows (aref (my/tb-browser-size state) 1))
                 (eql image (my/tb-browser-image state)))
      (user-error "Invalid Terminal browser placement: %S" message))
    (with-current-buffer (my/tb-browser-buffer state)
      (let ((inhibit-read-only t))
        (erase-buffer)
        (dotimes (row rows)
          (let ((start (point)))
            (insert (propertize
                     (concat (string #x10eeee (aref my/tb-rows row) #x0305)
                             (make-string (1- cols) #x10eeee))
                     'face `(:foreground ,(format "#%06x" image))))
            (compose-region start (+ start 3)))
          (unless (= row (1- rows)) (insert "\n")))
        (goto-char (point-min))
        (set-buffer-modified-p nil)))))

;; AC-0030-0020: only complete APC commands reach the real terminal.
(cl-defmethod my/tb-graphics ((state my/tb-browser) bytes)
  "Forward complete graphics commands from BYTES to STATE's frame.
Discard OSC metadata; forward bounded-format OSC 52 clipboard writes."
  (let ((pending (concat (my/tb-browser-graphics state) bytes)))
    (setf (my/tb-browser-graphics state) "")
    (while (not (string-empty-p pending))
      (cond
       ((string-prefix-p "\e_G" pending)
        (if (string-match "\e\\\\" pending)
            (let* ((end (match-end 0))
                   (command (substring pending 0 end)))
              (send-string-to-terminal
               (my/tb-terminal-sequence command (my/tb-browser-tmux state))
               (my/tb-browser-frame state))
              (setq pending (substring pending end)))
          (setf (my/tb-browser-graphics state) pending
                pending "")))
       ((string-prefix-p "\e]" pending)
        (let* ((bell (string-match "\a" pending 2))
               (st (string-match "\e\\\\" pending 2))
               (end (cond ((and bell (or (null st) (< bell st))) (1+ bell))
                          (st (+ st 2)))))
          (if end
              (let* ((sequence (substring pending 0 end))
                     (osc (substring pending 2 (- end (if (and bell (eq end (1+ bell))) 1 2))))
                     (clipboard-write
                      (and (<= (length sequence) 131072)
                           (string-match-p
                            "\\`52;[^;]+;[A-Za-z0-9+/=]*\\'" osc))))
                (when clipboard-write
                  (send-string-to-terminal
                   (my/tb-terminal-sequence sequence (my/tb-browser-tmux state))
                   (my/tb-browser-frame state)))
                (setq pending (substring pending end)))
            (setf (my/tb-browser-graphics state) pending
                  pending ""))))
       ((string-prefix-p "\e[" pending)
        (if (string-match "[@-~]" pending 2)
            (setq pending (substring pending (match-end 0)))
          (setf (my/tb-browser-graphics state) pending
                pending "")))
       ((or (equal pending "\e") (equal pending "\e_"))
        (setf (my/tb-browser-graphics state) pending
              pending ""))
       (t (user-error "Unexpected output on browser graphics relay: %S"
                      (mapconcat (lambda (byte) (format "%02x" byte))
                                 (string-to-list (substring pending 0 (min 32 (length pending))))
                                 "")))))))

(cl-defmethod my/tb-receive ((state my/tb-browser) text)
  "Handle newline-delimited browser protocol TEXT for STATE."
  (setf (my/tb-browser-pending state) (concat (my/tb-browser-pending state) text))
  (when (> (length (my/tb-browser-pending state)) 65536)
    (user-error "Terminal browser protocol message exceeds 64 KiB"))
  (while (string-match "\n" (my/tb-browser-pending state))
    (let* ((end (match-beginning 0))
           (line (substring (my/tb-browser-pending state) 0 end))
           (message (json-parse-string line :object-type 'alist)))
      (setf (my/tb-browser-pending state)
            (substring (my/tb-browser-pending state) (1+ end)))
      (pcase (alist-get 'type message)
        ("join"
         (setf (my/tb-browser-visible state) t)
         (my/tb-send state (append (my/tb-size-message state "init")
                                   (list :imageId (my/tb-browser-image state)
                                         :transport "file" :focused t))))
        ("placed" (my/tb-place state message))
        ("title" nil)
        ((or "pointer" "clipboard") nil)
        (_ (user-error "Unexpected Terminal browser message: %S" message))))))

(cl-defmethod my/tb-tick ((state my/tb-browser))
  "Synchronize STATE's visibility and window size."
  (unless (my/tb-browser-stopping state)
    (if (not (and (window-live-p (my/tb-browser-window state))
                  (frame-live-p (my/tb-browser-frame state))))
        (my/tb-stop state)
      (when (process-live-p (my/tb-browser-connection state))
        (let ((visible (eq (window-buffer (my/tb-browser-window state))
                           (my/tb-browser-buffer state))))
          (unless (eq visible (my/tb-browser-visible state))
            (setf (my/tb-browser-visible state) visible)
            (my/tb-send state (list :type "visible" :value (if visible t :false))))
          (when (and visible (not (equal (my/tb-browser-size state) (my/tb-measure state))))
            (my/tb-send state (my/tb-size-message state "size"))))))))

(cl-defmethod my/tb-failed ((state my/tb-browser) context event)
  "Stop STATE and report unexpected CONTEXT with EVENT to the user."
  (let ((detail (when (buffer-live-p (my/tb-browser-stderr state))
                  (with-current-buffer (my/tb-browser-stderr state)
                    (string-trim (buffer-string))))))
    (my/tb-stop state)
    (message "Terminal browser %s: %s%s" context (string-trim event)
             (if (and detail (not (string-empty-p detail))) (concat ": " detail) ""))))

(cl-defmethod my/tb-filter ((state my/tb-browser) function text)
  "Run STATE's protocol FUNCTION on TEXT, cleaning up on failure."
  (unless (my/tb-browser-stopping state)
    (condition-case err (funcall function state text)
      (error
       (my/tb-stop state)
       (user-error "Terminal browser protocol failed: %s" (error-message-string err))))))

(cl-defmethod my/tb-watch ((state my/tb-browser) process context)
  "Watch PROCESS owned by STATE and report unexpected CONTEXT."
  (set-process-sentinel
   process (lambda (proc event)
             (when (and (not (process-live-p proc)) (not (my/tb-browser-stopping state)))
               (my/tb-failed state context event)))))

(cl-defmethod my/tb-stop ((state my/tb-browser))
  "Release STATE's resources and restore its origin only if still displayed."
  (unless (my/tb-browser-stopping state)
    (setf (my/tb-browser-stopping state) t)
    (setq my/tb-browsers (delq state my/tb-browsers))
    (when (timerp (my/tb-browser-timer state)) (cancel-timer (my/tb-browser-timer state)))
    ;; Closing the CLI pipe also closes its daemon session.  SIGTERM first
    ;; lets the CLI explicitly send its session-close command, not daemon-wide quit.
    (unwind-protect
        (when (process-live-p (my/tb-browser-child state))
          (signal-process (my/tb-browser-child state) 'SIGTERM)
          (let ((deadline (+ (float-time) 3)))
            (while (and (process-live-p (my/tb-browser-child state)) (< (float-time) deadline))
              (accept-process-output (my/tb-browser-child state) 0.05)))
          (when (process-live-p (my/tb-browser-child state))
            (delete-process (my/tb-browser-child state))
            (user-error "Terminal browser did not acknowledge session close")))
      (unwind-protect
          (progn
            (dolist (proc (list (my/tb-browser-connection state) (my/tb-browser-server state)
                                (my/tb-browser-relay state)))
              (when (processp proc)
                (when (timerp (process-get proc 'deadline))
                  (cancel-timer (process-get proc 'deadline)))
                (delete-process proc)))
            (when (frame-live-p (my/tb-browser-frame state))
              (when (my/tb-browser-image state)
                (send-string-to-terminal
                 (my/tb-terminal-sequence
                  (format "\e_Ga=d,d=I,i=%d,q=2\e\\" (my/tb-browser-image state))
                  (my/tb-browser-tmux state)) (my/tb-browser-frame state)))
              (when (my/tb-browser-mouse-owned state)
                (with-selected-frame (my/tb-browser-frame state)
                  (turn-off-xterm-mouse-tracking-on-terminal
                   (frame-terminal (my/tb-browser-frame state)))))))
        (unwind-protect
            (let ((window (my/tb-browser-window state))
                  (source (my/tb-browser-origin state)))
              (when (and (window-live-p window) (buffer-live-p source)
                         (eq (window-buffer window) (my/tb-browser-buffer state)))
                (set-window-buffer window source)
                (set-window-point window (my/tb-browser-point state))
                ;; A direct q command runs with the preview as current buffer.  Move
                ;; current-buffer before killing it so Emacs does not select *scratch*.
                (when (eq (current-buffer) (my/tb-browser-buffer state))
                  (set-buffer source))))
          (unwind-protect
              (when (and (my/tb-browser-directory state) (file-directory-p (my/tb-browser-directory state)))
                (delete-directory (my/tb-browser-directory state) t))
            (dolist (buffer (list (my/tb-browser-log state) (my/tb-browser-stderr state)
                                  (my/tb-browser-buffer state)))
              (when (buffer-live-p buffer) (kill-buffer buffer)))
            (when (my/tb-browser-on-close state)
              (funcall (my/tb-browser-on-close state)))))))))

(defun my/tb-stop-terminal-preview (frame)
  "Stop previews owned by the terminal containing FRAME."
  (let ((terminal (frame-terminal frame)))
    (dolist (state (copy-sequence my/tb-browsers))
      (let ((owner (my/tb-browser-frame state)))
        (when (and (frame-live-p owner) (eq terminal (frame-terminal owner)))
          (my/tb-stop state))))))

(defun my/tb-stop-frame-previews (frame)
  "Stop previews owned by FRAME before it is deleted."
  (dolist (state (copy-sequence my/tb-browsers))
    (when (eq frame (my/tb-browser-frame state))
      (my/tb-stop state))))
(defun my/tb-stop-all ()
  "Release all preview resources before Emacs exits."
  (dolist (state (copy-sequence my/tb-browsers)) (my/tb-stop state)))
(cl-defmethod my/tb-start ((state my/tb-browser) url &optional mode)
  "Start STATE's relays and browser at URL, using display MODE."
  (with-current-buffer (my/tb-browser-buffer state)
    (funcall (or mode #'terminal-browser-embed-mode))
    (setq my/tb-browser-state state))
  (set-window-buffer (my/tb-browser-window state) (my/tb-browser-buffer state))
  (let ((terminal (frame-terminal (my/tb-browser-frame state))))
    (unless (terminal-parameter terminal 'xterm-mouse-mode)
      (setf (my/tb-browser-mouse-owned state) t)
      (let ((xterm-mouse-mode t)) (turn-on-xterm-mouse-tracking-on-terminal terminal))))
  (setf (my/tb-browser-relay state)
        (make-process :name "terminal-browser-graphics" :command '("cat")
                      :connection-type 'pty :coding 'binary :noquery t
                      :filter (lambda (_proc bytes) (my/tb-filter state #'my/tb-graphics bytes))))
  (my/tb-watch state (my/tb-browser-relay state) "graphics relay stopped")
  (setf (my/tb-browser-server state)
        (make-network-process
         :name "terminal-browser-embed" :family 'local :server t :service (my/tb-browser-socket state)
         :coding 'utf-8-unix :noquery t
         :log (lambda (_server connection _message)
                (when (process-live-p (my/tb-browser-connection state))
                  (delete-process connection)
                  (user-error "Terminal browser already connected"))
                (setf (my/tb-browser-connection state) connection)
                (set-process-query-on-exit-flag connection nil)
                (set-process-filter connection (lambda (_proc text) (my/tb-filter state #'my/tb-receive text)))
                (my/tb-watch state connection "disconnected"))))
  (let ((process-environment (copy-sequence (my/tb-browser-environment state))))
    (setenv "PIXEL_EMBED" (my/tb-browser-socket state))
    (setenv "PIXEL_TTY" (process-tty-name (my/tb-browser-relay state)))
    (setenv "PIXEL_PANE" nil)
    (setenv "TERMINAL_BROWSER_NO_CHROME" "1")
    (setf (my/tb-browser-child state)
          (make-process :name "terminal-browser-client" :connection-type 'pipe :noquery t
                        :buffer (my/tb-browser-log state) :stderr (my/tb-browser-stderr state)
                        :command (list (executable-find "terminal-browser") "open" url))))
  (my/tb-watch state (my/tb-browser-child state) "browser stopped")
  (setf (my/tb-browser-timer state) (run-at-time 0.2 0.2 #'my/tb-tick state)))

(defun my/tb-terminal-cell (tmux image)
  "Probe current terminal cell pixels and Kitty support with TMUX and IMAGE."
  (let (cell graphics)
    (xterm--query "\e[16t"
                  (list (cons "\e[6;" (lambda ()
                                        (let ((reply (xterm--read-string ?t)))
                                          (when (string-match "\\`\\([0-9]+\\);\\([0-9]+\\)\\'" reply)
                                            (setq cell (vector (string-to-number (match-string 2 reply))
                                                               (string-to-number (match-string 1 reply))))))))) t)
    (unless (and cell (> (aref cell 0) 0) (> (aref cell 1) 0))
      (user-error "Terminal browser: terminal did not report cell pixel size"))
    (xterm--query
     (my/tb-terminal-sequence (format "\e_Gi=%d,s=1,v=1,a=q,t=d,f=24;AAAA\e\\" image) tmux)
     (list (cons (format "\e_Gi=%d;" image)
                 (lambda () (setq graphics (equal (xterm--read-string ?\e ?\\) "OK"))))) t)
    (unless graphics
      (user-error "Terminal browser: Kitty graphics probe failed; %s"
                  (if tmux "check tmux allow-passthrough and the outer terminal"
                    "the terminal does not support Kitty graphics")))
    cell))

;;;###autoload
(defun my/terminal-browser-open-url (url &optional mode on-close)
  "Show URL in this TUI Emacs window using MODE and ON-CLOSE callback.
Return the owned browser state.  MODE defaults to `terminal-browser-embed-mode'."
  (interactive "sURL: ")
  (unless (and (stringp url) (not (string-empty-p url)))
    (user-error "Terminal browser URL must be nonempty"))
  (when (display-graphic-p)
    (user-error "Embedded terminal browser requires a TUI frame"))
  (dolist (program '("terminal-browser" "cat"))
    (unless (executable-find program)
      (user-error "Embedded terminal browser requires %s" program)))
  (unless (>= (display-color-cells) #x1000000)
    (user-error "Embedded terminal browser requires true-color output"))
  (let* ((frame (selected-frame))
         (environment (my/tb-frame-environment frame))
         (process-environment environment)
         (tmux (getenv "TMUX"))
         (image (1+ (random #xfffffe))))
    (while (cl-find image my/tb-browsers :key #'my/tb-browser-image)
      (setq image (1+ (random #xfffffe))))
    (let ((cell (my/tb-terminal-cell tmux image)))
      (my/tb-stop-terminal-preview frame)
      (let* ((directory (make-temp-file "emacs-terminal-browser-" t))
             (state (make-my/tb-browser
                     :origin (current-buffer) :point (point)
                     :window (selected-window) :frame frame
                     :environment environment :tmux tmux :cell cell :image image
                     :directory directory :socket (expand-file-name "embed.sock" directory)
                     :buffer (generate-new-buffer "*Terminal browser*")
                     :log (generate-new-buffer " *Terminal browser output*")
                     :stderr (generate-new-buffer " *Terminal browser errors*")
                     :on-close on-close)))
        (push state my/tb-browsers)
        (condition-case err
            (my/tb-start state url mode)
          (error
           (my/tb-stop state)
           (user-error "Cannot open terminal browser: %s" (error-message-string err))))
        state))))

(defun my/tb-key ()
  "Forward a navigation key from this preview to the browser."
  (interactive)
  (let ((key (pcase last-command-event
               (2 "pageup") (6 "pagedown")
               ('next "pagedown") ('prior "pageup")
               ((or 'down ?j) "down") ((or 'up ?k) "up")
               ('left "left") ('right "right") ('home "home") ('end "end")
               (?g "home") (?G "end")
               (13 "enter") (9 "tab")
               (_ (user-error "Unsupported Terminal browser key: %S" last-command-event)))))
    (my/tb-send my/tb-browser-state
                (list :type "key" :key key :kind "press"
                      :mods '(:shift :false :alt :false :ctrl :false :super :false)))))

(defun my/tb-half-page (direction)
  "Scroll half a preview page in DIRECTION, either up or down."
  (let ((key (if (eq direction 'down) "down" "up"))
        (count (max 1 (/ (window-body-height (my/tb-browser-window my/tb-browser-state)) 2))))
    (dotimes (_ count)
      (my/tb-send my/tb-browser-state
                  (list :type "key" :key key :kind "press"
                        :mods '(:shift :false :alt :false :ctrl :false :super :false))))))

(defun my/tb-half-page-down ()
  "Scroll down half a preview page, like Evil `C-d'."
  (interactive)
  (my/tb-half-page 'down))

(defun my/tb-half-page-up ()
  "Scroll up half a preview page, like Evil `C-u'."
  (interactive)
  (my/tb-half-page 'up))

(defun my/tb-browser-history (key)
  "Send browser history KEY to the current preview."
  (my/tb-send my/tb-browser-state
              (list :type "key" :key key :kind "press"
                    :mods (list :shift :false :alt :false
                                :ctrl (if (eq system-type 'darwin) :false t)
                                :super (if (eq system-type 'darwin) t :false)))))

(defun my/tb-browser-back ()
  "Move back in the current browser history."
  (interactive)
  (my/tb-browser-history "["))

(defun my/tb-browser-forward ()
  "Move forward in the current browser history."
  (interactive)
  (my/tb-browser-history "]"))

(defun my/tb-mouse (event)
  "Forward a click or wheel EVENT within the browser window."
  (interactive "e")
  (let* ((position (event-end event)) (window (posn-window position))
         (kind (event-basic-type event)))
    (when (and (windowp window) (null (posn-area position)))
      (with-current-buffer (window-buffer window)
        (unless (and my/tb-browser-state (eq window (my/tb-browser-window my/tb-browser-state)))
          (user-error "Mouse event is outside the Terminal browser window"))
        (let* ((state my/tb-browser-state) (cell (my/tb-browser-cell state))
               (coords (posn-col-row position)) (mods (event-modifiers event))
               (scroll (memq kind '(wheel-up wheel-down mouse-4 mouse-5))))
          (select-window window)
          (my/tb-send
           state (list :type "mouse" :kind (cond ((memq kind '(wheel-up mouse-4)) "scrollup")
                                                 ((memq kind '(wheel-down mouse-5)) "scrolldown")
                                                 ((memq 'down mods) "down") (t "up"))
                       :button (if scroll "none" "left")
                       :x (* (+ (car coords) 0.5) (aref cell 0))
                       :y (* (+ (cdr coords) 0.5) (aref cell 1))
                       :mods (list :shift (if (memq 'shift mods) t :false)
                                   :alt (if (memq 'meta mods) t :false)
                                   :ctrl (if (memq 'control mods) t :false) :super :false))))))))
(define-derived-mode terminal-browser-embed-mode special-mode "Terminal-Browser"
  "Read-only embedded browser display."
  :interactive nil
  (setq-local truncate-lines t cursor-type nil show-trailing-whitespace nil
              buffer-undo-list t display-line-numbers nil
              mode-line-format '(" Browser"))
  (auto-composition-mode -1)
  (add-hook 'kill-buffer-hook (lambda () (when my/tb-browser-state (my/tb-stop my/tb-browser-state))) nil t))
(defun my/tb-close ()
  "Close the browser in the current buffer."
  (interactive)
  (my/tb-stop my/tb-browser-state))

(defun my/tb-refresh ()
  "Reload the current browser page."
  (interactive)
  (my/tb-send my/tb-browser-state
              (list :type "key" :key "r" :kind "press"
                    :mods (list :shift :false :alt :false
                                :ctrl (if (eq system-type 'darwin) :false t)
                                :super (if (eq system-type 'darwin) t :false)))))

(define-key terminal-browser-embed-mode-map (kbd "q") #'my/tb-close)
(dolist (key '("<next>" "<prior>" "<down>" "<up>" "<left>" "<right>" "<home>" "<end>" "RET" "TAB"))
  (define-key terminal-browser-embed-mode-map (kbd key) #'my/tb-key))
(dolist (event '(down-mouse-1 mouse-1 drag-mouse-1 wheel-up wheel-down mouse-4 mouse-5))
  (define-key terminal-browser-embed-mode-map (vector event) #'my/tb-mouse)
  (define-key terminal-browser-embed-mode-map
    (vector (event-convert-list (list 'control event))) #'my/tb-mouse))

(add-hook 'kill-emacs-hook #'my/tb-stop-all)
(add-hook 'delete-frame-functions #'my/tb-stop-frame-previews)

(provide 'terminal-browser-embed)
;;; terminal-browser-embed.el ends here
