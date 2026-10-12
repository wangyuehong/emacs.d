;;; tmux-popup-test.el --- Tests for tmux-popup -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;;
;;; Commentary:
;; Unit tests for tmux-popup, run under `emacs -Q --batch' by `make test'.
;; Batch Emacs has no redisplay, so:
;; - a frame becomes a popup frame by getting the environment parameter
;;   emacsclient would give it, and a client by getting a process as its
;;   client parameter;
;; - frames other than the selected one are made on a pseudo terminal,
;;   as batch Emacs cannot make one on its own terminal;
;; - window change functions are run by hand where redisplay would run
;;   them;
;; - `format-mode-line' returns "" in batch, so the tests inspect the
;;   tab line construct a window shows instead of its rendering.
;;
;;; Code:

(require 'cl-lib)
(require 'ert)
(require 'ert-x)
(require 'seq)
(require 'tmux-popup)

(defconst tmux-popup-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory of the tmux-popup package.")

(defconst tmux-popup-test--popup-env '("TMUX_POPUP=1" "TERM=xterm-256color")
  "Environment an emacsclient running in a tmux popup passes.")

(defmacro tmux-popup-test--with-frame (env &rest body)
  "Run BODY in the selected frame with one window and environment ENV.
Afterwards `tmux-popup-mode' is off, the frame has one window and its
environment parameter is restored."
  (declare (indent 1) (debug t))
  `(let ((old-env (frame-parameter nil 'environment)))
     (unwind-protect
         (progn
           (delete-other-windows)
           (set-frame-parameter nil 'environment ,env)
           ,@body)
       (tmux-popup-mode -1)
       (delete-other-windows)
       (set-frame-parameter nil 'environment old-env))))

(defmacro tmux-popup-test--with-tty-frame (frame env &rest body)
  "Run BODY with FRAME bound to a new frame with environment ENV.
The frame is on a pseudo terminal of 80 columns and 24 lines and is not
selected.  The frame and the process holding the terminal are deleted
afterwards, also when making the frame fails."
  (declare (indent 2) (debug t))
  `(let ((pty nil)
         (,frame nil))
     (unwind-protect
         (progn
           (setq pty (make-process :name "tmux-popup-test-tty"
                                   :command '("sleep" "60")
                                   :connection-type 'pty :noquery t))
           (set-process-window-size pty 24 80)
           (setq ,frame (make-terminal-frame
                         (list (cons 'tty (process-tty-name pty))
                               (cons 'tty-type "xterm-256color")
                               (cons 'environment ,env))))
           (should-not (eq ,frame (selected-frame)))
           ,@body)
       (when (frame-live-p ,frame)
         (delete-frame ,frame t))
       (when pty
         (delete-process pty)))))

(defun tmux-popup-test--redisplay (&optional frame)
  "Run what redisplay runs after the windows of FRAME changed.
FRAME defaults to the selected frame."
  (run-hook-with-args 'window-state-change-functions
                      (or frame (selected-frame))))

(defun tmux-popup-test--button-windows (&optional frame)
  "Return the windows of FRAME that show a tab line of their own.
FRAME defaults to the selected frame."
  (seq-filter (lambda (window) (window-parameter window 'tab-line-format))
              (window-list frame 'nomini)))

(defun tmux-popup-test--should-show-button (window)
  "Assert that WINDOW, and no other window of its frame, shows the close button.
The button is alone on the tab line: a space stretching to two columns
from the right edge, then the \"×\" and one blank.  The header line is
left to the buffer."
  (should (equal (tmux-popup-test--button-windows (window-frame window))
                 (list window)))
  (should-not (window-parameter window 'header-line-format))
  (pcase (window-parameter window 'tab-line-format)
    (`(,space ,button)
     (should (equal button "× "))
     (should (eq (lookup-key (get-text-property 0 'local-map button)
                             [tab-line mouse-1])
                 #'tmux-popup-close))
     (should (equal (get-text-property 0 'display space)
                    '(space :align-to (- right 2)))))
    (format (ert-fail (list "No close button on the tab line" format)))))

(defun tmux-popup-test--click (window)
  "Click with mouse-1 on the close button in the tab line of WINDOW."
  (let* ((button (car (last (window-parameter window 'tab-line-format))))
         (command (lookup-key (get-text-property 0 'local-map button)
                              [tab-line mouse-1])))
    (call-interactively command nil
                        (vector `(mouse-1 (,window tab-line (0 . 0) 0))))))

(defmacro tmux-popup-test--with-client (frame &rest body)
  "Run BODY with FRAME owned by a client process.
FRAME's client parameter is restored afterwards.  Calls to
`save-buffers-kill-terminal' are recorded in `calls' as (ARG SELECTED),
SELECTED being the frame selected during the call."
  (declare (indent 1) (debug t))
  `(let* ((target ,frame)
          (old-client (frame-parameter target 'client))
          (client (make-pipe-process :name "tmux-popup-test-client" :noquery t))
          (calls nil))
     (unwind-protect
         (cl-letf (((symbol-function 'save-buffers-kill-terminal)
                    (lambda (&optional arg)
                      (push (list arg (selected-frame)) calls))))
           (set-frame-parameter target 'client client)
           ,@body)
       (set-frame-parameter target 'client old-client)
       (delete-process client))))

;;; US-0010

(ert-deftest tmux-popup-test-popup-frame-shows-button ()
  "AC-0010-0010: a popup frame shows the button on its top right window.
The window gets a tab line holding only the button; the buffer has no
header line, so none appears."
  (tmux-popup-test--with-frame nil
    (tmux-popup-mode 1)
    (should-not (buffer-local-value 'header-line-format (window-buffer)))
    (should (= (window-tab-line-height) 0))
    (set-frame-parameter nil 'environment tmux-popup-test--popup-env)
    (tmux-popup-test--redisplay)
    (tmux-popup-test--should-show-button (selected-window))
    (should (= (window-tab-line-height) 1))
    (should (= (window-header-line-height) 0))))

(ert-deftest tmux-popup-test-main-frame-unchanged ()
  "AC-0010-0020: the frame of an Emacs started with TMUX_POPUP=1 gets no button.
It runs in a separate Emacs: only an initial frame has no environment
parameter at all, and the parameter cannot be removed once set.  That
Emacs prefers the newer of source and byte code, as `make test' does,
and its stderr is kept out of the output checked."
  (let ((process-environment (cons "TMUX_POPUP=1" process-environment)))
    (ert-with-temp-file stderr
      (with-temp-buffer
        (let ((status (call-process
                       (expand-file-name invocation-name invocation-directory)
                       nil (list t stderr) nil
                       "-Q" "--batch" "--eval" "(setq load-prefer-newer t)"
                       "-L" tmux-popup-test--dir "-l" "tmux-popup-test" "--eval"
                       (prin1-to-string
                        '(progn
                           (tmux-popup-mode 1)
                           (tmux-popup-test--redisplay)
                           (prin1 (list (getenv "TMUX_POPUP")
                                        (tmux-popup-test--button-windows))))))))
          (ert-info ((with-temp-buffer
                       (insert-file-contents stderr)
                       (buffer-string))
                     :prefix "stderr: ")
            (should (eql status 0))
            (should (equal (buffer-string) "(\"1\" nil)"))))))))

(ert-deftest tmux-popup-test-other-clients-unchanged ()
  "AC-0010-0020: frames of clients without TMUX_POPUP=1 stay as they are.
Each of these rows of the Examples table is checked on its own."
  (dolist (env '(("TERM=xterm-256color") ("TMUX_POPUP=0")))
    (ert-info ((format "client environment %S" env))
      (tmux-popup-test--with-frame env
        (with-temp-buffer
          (let ((own '("SENTINEL")))
            (setq-local header-line-format own)
            (set-window-buffer nil (current-buffer))
            (tmux-popup-mode 1)
            (tmux-popup-test--redisplay)
            (should-not (tmux-popup-test--button-windows))
            (should (= (window-tab-line-height) 0))
            (should (eq header-line-format own))))))))

(ert-deftest tmux-popup-test-button-stays-on-file ()
  "AC-0010-0030: the button stays when the window visits another file."
  (ert-with-temp-file file
    (tmux-popup-test--with-frame tmux-popup-test--popup-env
      (tmux-popup-mode 1)
      (find-file file)
      (unwind-protect
          (progn
            (tmux-popup-test--redisplay)
            (should (equal (buffer-file-name (window-buffer)) file))
            (tmux-popup-test--should-show-button (selected-window)))
        (kill-buffer (get-file-buffer file))))))

(ert-deftest tmux-popup-test-button-stays-on-dired ()
  "AC-0010-0030: the button stays when the window enters dired."
  (tmux-popup-test--with-frame tmux-popup-test--popup-env
    (tmux-popup-mode 1)
    (let ((dired (dired temporary-file-directory)))
      (unwind-protect
          (progn
            (tmux-popup-test--redisplay)
            (should (eq (window-buffer) dired))
            (tmux-popup-test--should-show-button (selected-window)))
        (kill-buffer dired)))))

(ert-deftest tmux-popup-test-split-below ()
  "AC-0010-0040: after a split below, only the upper window has the button."
  (tmux-popup-test--with-frame tmux-popup-test--popup-env
    (tmux-popup-mode 1)
    (let* ((upper (selected-window))
           (lower (split-window upper nil 'below)))
      (tmux-popup-test--redisplay)
      (should (window-live-p lower))
      (tmux-popup-test--should-show-button upper))))

(ert-deftest tmux-popup-test-split-right ()
  "AC-0010-0040: after a split right, only the right window has the button."
  (tmux-popup-test--with-frame tmux-popup-test--popup-env
    (tmux-popup-mode 1)
    (let ((right (split-window (selected-window) nil 'right)))
      (tmux-popup-test--redisplay)
      (tmux-popup-test--should-show-button right))))

(ert-deftest tmux-popup-test-delete-top-right ()
  "AC-0010-0040: deleting the top right window moves the button.
The right column is split below; once its upper window is gone, the
lower one is the top right window."
  (tmux-popup-test--with-frame tmux-popup-test--popup-env
    (tmux-popup-mode 1)
    (let* ((right (split-window (selected-window) nil 'right))
           (lower (split-window right nil 'below)))
      (tmux-popup-test--redisplay)
      (tmux-popup-test--should-show-button right)
      (delete-window right)
      (tmux-popup-test--redisplay)
      (tmux-popup-test--should-show-button lower))))

(ert-deftest tmux-popup-test-split-in-unselected-frame ()
  "AC-0010-0040: the button follows a split in a popup frame not selected.
Redisplay runs the window change functions for each changed frame,
whichever frame is selected."
  (tmux-popup-test--with-frame nil
    (tmux-popup-test--with-tty-frame popup tmux-popup-test--popup-env
      (tmux-popup-mode 1)
      (let ((right (split-window (frame-first-window popup) nil 'right)))
        (tmux-popup-test--redisplay popup)
        (tmux-popup-test--should-show-button right)
        (should-not (tmux-popup-test--button-windows))))))

(ert-deftest tmux-popup-test-buffer-header-line-kept ()
  "AC-0010-0050: the buffer's own header line shows below the button.
Neither the header line nor its width depends on the button, so each
row of the Examples table is left as it is, whether it is wider than
the window or aligned up to its right edge."
  (dolist (own (list '(:eval (concat "src › a.el : " (make-string 200 ?c)))
                     (make-string 200 ?中)
                     (concat "CRM Buffer"
                             (propertize " " 'display
                                         '(space :align-to (- right 5)))
                             "Size")))
    (ert-info ((format "header line %S" own))
      (tmux-popup-test--with-frame tmux-popup-test--popup-env
        (with-temp-buffer
          (let ((right (split-window (selected-window) nil 'right)))
            (setq-local header-line-format own)
            (set-window-buffer right (current-buffer))
            (tmux-popup-mode 1)
            (tmux-popup-test--redisplay)
            (should (< (window-body-width right) (frame-width)))
            (tmux-popup-test--should-show-button right)
            (should (eq header-line-format own))
            (should (= (window-tab-line-height right) 1))
            (should (= (window-header-line-height right) 1))))))))

;;; US-0020

(ert-deftest tmux-popup-test-click-closes-frame ()
  "AC-0020-0010 AC-0020-0020: clicking the button runs C-x C-c in its frame.
The clicked frame is not the selected one.  What this observes is one
call of `save-buffers-kill-terminal' without a prefix argument while
that frame is selected, so that it asks before saving as C-x C-c does.
The question itself is left to a real terminal."
  (tmux-popup-test--with-frame nil
    (tmux-popup-test--with-tty-frame popup tmux-popup-test--popup-env
      (tmux-popup-test--with-client popup
        (tmux-popup-mode 1)
        (tmux-popup-test--click (frame-first-window popup))
        (should (equal calls (list (list nil popup))))))))

(ert-deftest tmux-popup-test-close-refuses-non-client-frame ()
  "AC-0020-0030: closing a frame no emacsclient opened is refused."
  (tmux-popup-test--with-frame tmux-popup-test--popup-env
    (tmux-popup-test--with-client (selected-frame)
      (set-frame-parameter nil 'client nil)
      (tmux-popup-mode 1)
      (should-error (tmux-popup-test--click (selected-window))
                    :type 'user-error)
      (should-not calls))))

;;; US-0030

(ert-deftest tmux-popup-test-enable-shows-button-at-once ()
  "AC-0030-0010: enabling the mode shows the button on all open popup frames.
One of them is not selected, and no window change follows."
  (tmux-popup-test--with-frame tmux-popup-test--popup-env
    (tmux-popup-test--with-tty-frame popup tmux-popup-test--popup-env
      (should-not (tmux-popup-test--button-windows))
      (should-not (tmux-popup-test--button-windows popup))
      (tmux-popup-mode 1)
      (tmux-popup-test--should-show-button (selected-window))
      (tmux-popup-test--should-show-button (frame-first-window popup)))))

(ert-deftest tmux-popup-test-disable-removes-button ()
  "AC-0030-0020: disabling the mode removes the button for good.
The popup frame is not selected.  The tab line some other code gives
its left window, once the button has moved away, is left alone."
  (tmux-popup-test--with-frame nil
    (tmux-popup-test--with-tty-frame popup tmux-popup-test--popup-env
      (tmux-popup-mode 1)
      (let* ((left (frame-first-window popup))
             (right (split-window left nil 'right))
             (other '("OTHER")))
        (set-window-parameter left 'tab-line-format other)
        (tmux-popup-test--redisplay popup)
        (should (equal (window-parameter right 'tab-line-format)
                       tmux-popup--tab-line-format))
        (tmux-popup-mode -1)
        (should (equal (tmux-popup-test--button-windows popup) (list left)))
        (should (eq (window-parameter left 'tab-line-format) other))
        (split-window right nil 'below)
        (tmux-popup-test--redisplay popup)
        (should (equal (tmux-popup-test--button-windows popup) (list left)))
        (with-temp-buffer
          (set-window-buffer right (current-buffer))
          (tmux-popup-test--redisplay popup)
          (should (equal (tmux-popup-test--button-windows popup)
                         (list left))))))))

(ert-deftest tmux-popup-test-disable-removes-button-from-deleted-windows ()
  "AC-0030-0020: a window brought back after disabling shows no button.
Restoring a window configuration, as `winner-undo' does, revives a
deleted window with the parameters it had when it was deleted."
  (tmux-popup-test--with-frame tmux-popup-test--popup-env
    (tmux-popup-mode 1)
    (let ((right (split-window (selected-window) nil 'right)))
      (tmux-popup-test--redisplay)
      (tmux-popup-test--should-show-button right)
      (let ((config (current-window-configuration)))
        (delete-window right)
        (tmux-popup-test--redisplay)
        (tmux-popup-mode -1)
        (set-window-configuration config))
      (should (window-live-p right))
      (should-not (tmux-popup-test--button-windows)))))

(provide 'tmux-popup-test)
;;; tmux-popup-test.el ends here
