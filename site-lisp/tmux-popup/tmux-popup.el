;;; tmux-popup.el --- Close button for Emacs frames in a tmux popup -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; tmux-open shows a file in a tmux popup by running emacsclient there
;; with TMUX_POPUP=1 in its environment.  `tmux-popup-mode' puts a "×"
;; at the right end of the tab line of the top right window of such a
;; frame; clicking it does what C-x C-c does in that frame.  The tab
;; line is a line of its own above the header line, so the button never
;; competes with the buffer's header line for room.  The button is a
;; window parameter, so it stays when the window shows another buffer,
;; and it moves when the windows are split or deleted.
;; SPEC.md holds the behavior contract.

;;; Code:

(defgroup tmux-popup nil
  "Close button for Emacs frames shown in a tmux popup."
  :group 'frames)

(defconst tmux-popup--button
  (propertize "× " 'local-map
              (let ((map (make-sparse-keymap)))
                (keymap-set map "<tab-line> <mouse-1>" #'tmux-popup-close)
                map))
  "Close button shown at the right end of the tab line.
A terminal cell is about twice as tall as it is wide, so the trailing
space keeps the \"×\" about as far from the right border as the tab
line keeps it from the top one.")

(defconst tmux-popup--tab-line-format
  (let ((width (string-width tmux-popup--button)))
    (list (propertize " " 'display `(space :align-to (- right ,width)))
          tmux-popup--button))
  "Tab line format of the window that shows the close button.")

(defun tmux-popup--frame-p (frame)
  "Return non-nil if FRAME was opened by an emacsclient with TMUX_POPUP=1.
Only the environment the client passed counts: a frame without one,
like the initial frame, never qualifies, whatever the environment of
Emacs itself says."
  (let ((env (frame-parameter frame 'environment)))
    (and env (equal (getenv-internal "TMUX_POPUP" env) "1"))))

(defvar tmux-popup--windows (make-hash-table :test #'eq :weakness 'key)
  "Windows that were given the close button, deleted ones included.
A deleted window keeps its parameters, and restoring a window
configuration, as `winner-undo' does, brings it back with them.
Windows nothing else refers to any more drop out of the table.")

(defun tmux-popup--remove (window)
  "Remove the close button from WINDOW if it shows one."
  (when (equal (window-parameter window 'tab-line-format)
               tmux-popup--tab-line-format)
    (set-window-parameter window 'tab-line-format nil)))

(defun tmux-popup--update (frame)
  "Show the close button on the top right window of FRAME and no other.
Do nothing unless FRAME is a popup frame."
  (when (tmux-popup--frame-p frame)
    (dolist (window (window-list frame 'nomini))
      (if (and (window-at-side-p window 'top) (window-at-side-p window 'right))
          (progn
            (puthash window t tmux-popup--windows)
            (set-window-parameter window 'tab-line-format
                                  tmux-popup--tab-line-format))
        (tmux-popup--remove window)))))

(defun tmux-popup-close (event)
  "Close the frame clicked in EVENT as \\[save-buffers-kill-terminal] there would.
Signal a `user-error' if no emacsclient opened that frame: there
`save-buffers-kill-terminal' would kill Emacs itself."
  (interactive "e")
  (let ((frame (window-frame (posn-window (event-start event)))))
    (unless (processp (frame-parameter frame 'client))
      (user-error "Not a frame of emacsclient: %s" frame))
    (with-selected-frame frame
      (save-buffers-kill-terminal))))

;;;###autoload
(define-minor-mode tmux-popup-mode
  "Show a close button in Emacs frames opened in a tmux popup.
A frame opened by an emacsclient with TMUX_POPUP=1 in its environment
gets a \"×\" at the right end of the tab line of its top right window.
Clicking it runs `tmux-popup-close'."
  :global t
  :group 'tmux-popup
  (if tmux-popup-mode
      (progn
        ;; The default value runs for each frame whose windows were added,
        ;; deleted, resized or changed buffer, new frames included.
        (add-hook 'window-state-change-functions #'tmux-popup--update)
        (mapc #'tmux-popup--update (frame-list)))
    (remove-hook 'window-state-change-functions #'tmux-popup--update)
    (maphash (lambda (window _) (tmux-popup--remove window))
             tmux-popup--windows))
  ;; Setting a window parameter does not redraw the window, and the
  ;; mode's own update covers the current buffer only.
  (force-mode-line-update t))

(provide 'tmux-popup)
;;; tmux-popup.el ends here
