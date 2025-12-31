;;; im-bridge-core.el --- Core definitions for im-bridge -*- lexical-binding:t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; Core definitions, options, and CLI helpers for im-bridge.
;;
;;; Code:

(defgroup im-bridge nil
  "Bridge system and Emacs input methods."
  :group 'convenience
  :prefix "imb-")

(defcustom imb-english-id "com.apple.keylayout.ABC"
  "Identifier for the English system IME."
  :type 'string
  :safe #'stringp)

(defcustom imb-fast-switch-ids '("com.apple.keylayout.ABC")
  "List of IME IDs that skip macOS bug workaround when switching.
Only pure keyboard layouts (like ABC) should be in this list.
CJK input methods need the workaround to function correctly."
  :type '(repeat string))

(defconst imb--cli-command "macism"
  "CLI tool used to query / set system IME.")

(defun imb--cli-available-p ()
  "Return non-nil if macism is executable."
  (executable-find imb--cli-command))

(defun imb--warn-cli-unavailable ()
  "Warn that macism is not available."
  (message "im-bridge: macism not found"))

(defun imb--im-get ()
  "Return current system IME id."
  (string-trim (shell-command-to-string imb--cli-command)))

(defun imb--im-set (id)
  "Switch system IME to ID.
If ID is in `imb-fast-switch-ids', skip macOS bug workaround."
  (when id
    (if (member id imb-fast-switch-ids)
        (call-process imb--cli-command nil nil nil id "0")
      (call-process imb--cli-command nil nil nil id))))

(defun imb--ensure-english ()
  "Ensure system IME is English."
  (unless (string= (imb--im-get) imb-english-id)
    (imb--im-set imb-english-id)))

(provide 'im-bridge-core)
;;; im-bridge-core.el ends here
