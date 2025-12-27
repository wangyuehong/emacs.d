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

(defcustom imb-cli-command "macism"
  "CLI tool used to query / set system IME."
  :type 'string
  :safe #'stringp)

(defcustom imb-english-id "com.apple.keylayout.ABC"
  "Identifier for the English system IME."
  :type 'string
  :safe #'stringp)

(defun imb--cli-available-p ()
  "Return non-nil if `imb-cli-command' is executable."
  (executable-find imb-cli-command))

(defun imb--warn-cli-unavailable ()
  "Warn that CLI command is not available."
  (message "im-bridge: CLI command not found: %s" imb-cli-command))

(defun imb--im-get ()
  "Return current system IME id."
  (string-trim (shell-command-to-string imb-cli-command)))

(defun imb--im-set (id)
  "Synchronously switch system IME to ID."
  (when id (call-process imb-cli-command nil nil nil id)))

(defun imb--ensure-english ()
  "Ensure system IME is English."
  (unless (string= (imb--im-get) imb-english-id)
    (imb--im-set imb-english-id)))

(provide 'im-bridge-core)
;;; im-bridge-core.el ends here
