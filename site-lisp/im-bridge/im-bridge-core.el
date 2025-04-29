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

(require 'seq)

(defgroup im-bridge nil
  "Bridge system and Emacs input methods."
  :group 'convenience
  :prefix "imb-")

(defcustom imb-cli-command "im-select"
  "CLI tool used to query / set system IME."
  :type 'string
  :safe #'stringp)

(defcustom imb-english-id "com.apple.keylayout.ABC"
  "Identifier for the English system IME."
  :type 'string
  :safe #'stringp)

(defvar-local imb--saved-system-im nil
  "System IME cached on last insert exit (buffer-local).")

(defvar-local imb--saved-emacs-im nil
  "Emacs input method cached on last insert exit (buffer-local).")

(defun imb--im-get ()
  "Return current system IME id, or `imb-english-id' on failure."
  (condition-case _
    (string-trim (shell-command-to-string imb-cli-command))
    (error imb-english-id)))

(defun imb--im-set (id)
  "Synchronously switch system IME to ID."
  (when id (call-process imb-cli-command nil nil nil id)))

(defun imb--ensure-english ()
  "Ensure system IME is English; return t when a switch occurred."
  (unless (string= (imb--im-get) imb-english-id)
    (imb--im-set imb-english-id)
    t))

(provide 'im-bridge-core)
;;; im-bridge-core.el ends here
