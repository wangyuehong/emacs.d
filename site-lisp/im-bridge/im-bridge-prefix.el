;;; im-bridge-prefix.el --- Prefix key IME switch for im-bridge -*- lexical-binding:t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; Switch to English IME when pressing prefix keys (C-x, C-c, SPC leader, etc.)
;;
;;; Code:

(require 'im-bridge-core)

(defvar imb--last-checked-time 0
  "Last time we checked the system IM.")

(defcustom imb-prefix-check-interval 0.1
  "Minimum interval (seconds) between IM checks for performance."
  :type 'number
  :group 'im-bridge)

(defcustom imb-prefix-keys '("C-x" "C-c")
  "List of global prefix keys that always trigger IM switch."
  :type '(repeat string)
  :group 'im-bridge)

(defcustom imb-prefix-auto-detect-keys '("SPC")
  "List of keys to auto-detect as prefix in Evil mode.
These keys will trigger IM switch only when bound to a keymap (prefix)
in current Evil state. No need to specify states - it auto-detects
from general.el or other configurations."
  :type '(repeat string)
  :group 'im-bridge)

(defun imb--ensure-english-cached ()
  "Ensure English IM with caching to avoid frequent CLI calls."
  (let ((now (float-time)))
    (when (> (- now imb--last-checked-time) imb-prefix-check-interval)
      (setq imb--last-checked-time now)
      (imb--ensure-english))))

(defun imb--evil-prefix-p (key-vec)
  "Return non-nil if KEY-VEC is bound to a keymap in Evil mode."
  (and (bound-and-true-p evil-local-mode)
       (keymapp (key-binding key-vec))))

(defun imb--make-translator (key &optional predicate)
  "Create a translator for KEY.
PREDICATE is an optional function taking KEY-VEC, trigger only when it returns non-nil."
  (let ((key-vec (kbd key)))
    (lambda (_prompt)
      (when (or (null predicate) (funcall predicate key-vec))
        (imb--ensure-english-cached))
      key-vec)))

(defun imb--install-translators ()
  "Install key translators for prefix keys."
  (dolist (key imb-prefix-keys)
    (define-key input-decode-map (kbd key)
                (imb--make-translator key)))
  (dolist (key imb-prefix-auto-detect-keys)
    (define-key input-decode-map (kbd key)
                (imb--make-translator key #'imb--evil-prefix-p))))

(defun imb--remove-translators ()
  "Remove key translators for prefix keys."
  (dolist (key (append imb-prefix-keys imb-prefix-auto-detect-keys))
    (define-key input-decode-map (kbd key) nil)))

;;;###autoload
(define-minor-mode imb-prefix-mode
  "Global mode: switch to English IM when pressing prefix keys."
  :global t
  (if imb-prefix-mode
      (if (imb--cli-available-p)
          (imb--install-translators)
        (setq imb-prefix-mode nil)
        (imb--warn-cli-unavailable))
    (imb--remove-translators)))

(provide 'im-bridge-prefix)
;;; im-bridge-prefix.el ends here
