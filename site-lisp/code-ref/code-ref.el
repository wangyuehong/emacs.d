;;; code-ref.el --- Copy code references to clipboard -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; Interactive commands for copying code references and snippets.
;; Supports copying buffer paths and region locations with optional content.
;;
;;; Code:

(require 'code-ref-core)

;;; Buffer Path Commands

(defun cref--copy-buffer-path-generic (style &optional message-prefix)
  "Generic function to copy buffer path based on STYLE.
STYLE determines the path format.
MESSAGE-PREFIX customizes the message shown."
  (let* ((path (cref--get-path-by-style style))
         (to-clipboard (cref--copy-to-clipboard path)))
    (message "%s%s: %s"
             (or message-prefix "Copied")
             (if to-clipboard "" " to kill-ring")
             path)))

;;;###autoload
(defun cref-copy-buffer-path ()
  "Copy the buffer's file path to the clipboard.
If the file is part of a Git repository, copy the relative path;
otherwise, copy the full absolute path."
  (interactive)
  (cref--copy-buffer-path-generic 'display))

;;;###autoload
(defun cref-copy-buffer-absolute-path ()
  "Copy the buffer's absolute file path to the clipboard."
  (interactive)
  (cref--copy-buffer-path-generic 'absolute "Copied absolute path"))

;;;###autoload
(defun cref-copy-buffer-file-name ()
  "Copy the buffer's file name (without directory) to the clipboard."
  (interactive)
  (cref--copy-buffer-path-generic 'filename "Copied file name"))

;;;###autoload
(defun cref-copy-buffer-git-path ()
  "Copy the buffer's Git-relative path to the clipboard."
  (interactive)
  (cref--copy-buffer-path-generic 'git))

;;; Region Location Core

(defun cref--copy-region-location-core (style &optional with-content)
  "Copy region location to clipboard.
STYLE: \\='display, \\='absolute, \\='git, or \\='filename.
WITH-CONTENT: if non-nil, include region content as Markdown code block."
  (let* ((bounds (cref--get-region-or-line))
         (is-region (plist-get bounds :is-region))
         (saved (cref--save-buffer-if-modified))
         (location-path (cref--get-path-by-style style))
         (location-string (cref--get-region-location-string location-path bounds))
         (final-string (if with-content
                           (format "%s\n%s" location-string
                                   (cref--get-region-content-with-fence bounds))
                         location-string))
         (to-clipboard (cref--copy-to-clipboard final-string)))
    (message "%sCopied %s%s: %s"
             (if saved "Saved. " "")
             (if with-content
                 (if is-region "region" "line")
               "location")
             (if to-clipboard "" " to kill-ring")
             location-string)
    (when is-region
      (deactivate-mark))))

;;; Copy Region With Content

;;;###autoload
(defun cref-copy-region-with-location ()
  "Copy region with location and content as Markdown code block."
  (interactive)
  (cref--copy-region-location-core 'display t))

;;;###autoload
(defun cref-copy-region-with-absolute-location ()
  "Copy region with absolute file path location."
  (interactive)
  (cref--copy-region-location-core 'absolute t))

;;;###autoload
(defun cref-copy-region-with-git-location ()
  "Copy region with Git-relative path location."
  (interactive)
  (cref--copy-region-location-core 'git t))

;;;###autoload
(defun cref-copy-region-with-filename-location ()
  "Copy region with filename-only location."
  (interactive)
  (cref--copy-region-location-core 'filename t))

;;; Copy Region Location Only

;;;###autoload
(defun cref-copy-region-location ()
  "Copy region location only (without content)."
  (interactive)
  (cref--copy-region-location-core 'display))

;;;###autoload
(defun cref-copy-region-location-absolute ()
  "Copy region location with absolute file path."
  (interactive)
  (cref--copy-region-location-core 'absolute))

;;;###autoload
(defun cref-copy-region-location-git ()
  "Copy region location with Git-relative path."
  (interactive)
  (cref--copy-region-location-core 'git))

;;;###autoload
(defun cref-copy-region-location-filename ()
  "Copy region location with filename only."
  (interactive)
  (cref--copy-region-location-core 'filename))

(provide 'code-ref)
;;; code-ref.el ends here
