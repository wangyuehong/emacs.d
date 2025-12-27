;;; im-bridge-evil.el --- Evil integration for im-bridge -*- lexical-binding:t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;;
;; Author:  Yuehong Wang <wangyuehong@gmail.com>
;; URL:     https://github.com/wangyuehong/emacs.d
;; Version: 0.1
;;
;;; Commentary:
;; Save/restore system IME around Evil insert state.
;;
;;; Code:

(require 'im-bridge-core)

(defvar-local imb--saved-system-im nil
  "Buffer-local cache of system IME on insert exit.")

(defvar-local imb--saved-emacs-im nil
  "Buffer-local cache of Emacs input method on insert exit.")

(defun imb--evil-exit-insert ()
  "Save IMEs and switch to English on insert exit."
  ;; Save and switch system IM if not English
  (let ((sys (imb--im-get)))
    (unless (string= sys imb-english-id)
      (setq imb--saved-system-im sys)
      (imb--im-set imb-english-id)))
  ;; Save and deactivate Emacs IM if active
  (when current-input-method
    (setq imb--saved-emacs-im current-input-method)
    (set-input-method nil)))

(defun imb--evil-enter-insert ()
  "Restore IMEs on insert entry."
  (cond
   ;; Priority 1: Restore Emacs IM, ensure system IM is English
   (imb--saved-emacs-im
    (set-input-method imb--saved-emacs-im)
    (setq imb--saved-emacs-im nil)
    (imb--ensure-english)
    (setq imb--saved-system-im nil))
   ;; Priority 2: Restore system IM
   (imb--saved-system-im
    (imb--im-set imb--saved-system-im)
    (setq imb--saved-system-im nil))))

(defun imb--install-evil-hooks ()
  "Attach IM-bridge hooks to Evil insert state."
  (add-hook 'evil-insert-state-exit-hook  #'imb--evil-exit-insert  -100)
  (add-hook 'evil-insert-state-entry-hook #'imb--evil-enter-insert  100))

(defun imb--remove-evil-hooks ()
  "Remove IM-bridge hooks from Evil insert state."
  (remove-hook 'evil-insert-state-exit-hook  #'imb--evil-exit-insert)
  (remove-hook 'evil-insert-state-entry-hook #'imb--evil-enter-insert))

;;;###autoload
(define-minor-mode imb-evil-mode
  "Global mode: save / restore IMEs around Evil insert state."
  :global t
  (if imb-evil-mode
      (if (imb--cli-available-p)
          (with-eval-after-load 'evil (imb--install-evil-hooks))
        (setq imb-evil-mode nil)
        (imb--warn-cli-unavailable))
    (with-eval-after-load 'evil (imb--remove-evil-hooks))))

(provide 'im-bridge-evil)
;;; im-bridge-evil.el ends here
