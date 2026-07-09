;;; evil-iedit-state.el --- Evil states to interface iedit mode.  -*- lexical-binding: t; -*-

;; Copyright (C) 2014 syl20bnr
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; Keywords: convenience editing evil iedit mnemonic
;; Created: 12 Dec 2014
;; URL: https://github.com/syl20bnr/evil-iedit-state

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Adds two new Evil states `iedit' and `iedit insert' with expand-region
;; integration.
;;
;; Vendored locally (originally distributed via MELPA) to replace the
;; obsolete `defadvice' expand-region hook with `advice-add'.
;;
;; For more info visit: https://github.com/syl20bnr/evil-iedit-state

;;; Code:

(require 'evil)
(require 'iedit)

(defvar evil-iedit-state-default-state 'normal
  "The state to activate when exiting iedit state.")

(evil-define-state iedit
  "`iedit state' interfacing iedit mode."
  :tag " <E> "
  :enable (normal)
  :cursor box
  :message "-- IEDIT --"
  ;; force iedit mode
  (if (evil-replace-state-p) (call-interactively 'iedit-mode)))

(evil-define-state iedit-insert
  "Replace insert state in `iedit state'."
  :tag " <Ei> "
  :enable (insert)
  :cursor (bar . 2)
  :message "-- IEDIT-INSERT --"
  :input-method t)

(defun evil-iedit-state/iedit-mode (&optional arg)
  "Start `iedit-mode'."
  (interactive "P")
  (when (fboundp 'auto-highlight-symbol-mode)
    (auto-highlight-symbol-mode -1))
  (iedit-mode arg)
  (evil-iedit-state))

(defun evil-iedit-state/quit-iedit-mode ()
  "Quit iedit-mode and return set state `evil-iedit-state-default-state'."
  (interactive)
  (iedit-done)
  (funcall (intern (format "evil-%S-state" evil-iedit-state-default-state))))

(defmacro evil-iedit-state||switch-to-insert-state-after-command
    (command &optional interactive)
  "Call COMMAND and switch to iedit-insert state.
If INTERACTIVE is non-nil then COMMAND is called interactively."
  `(progn
     (if ,interactive
         (call-interactively ',command)
       (funcall ',command))
     ;; required to correctly update the cursors
     (evil-iedit-state)
     (evil-iedit-insert-state)))

(defun evil-iedit-state//goto-overlay-start ()
  "Return the position of the start of the current overlay."
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-start overlay))
      (call-interactively 'evil-beginning-of-line))))

(defun evil-iedit-state//goto-overlay-end ()
  "Return the position of the end of the current overlay."
  (let ((overlay (iedit-find-current-occurrence-overlay)))
    (if overlay
        (goto-char (overlay-end overlay))
      (call-interactively 'evil-end-of-line))))

(defun evil-iedit-state/evil-beginning-of-line (_count)
  "Go to the beginning of the current overlay."
  (interactive "p")
  (evil-iedit-state//goto-overlay-start))

(defun evil-iedit-state/evil-end-of-line ()
  "Go to the end of the current overlay."
  (interactive)
  (evil-iedit-state//goto-overlay-end))

(defun evil-iedit-state/evil-append-line ()
  "Put the point at the end of current overlay and switch to
`iedit-insert state'."
  (interactive)
  (evil-iedit-state||switch-to-insert-state-after-command
   evil-iedit-state//goto-overlay-end))

(defun evil-iedit-state/evil-insert-line ()
  "Put the point at the beginning of current overlay and switch to
`iedit-insert state'."
  (interactive)
  (evil-iedit-state||switch-to-insert-state-after-command
   evil-iedit-state//goto-overlay-start))

(defun evil-iedit-state/substitute ()
  "Wipe all the occurrences and switch to `iedit-insert state'."
  (interactive)
  (iedit-delete-occurrences)
  (evil-iedit-insert-state))

(defun evil-iedit-state/evil-change ()
  "Wipe all the occurrences and switch to `iedit-insert state'."
  (interactive)
  (evil-iedit-state||switch-to-insert-state-after-command evil-change t))

(defun evil-iedit-state/evil-append ()
  "Append and switch to `iedit-insert state'."
  (interactive)
  (evil-iedit-state||switch-to-insert-state-after-command evil-append t))

(defun evil-iedit-state/evil-open-below ()
  "Insert new line below and switch to `iedit-insert state'."
  (interactive)
  (evil-iedit-state||switch-to-insert-state-after-command evil-open-below t))

(defun evil-iedit-state/evil-open-above ()
  "Insert new line above and switch to `iedit-insert state'."
  (interactive)
  (evil-iedit-state||switch-to-insert-state-after-command evil-open-above t))

(defun evil-iedit-state/evil-substitute ()
  "Append and switch to `iedit-insert state'."
  (interactive)
  (evil-iedit-state||switch-to-insert-state-after-command evil-substitute t))

(defun evil-iedit-state/paste-replace (count)
  "Replace the selection with the yanked text."
  (interactive "P")
  (when kill-ring (iedit-delete-occurrences))
  (evil-paste-before count))

;; expand-region integration, add an "e" command
(defun evil-iedit-state/iedit-mode-from-expand-region (&optional arg)
  "Start `iedit-mode'."
  (interactive "P")
  (evil-iedit-state/iedit-mode arg)
  ;; force expand-region temporary overlay map exit
  (setq overriding-terminal-local-map nil))

(defun evil-iedit-state//er-add-edit-binding (return-value)
  "Advise `er/prepare-for-more-expansions-internal' to add an \"e to edit\" hint."
  (cons (concat (car return-value) ", e to edit")
        (cons '("e" evil-iedit-state/iedit-mode-from-expand-region)
              (cdr return-value))))

(with-eval-after-load 'expand-region
  (advice-add 'er/prepare-for-more-expansions-internal :filter-return
    #'evil-iedit-state//er-add-edit-binding))

;; iedit now has an official switch for this; no need to override `iedit-done'.
(setopt iedit-auto-save-occurrence-in-kill-ring nil)

(define-key evil-iedit-state-map "#"   'iedit-number-occurrences)
(define-key evil-iedit-state-map "$"   'evil-iedit-state/evil-end-of-line)
(define-key evil-iedit-state-map "0"   'evil-iedit-state/evil-beginning-of-line)
(define-key evil-iedit-state-map "a"   'evil-iedit-state/evil-append)
(define-key evil-iedit-state-map "A"   'evil-iedit-state/evil-append-line)
(define-key evil-iedit-state-map "c"   'evil-iedit-state/evil-change)
(define-key evil-iedit-state-map "D"   'iedit-delete-occurrences)
(define-key evil-iedit-state-map "F"   'iedit-restrict-function)
(define-key evil-iedit-state-map "gg"  'iedit-goto-first-occurrence)
(define-key evil-iedit-state-map "G"   'iedit-goto-last-occurrence)
(define-key evil-iedit-state-map "i"   'evil-iedit-insert-state)
(define-key evil-iedit-state-map "I"   'evil-iedit-state/evil-insert-line)
(define-key evil-iedit-state-map "J"   'iedit-expand-down-a-line)
(define-key evil-iedit-state-map "K"   'iedit-expand-up-a-line)
(define-key evil-iedit-state-map "L"   'iedit-restrict-current-line)
(define-key evil-iedit-state-map "n"   'iedit-next-occurrence)
(define-key evil-iedit-state-map "N"   'iedit-prev-occurrence)
(define-key evil-iedit-state-map "o"   'evil-iedit-state/evil-open-below)
(define-key evil-iedit-state-map "O"   'evil-iedit-state/evil-open-above)
(define-key evil-iedit-state-map "p"   'evil-iedit-state/paste-replace)
(define-key evil-iedit-state-map "s"   'evil-iedit-state/evil-substitute)
(define-key evil-iedit-state-map "S"   'evil-iedit-state/substitute)
(define-key evil-iedit-state-map "V"   'iedit-show/hide-context-lines)
(define-key evil-iedit-state-map "U"   'iedit-upcase-occurrences)
(define-key evil-iedit-state-map (kbd "C-S-u") 'iedit-downcase-occurrences)
(define-key evil-iedit-state-map (kbd "C-g") 'evil-iedit-state/quit-iedit-mode)
(define-key evil-iedit-state-map (kbd "TAB") 'iedit-toggle-selection)
(define-key evil-iedit-state-map [tab]       'iedit-toggle-selection)
(define-key evil-iedit-state-map [backspace] 'iedit-blank-occurrences)
(define-key evil-iedit-state-map [escape]    'evil-iedit-state/quit-iedit-mode)
;; override overlay keymap to toggle occurrences
(define-key iedit-occurrence-keymap-default
  (kbd "TAB") 'iedit-toggle-selection)
(define-key iedit-occurrence-keymap-default
  [tab] 'iedit-toggle-selection)

(define-key evil-iedit-insert-state-map (kbd "C-g") 'evil-iedit-state/quit-iedit-mode)
(define-key evil-iedit-insert-state-map [escape]    'evil-iedit-state)

;; unbound iedit commands:
;; toggle buffering
;; toggle case sensitive

(provide 'evil-iedit-state)

;;; evil-iedit-state.el ends here
