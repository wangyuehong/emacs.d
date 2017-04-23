(require-package 'unfill)

(when (fboundp 'electric-pair-mode)
  (electric-pair-mode))
(when (eval-when-compile (version< "24.4" emacs-version))
  (electric-indent-mode 1))

;;----------------------------------------------------------------------------
;; Some basic preferences
;;----------------------------------------------------------------------------
(setq-default
 blink-cursor-interval 0.4
 bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
 buffers-menu-max-size 30
 case-fold-search t
 column-number-mode t
 compilation-scroll-output t
 delete-selection-mode t
 ediff-split-window-function 'split-window-horizontally
 ediff-window-setup-function 'ediff-setup-windows-plain
 indent-tabs-mode nil
 line-spacing 0.2
 make-backup-files nil
 mouse-yank-at-point t
 save-interprogram-paste-before-kill t
 scroll-preserve-screen-position 'always
 set-mark-command-repeat-pop t
 tooltip-delay 1.5
 truncate-lines nil
 scroll-conservatively 10000
 ring-bell-function 'ignore
 truncate-partial-width-windows nil)

(global-auto-revert-mode)
(setq global-auto-revert-non-file-buffers t
      auto-revert-verbose nil)

(transient-mark-mode t)


 ;;; A simple visible bell which works in all terminal types

(defun sanityinc/flash-mode-line ()
  (invert-face 'mode-line)
  (run-with-timer 0.05 nil 'invert-face 'mode-line))

(setq-default
 ring-bell-function 'sanityinc/flash-mode-line)

(when (maybe-require-package 'indent-guide)
  (add-hook 'prog-mode-hook 'indent-guide-mode)
  (add-hook 'yaml-mode-hook 'indent-guide-mode)
  (after-load 'indent-guide
    (diminish 'indent-guide-mode)))

;; (require-package 'nlinum)

(when (require-package 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(when (fboundp 'global-prettify-symbols-mode)
  (global-prettify-symbols-mode))

(require-package 'undo-tree)
(global-undo-tree-mode)
(diminish 'undo-tree-mode)

;;----------------------------------------------------------------------------
;; Zap *up* to char is a handy pair for zap-to-char
;;----------------------------------------------------------------------------
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR.")
(global-set-key (kbd "M-Z") 'zap-up-to-char)

;;----------------------------------------------------------------------------
;; Don't disable narrowing commands
;;----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;----------------------------------------------------------------------------
;; Show matching parens
;;----------------------------------------------------------------------------
(show-paren-mode 1)

;;----------------------------------------------------------------------------
;; Expand region
;;----------------------------------------------------------------------------
(require-package 'expand-region)
;; (global-set-key (kbd "C-=") 'er/expand-region)

;;----------------------------------------------------------------------------
;; Don't disable case-change functions
;;----------------------------------------------------------------------------
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)


;;----------------------------------------------------------------------------
;; Rectangle selections, and overwrite text when the selection is active
;;----------------------------------------------------------------------------
(cua-selection-mode t)                  ; for rectangles, CUA is nice

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

;; (global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

(require-package 'highlight-escape-sequences)
(hes-mode)

(require-package 'iedit)

(provide 'init-editing-utils)
