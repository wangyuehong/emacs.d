(defmacro after-load (feature &rest body)
  "After FEATURE is loaded, evaluate BODY."
  (declare (indent defun))
  `(eval-after-load ,feature
     '(progn ,@body)))


;;----------------------------------------------------------------------------
;; Handier way to add modes to auto-mode-alist
;;----------------------------------------------------------------------------
(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;;----------------------------------------------------------------------------
;; String utilities missing from core emacs
;;----------------------------------------------------------------------------
(defun string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))

(defun string-rtrim (str)
  "Remove trailing whitespace from `STR'."
  (replace-regexp-in-string "[ \t\n]*$" "" str))


;;----------------------------------------------------------------------------
;; Find the directory containing a given library
;;----------------------------------------------------------------------------
(autoload 'find-library-name "find-func")
(defun directory-of-library (library-name)
  "Return the directory in which the `LIBRARY-NAME' load file is found."
  (file-name-as-directory (file-name-directory (find-library-name library-name))))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-current-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

;; https://github.com/redguardtoo/emacs.d
;;----------------------------------------------------------------------------
;; convert a buffer from dos ^M end of lines to unix end of lines
;;----------------------------------------------------------------------------
(defun dos2unix ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\r" nil t) (replace-match "")))


;;----------------------------------------------------------------------------
;; unix to dos
;;----------------------------------------------------------------------------
(defun unix2dos ()
  (interactive)
  (goto-char (point-min))
  (while (search-forward "\n" nil t) (replace-match "\r\n")))


;;----------------------------------------------------------------------------
;; copy filename of current buffer
;;----------------------------------------------------------------------------
(defun copy-current-filename ()
  "copy file name (NOT full path) into the yank ring"
  (interactive)
  (let ((filename))
    (when buffer-file-name
      (setq filename (file-name-nondirectory buffer-file-name))
      (kill-new filename)
      (message "filename %s => yank ring" filename)
      )))


;;----------------------------------------------------------------------------
;; copy full path
;;----------------------------------------------------------------------------
(defun copy-current-file-full-path()
  "copy full path into the yank ring"
  (interactive)
  (when buffer-file-name
    (kill-new (file-truename buffer-file-name))
    (message "full path %s => yank ring" (file-truename buffer-file-name))
    ))


;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (browse-url (concat "file://" (buffer-file-name))))


(provide 'init-utils)
