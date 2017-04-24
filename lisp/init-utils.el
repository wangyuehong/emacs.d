(if (fboundp 'with-eval-after-load)
    (defalias 'after-load 'with-eval-after-load)
  (defmacro after-load (feature &rest body)
    "After FEATURE is loaded, evaluate BODY."
    (declare (indent defun))
    `(eval-after-load ,feature
       '(progn ,@body))))


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
(defun sanityinc/string-all-matches (regex str &optional group)
  "Find all matches for `REGEX' within `STR', returning the full match string or group `GROUP'."
  (let ((result nil)
        (pos 0)
        (group (or group 0)))
    (while (string-match regex str pos)
      (push (match-string group str) result)
      (setq pos (match-end group)))
    result))


;;----------------------------------------------------------------------------
;; Delete the current file
;;----------------------------------------------------------------------------
(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (unless (buffer-file-name)
    (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))


;;----------------------------------------------------------------------------
;; Rename the current file
;;----------------------------------------------------------------------------
(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (progn
      (when (file-exists-p filename)
        (rename-file filename new-name 1))
      (set-visited-file-name new-name)
      (rename-buffer new-name))))

;;----------------------------------------------------------------------------
;; Browse current HTML file
;;----------------------------------------------------------------------------
(defun browse-current-file ()
  "Open the current file as a URL using `browse-url'."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (if (and (fboundp 'tramp-tramp-file-p)
             (tramp-tramp-file-p file-name))
        (error "Cannot open tramp file")
      (browse-url (concat "file://" file-name)))))

      (require 'cl)

      (defmacro with-selected-frame (frame &rest forms)
        (let ((prev-frame (gensym))
              (new-frame (gensym)))
          `(progn
             (let* ((,new-frame (or ,frame (selected-frame)))
                    (,prev-frame (selected-frame)))
               (select-frame ,new-frame)
               (unwind-protect
                   (progn ,@forms)
                 (select-frame ,prev-frame))))))

      (defvar load-user-customized-major-mode-hook t)
      (defvar cached-normal-file-full-path nil)

      (defun buffer-too-big-p ()
        (or (> (buffer-size) (* 5000 64))
            (> (line-number-at-pos (point-max)) 5000)))

      (defun is-buffer-file-temp ()
        (interactive)
        "If (buffer-file-name) is nil or a temp file or HTML file converted from org file"
        (let ((f (buffer-file-name))
              org
              (rlt t))
          (cond
           ((not load-user-customized-major-mode-hook) t)
           ((not f)
            ;; file does not exist at all
            (setq rlt t))
           ((string= f cached-normal-file-full-path)
            (setq rlt nil))
           ((string-match (concat "^" temporary-file-directory) f)
            ;; file is create from temp directory
            (setq rlt t))
           ((and (string-match "\.html$" f)
                 (file-exists-p (setq org (replace-regexp-in-string "\.html$" ".org" f))))
            ;; file is a html file exported from org-mode
            (setq rlt t))
           (t
            (setq cached-normal-file-full-path f)
            (setq rlt nil)))
          rlt))

      (defun my-guess-mplayer-path ()
        (let* ((rlt "mplayer"))
          (cond
           (*is-a-mac* (setq rlt "mplayer -quiet"))
           (*linux* (setq rlt "mplayer -quiet -stop-xscreensaver"))
           (*cygwin*
            (if (file-executable-p "/cygdrive/c/mplayer/mplayer.exe")
                (setq rlt "/cygdrive/c/mplayer/mplayer.exe -quiet")
              (setq rlt "/cygdrive/d/mplayer/mplayer.exe -quiet")))
           (t ; windows
            (if (file-executable-p "c:\\\\mplayer\\\\mplayer.exe")
                (setq rlt "c:\\\\mplayer\\\\mplayer.exe -quiet")
              (setq rlt "d:\\\\mplayer\\\\mplayer.exe -quiet"))))
          rlt))

      (defun my-guess-image-viewer-path (file &optional is-stream)
        (let ((rlt "mplayer"))
          (cond
           (*is-a-mac*
            (setq rlt
                  (format "open %s &" file)))
           (*linux*
            (setq rlt
                  (if is-stream (format "curl -L %s | feh -F - &" file) (format "feh -F %s &" file))))
           (*cygwin* (setq rlt "feh -F"))
           (t ; windows
            (setq rlt
                  (format "rundll32.exe %SystemRoot%\\\\System32\\\\\shimgvw.dll, ImageView_Fullscreen %s &" file))))
          rlt))

      ;; {{ diff region SDK
      (defun diff-region-exit-from-certain-buffer (buffer-name)
        (bury-buffer buffer-name)
        (winner-undo))

      (defmacro diff-region-open-diff-output (content buffer-name)
        `(let ((rlt-buf (get-buffer-create ,buffer-name)))
          (save-current-buffer
            (switch-to-buffer-other-window rlt-buf)
            (set-buffer rlt-buf)
            (erase-buffer)
            (insert ,content)
            (diff-mode)
            (goto-char (point-min))
            ;; evil keybinding
            (if (fboundp 'evil-local-set-key)
                (evil-local-set-key 'normal "q"
                                    (lambda ()
                                      (interactive)
                                      (diff-region-exit-from-certain-buffer ,buffer-name))))
            ;; Emacs key binding
            (local-set-key (kbd "C-c C-c")
                           (lambda ()
                             (interactive)
                             (diff-region-exit-from-certain-buffer ,buffer-name)))
            )))

      ;; }}

(provide 'init-utils)
