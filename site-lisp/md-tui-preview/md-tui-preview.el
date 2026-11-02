;;; md-tui-preview.el --- Preview saved Markdown with mdv -*- lexical-binding: t; -*-
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Author: Yuehong Wang <wangyuehong@gmail.com>

;;; Commentary:
;; Serve a saved Markdown file with mdv, then display its URL in the
;; content-independent terminal browser embed.

;;; Code:
(require 'cl-lib)
(require 'subr-x)
(require 'url-util)
(require 'terminal-browser-embed)
(require 'init-custom)

(defvar markdown-mode-command-map)

(defconst my/md-stylesheet
  (expand-file-name "preview.css" (file-name-directory (or load-file-name buffer-file-name)))
  "Stylesheet used by Markdown previews.")

(cl-defstruct my/md-preview
  file directory css process stderr-pipe stdout stderr url browser stopping)

(defvar-local my/md-preview-state nil
  "Markdown server owned by this preview buffer.")

(defun my/md-color (face attribute frame fallback)
  "Return FACE ATTRIBUTE as a CSS hex color on FRAME, or FALLBACK."
  (let* ((color (and (facep face) (face-attribute face attribute frame t)))
         (rgb (and (stringp color) (color-values color frame))))
    (if rgb
        (apply #'format "#%02x%02x%02x" (mapcar (lambda (v) (/ v 257)) rgb))
      fallback)))

(defun my/md-css-font (font)
  "Return FONT quoted for a CSS font-family declaration."
  (unless (and (stringp font) (not (string-empty-p font))
               (not (string-match-p "[[:cntrl:]]" font)))
    (user-error "Markdown preview font family must be a nonempty name"))
  (concat "\"" (replace-regexp-in-string
                  "[\\\"]" (lambda (char) (concat "\\" char)) font t t) "\""))

(cl-defmethod my/md-write-css ((state my/md-preview) frame)
  "Write the current Emacs FRAME colors and configured font for STATE."
  (unless (and (numberp my/md-preview-font-size) (> my/md-preview-font-size 0))
    (user-error "Markdown preview font size must be positive"))
  (let* ((fg (my/md-color 'default :foreground frame nil))
         (bg (my/md-color 'default :background frame nil))
         (properties
          `(("foreground" . ,fg)
            ("background" . ,bg)
            ("link" . ,(my/md-color 'link :foreground frame fg))
            ("muted" . ,(my/md-color 'font-lock-comment-face :foreground frame fg))
            ("code-background" . ,(my/md-color 'fringe :background frame bg))
            ("font-family" . ,(my/md-css-font my/md-preview-font-family))
            ("font-size" . ,(format "%spt" my/md-preview-font-size)))))
    (unless (and fg bg)
      (user-error "Markdown preview cannot resolve Emacs default colors"))
    (dotimes (i 6)
      (push (cons (format "heading-%d" (1+ i))
                  (my/md-color (intern (format "markdown-header-face-%d" (1+ i)))
                               :foreground frame fg))
            properties))
    (with-temp-file (my/md-preview-css state)
      (insert ":root {\n"
              (mapconcat (lambda (property)
                           (format "  --preview-%s: %s;" (car property) (cdr property)))
                         properties "\n")
              "\n}\n")
      (insert-file-contents my/md-stylesheet))))

(cl-defmethod my/md-receive-stderr ((state my/md-preview) text)
  "Collect mdv stderr TEXT for STATE and discover its listening URL."
  (let ((output (concat (or (my/md-preview-stderr state) "") text)))
    (setf (my/md-preview-stderr state)
          (substring output (max 0 (- (length output) 65536)))))
  (when (and (not (my/md-preview-url state))
             (string-match "mdv: http://127\\.0\\.0\\.1:\\([0-9]+\\)/.*  (Ctrl-C to stop)"
                           (my/md-preview-stderr state)))
    (setf (my/md-preview-url state)
          (format "http://127.0.0.1:%s/%s"
                  (match-string 1 (my/md-preview-stderr state))
                  (url-hexify-string (file-name-nondirectory (my/md-preview-file state)))))))

(cl-defmethod my/md-stop ((state my/md-preview))
  "Stop STATE's mdv process and discard its temporary stylesheet."
  (unless (my/md-preview-stopping state)
    (setf (my/md-preview-stopping state) t)
    (when (process-live-p (my/md-preview-process state))
      (delete-process (my/md-preview-process state)))
    (when (process-live-p (my/md-preview-stderr-pipe state))
      (delete-process (my/md-preview-stderr-pipe state)))
    (when (buffer-live-p (my/md-preview-stdout state))
      (kill-buffer (my/md-preview-stdout state)))
    (when (and (my/md-preview-directory state)
               (file-directory-p (my/md-preview-directory state)))
      (delete-directory (my/md-preview-directory state) t))))

(cl-defmethod my/md-start ((state my/md-preview))
  "Start mdv for STATE and wait for its actual listening URL."
  (unless (executable-find "mdv") (user-error "Markdown preview requires mdv"))
  (let ((stderr-pipe
         (make-pipe-process
          :name "md-preview-mdv-stderr" :noquery t
          :filter (lambda (_process text) (my/md-receive-stderr state text)))))
    (setf (my/md-preview-stderr-pipe state) stderr-pipe)
    (setf (my/md-preview-process state)
          (make-process
           :name "md-preview-mdv" :noquery t :connection-type 'pipe
           :buffer (my/md-preview-stdout state) :stderr stderr-pipe
           :command (list (executable-find "mdv") "--no-open" "--host" "127.0.0.1"
                          "--css" (my/md-preview-css state)
                          (my/md-preview-file state))))
    (set-process-sentinel
     (my/md-preview-process state)
     (lambda (process event)
       (when (and (not (process-live-p process)) (not (my/md-preview-stopping state))
                  (my/md-preview-browser state))
         (my/tb-failed (my/md-preview-browser state) "Markdown server stopped" event))))
    (let ((deadline (+ (float-time) 5)))
      (while (and (not (my/md-preview-url state))
                  (process-live-p (my/md-preview-process state))
                  (< (float-time) deadline))
        (accept-process-output stderr-pipe 0.05)))
    (unless (and (my/md-preview-url state)
                 (process-live-p (my/md-preview-process state)))
      (user-error "Mdv did not start: %s"
                  (if (process-live-p (my/md-preview-process state))
                      "startup timed out"
                    (string-trim (or (my/md-preview-stderr state) "process exited")))))))

(defun my/md-refresh ()
  "Refresh the stylesheet and browser for the current Markdown preview."
  (interactive)
  (my/md-write-css my/md-preview-state (selected-frame))
  (my/tb-refresh))

;;;###autoload
(define-derived-mode md-tui-preview-mode terminal-browser-embed-mode "MD-Preview"
  "Display a saved Markdown document in the embedded browser."
  :interactive nil
  (setq-local mode-line-format
              '(" MD | q exit | gr refresh | h back l fwd | C-b/f page | C-u/d half | gg/G top/end")))

;;;###autoload
(defun md-tui-preview-toggle ()
  "Preview this saved Markdown file, or return to its source buffer."
  (interactive)
  (cond
   ((derived-mode-p 'md-tui-preview-mode) (my/tb-close))
   ((not (derived-mode-p 'markdown-mode)) (user-error "Not a Markdown buffer"))
   ((not (and buffer-file-name
              (equal (downcase (or (file-name-extension buffer-file-name) "")) "md")
              (file-readable-p buffer-file-name)))
    (user-error "Markdown preview requires a saved, readable .md file"))
   ((buffer-modified-p) (user-error "Save the Markdown buffer before previewing"))
   (t
    (let*((directory (make-temp-file "emacs-mdv-preview-" t))
           (state (make-my/md-preview :file buffer-file-name :directory directory
                                      :css (expand-file-name "preview.css" directory)
                                      :stdout (generate-new-buffer " *mdv output*"))))
      (condition-case err
          (progn
            (my/md-write-css state (selected-frame))
            (my/md-start state)
            (let ((browser (my/terminal-browser-open-url
                            (my/md-preview-url state) #'md-tui-preview-mode
                            (lambda () (my/md-stop state)))))
              (setf (my/md-preview-browser state) browser)
              (unless (process-live-p (my/md-preview-process state))
                (my/tb-stop browser)
                (user-error "Markdown server exited while opening the browser"))
              (with-current-buffer (my/tb-browser-buffer browser)
                (setq my/md-preview-state state))
              browser))
        (error
         (my/md-stop state)
         (user-error "Cannot start Markdown preview: %s" (error-message-string err))))))))

(with-eval-after-load 'markdown-mode
  (define-key markdown-mode-command-map (kbd "p") #'md-tui-preview-toggle))
(define-key md-tui-preview-mode-map (kbd "q") #'md-tui-preview-toggle)
(define-key md-tui-preview-mode-map (kbd "C-c C-c") #'md-tui-preview-toggle)
(define-prefix-command 'my/md-preview-g-map)
(defvar my/md-preview-g-map)
(define-key md-tui-preview-mode-map (kbd "g") 'my/md-preview-g-map)
(define-key my/md-preview-g-map (kbd "r") #'my/md-refresh)
(define-key my/md-preview-g-map (kbd "g") #'my/tb-key)
(define-key md-tui-preview-mode-map (kbd "h") #'my/tb-browser-back)
(define-key md-tui-preview-mode-map (kbd "l") #'my/tb-browser-forward)
(define-key md-tui-preview-mode-map (kbd "C-b") #'my/tb-key)
(define-key md-tui-preview-mode-map (kbd "C-f") #'my/tb-key)
(define-key md-tui-preview-mode-map (kbd "C-u") #'my/tb-half-page-up)
(define-key md-tui-preview-mode-map (kbd "C-d") #'my/tb-half-page-down)
(define-key md-tui-preview-mode-map (kbd "G") #'my/tb-key)
(dolist (key '("j" "k"))
  (define-key md-tui-preview-mode-map (kbd key) #'my/tb-key))

(provide 'md-tui-preview)
;;; md-tui-preview.el ends here
