;;; init-ai.el --- ai tool. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package copilot
  :defines (copilot-completion-map copilot-disable-display-predicates)
  :hook
  ((prog-mode git-commit-setup yaml-mode markdown-mode) . copilot-mode)
  :bind
  (("C-x c g" . copilot-diagnose)
    :map copilot-completion-map
    ("TAB" . copilot-accept-completion)
    ([tab] . copilot-accept-completion)
    ("C-f" . copilot-accept-completion-by-line)
    ("C-n" . copilot-next-completion)
    ("C-p" . copilot-previous-completion)
    ("C-g" . copilot-clear-overlay))
  :custom-face
  (copilot-overlay-face ((t (:inherit shadow :foreground "#7ec0ee"))))
  :config
  (with-eval-after-load 'company
    (add-to-list 'copilot-disable-display-predicates 'company-tooltip-visible-p))
  :custom
  (copilot-idle-delay 0.2)
  (copilot-log-max 0))

(use-package copilot-chat
  :bind (("C-c c" . copilot-chat-transient))
  :custom
  (copilot-chat-frontend 'markdown)
  (copilot-chat-follow t)
  (copilot-chat-markdown-prompt "Respone in 中文:\n")
  (copilot-chat-prompt-test "Write unit tests for the following code:\n")
  (copilot-chat-prompt-optimize "Optimize and refactor the following code:\n")
  (copilot-chat-prompt-explain "Explain the following code:\n"))

(use-package gptel
  :defines (gptel-mode-map gptel-backend)
  :custom
  (gptel-log-level 'info)
  (gptel-post-stream-hook #'gptel-auto-scroll)
  (gptel-post-response-functions #'gptel-end-of-response)
  (gptel-rewrite-default-action #'gptel--rewrite-diff)
  :config
  (setq gptel-model 'gpt-4o
    gptel-backend (gptel-make-gh-copilot "Copilot"
                    :models '(gpt-4o)))
  (gptel-make-openai "GeminiPro"
    :stream t
    :protocol "http"
    :host "localhost:4000"
    :models '(gemini-2.5-pro))
  ;; (gptel-make-openai "Claude-3.7"
  ;;   :stream t
  ;;   :protocol "http"
  ;;   :host "localhost:4000"
  ;;   :models '(claude-3.7)
  ;;   :request-params '(:thinking (:type "enabled" :budget_tokens 4096) :max_tokens 64000))

  (gptel-make-anthropic "Claude" :stream t :key #'gptel-api-key)

  (use-package codel
    :vc (:url "https://github.com/skissue/llm-tool-collection"
          :branch "main"
          :rev :newest))

  (mapcar (apply-partially #'apply #'gptel-make-tool)
    (llm-tool-collection-get-all))

;;   (gptel-make-tool
;;     :name "read_file"
;;     :description "Read and display the contents of a file"
;;     :category "filesystem"
;;     :function (lambda (filepath)
;;                 (with-temp-buffer
;;                   (insert-file-contents (expand-file-name filepath))
;;                   (buffer-string)))
;;     :args (list '(:name "filepath"
;;                    :type string
;;                    :description "Path to the file to read. Supports relative paths and ~.")))

;;   (gptel-make-tool
;;     :name "list_directory"
;;     :description "List the contents of a given directory"
;;     :category "filesystem"
;;     :function (lambda (directory)
;;                 (mapconcat #'identity
;;                   (directory-files directory)
;;                   "\n"))
;;     :args (list '(:name "directory"
;;                    :type string
;;                    :description "The path to the directory to list")))

;;   (gptel-make-tool
;;     :name "make_directory"
;;     :description "Create a new directory with the given name in the specified parent directory"
;;     :category "filesystem"
;;     :function (lambda (parent name)
;;                 (condition-case nil
;;                   (progn
;;                     (make-directory (expand-file-name name parent) t)
;;                     (format "Directory %s created/verified in %s" name parent))
;;                   (error (format "Error creating directory %s in %s" name parent))))
;;     :args (list '(:name "parent"
;;                    :type string
;;                    :description "The parent directory where the new directory should be created, e.g. /tmp")
;;             '(:name "name"
;;                :type string
;;                :description "The name of the new directory to create, e.g. testdir")))
;;   (gptel-make-tool
;;     :name "create_file"
;;     :description "Create a new file with the specified content"
;;     :category "filesystem"
;;     :function (lambda (path filename content)
;;                 (let ((full-path (expand-file-name filename path)))
;;                   (with-temp-buffer
;;                     (insert content)
;;                     (write-file full-path))
;;                   (format "Created file %s in %s" filename path)))
;;     :args (list '(:name "path"
;;                    :type string
;;                    :description "The directory where to create the file")
;;             '(:name "filename"
;;                :type string
;;                :description "The name of the file to create")
;;             '(:name "content"
;;                :type string
;;                :description "The content to write to the file")))

;;   (defun my-gptel--edit_file (file-path file-edits)
;;     "In FILE-PATH, apply FILE-EDITS with pattern matching and replacing."
;;     (if (and file-path (not (string= file-path "")) file-edits)
;;       (with-current-buffer (get-buffer-create "*edit-file*")
;;         (insert-file-contents (expand-file-name file-path))
;;         (let ((inhibit-read-only t)
;;                (case-fold-search nil)
;;                (file-name (expand-file-name file-path))
;;                (edit-success nil))
;;           ;; apply changes
;;           (dolist (file-edit (seq-into file-edits 'list))
;;             (when-let ((line-number (plist-get file-edit :line_number))
;;                         (old-string (plist-get file-edit :old_string))
;;                         (new-string (plist-get file-edit :new_string))
;;                         (is-valid-old-string (not (string= old-string ""))))
;;               (goto-char (point-min))
;;               (forward-line (1- line-number))
;;               (when (search-forward old-string nil t)
;;                 (replace-match new-string t t)
;;                 (setq edit-success t))))
;;           ;; return result to gptel
;;           (if edit-success
;;             (progn
;;               ;; show diffs
;;               (ediff-buffers (find-file-noselect file-name) (current-buffer))
;;               (format "Successfully edited %s" file-name))
;;             (format "Failed to edited %s" file-name))))
;;       (format "Failed to edited %s" file-path)))

;;   (gptel-make-tool
;;     :name "edit_file"
;;     :description "Edit file with a list of edits, each edit contains a line-number,
;; a old-string and a new-string, new-string will replace the old-string at the specified line."
;;     :category "filesystem"
;;     :function #'my-gptel--edit_file
;;     :args (list '(:name "file-path"
;;                    :type string
;;                    :description "The full path of the file to edit")
;;             '(:name "file-edits"
;;                :type array
;;                :items (:type object
;;                         :properties
;;                         (:line_number
;;                           (:type integer :description "The line number of the file where edit starts.")
;;                           :old_string
;;                           (:type string :description "The old-string to be replaced.")
;;                           :new_string
;;                           (:type string :description "The new-string to replace old-string.")))
;;                :description "The list of edits to apply on the file")))

  (gptel-make-tool
    :name "run_command"
    :description "Run a command."
    :category "command"
    :function (lambda (command)
                (with-temp-message (format "Running command: %s" command)
                  (shell-command-to-string command)))
    :args (list
            '(:name "command"
               :type "string"
               :description "Command to run.")))

;;   (gptel-make-tool
;;     :name "echo_message"
;;     :description "Send a message to the *Messages* buffer"
;;     :category "emacs"
;;     :function (lambda (text)
;;                 (message "%s" text)
;;                 (format "Message sent: %s" text))
;;     :args (list '(:name "text"
;;                    :type string
;;                    :description "The text to send to the messages buffer")))

;;   (defun gptel-read-documentation (symbol)
;;     "Read the documentation for SYMBOL, which can be a function or variable."
;;     (let ((sym (intern symbol)))
;;       (cond
;;         ((fboundp sym)
;;           (documentation sym))
;;         ((boundp sym)
;;           (documentation-property sym 'variable-documentation))
;;         (t
;;           (format "No documentation found for %s" symbol)))))

;;   (gptel-make-tool
;;     :name "read_documentation"
;;     :description "Read the documentation for a given function or variable"
;;     :category "emacs"
;;     :function #'gptel-read-documentation
;;     :args (list '(:name "name"
;;                    :type string
;;                    :description "The name of the function or variable whose documentation is to be retrieved")))

;;   (gptel-make-tool
;;     :name "read_buffer"
;;     :description "Return the contents of an Emacs buffer"
;;     :category "emacs"
;;     :function (lambda (buffer)
;;                 (unless (buffer-live-p (get-buffer buffer))
;;                   (error "Error: buffer %s is not live" buffer))
;;                 (with-current-buffer buffer
;;                   (buffer-substring-no-properties (point-min) (point-max))))
;;     :args (list '(:name "buffer"
;;                    :type string
;;                    :description "The name of the buffer whose contents are to be retrieved")))

  (gptel-make-tool
    :name "append_to_buffer"
    :description "Append text to an Emacs buffer. If the buffer does not exist, it will be created."
    :category "emacs"
    :function (lambda (buffer text)
                (with-current-buffer (get-buffer-create buffer)
                  (save-excursion
                    (goto-char (point-max))
                    (insert text)))
                (format "Appended text to buffer %s" buffer))
    :args (list '(:name "buffer"
                   :type string
                   :description "The name of the buffer to append text to.")
            '(:name "text"
               :type string
               :description "The text to append to the buffer.")))

;; (defun codel-edit-buffer (buffer-name old-string new-string)
;; "In BUFFER-NAME, replace OLD-STRING with NEW-STRING."
;; (with-current-buffer buffer-name
;;   (let ((case-fold-search nil))  ;; Case-sensitive search
;;     (save-excursion
;;       (goto-char (point-min))
;;       (let ((count 0))
;;         (while (search-forward old-string nil t)
;;           (setq count (1+ count)))
;;         (if (= count 0)
;;             (format "Error: Could not find text to replace in buffer %s" buffer-name)
;;           (if (> count 1)
;;               (format "Error: Found %d matches for the text to replace in buffer %s" count buffer-name)
;;             (goto-char (point-min))
;;             (search-forward old-string)
;;             (replace-match new-string t t)
;;             (format "Successfully edited buffer %s" buffer-name))))))))

;;   (gptel-make-tool
;;     :name "edit_buffer"
;;     :description "Edits Emacs buffers"
;;     :category "edit"
;;     :function #'codel-edit-buffer
;;     :args '((:name "buffer_name"
;;               :type string
;;               :description "Name of the buffer to modify"
;;               :required t)
;;              (:name "old_string"
;;                :type string
;;                :description "Text to replace (must match exactly)"
;;                :required t)
;;              (:name "new_string"
;;                :type string
;;                :description "Text to replace old_string with"
;;                :required t)))
  )

(use-package gptel-quick
  :vc (:url "https://github.com/karthink/gptel-quick" :branch "main" :rev :newest)
  :after gptel
  :init
  (setq gptel-quick-word-count 48)
  :config
  (setq gptel-quick-system-message
    (lambda (count)
      (format "Provide a most detailed explanation using fewer than %d Chinese characters. \
Prioritize completeness within the character limit." count))))

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (aidermacs-setup-minor-mode)
  :custom
  (aidermacs-backend 'vterm))

(provide 'init-ai)
;;; init-ai.el ends here
