;;; init-lsp.el --- lsp by eglot. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Python LSP server setting, used for eglot server selection.
;; Configure per-project via .dir-locals.el in project root:
;;   ((python-ts-mode . ((my/python-lsp-server . ty))))
(defvar-local my/python-lsp-server 'basedpyright
  "Python LSP server. Values: `basedpyright', `ty'.")
(put 'my/python-lsp-server 'safe-local-variable
  (lambda (v) (memq v '(basedpyright ty))))

(use-package eglot
  :functions (eglot-server-capable eglot-format-buffer eglot--project
               eglot--major-modes eglot-path-to-uri)
  :preface
  (defun my/eglot-organize-imports ()
    "Apply LSP source.organizeImports if the server supports it.

Calls `eglot-code-actions` with the kind source.organizeImports and
non-interactively applies it when supported by the server."
    (eglot-code-actions nil nil "source.organizeImports" t))

  (defun my/eglot-setup-hooks ()
    "Add before-save hooks to organize imports and format via the LSP server."
    (add-hook 'before-save-hook #'my/eglot-organize-imports nil t)
    (when (eglot-server-capable :documentFormattingProvider)
      (add-hook 'before-save-hook #'eglot-format-buffer nil t)))

  (defun my/python-lsp-contact (_interactive)
    "Return eglot contact for Python based on `my/python-lsp-server'."
    (pcase my/python-lsp-server
      ('basedpyright '("basedpyright-langserver" "--stdio"))
      ('ty '("ty" "server"))))

  (defun my/python-venv-python ()
    "Return the nearest ancestor `.venv/bin/python' interpreter, or nil.
Walk up from `default-directory'.  Eglot evaluates
`eglot-workspace-configuration' in a temp buffer whose `default-directory'
is the requesting workspace folder's directory (or the project root), so
this resolves the venv nearest that folder, covering both a single-project
layout and a monorepo where each app owns a `.venv'."
    (when-let* ((dir (locate-dominating-file
                       default-directory
                       (lambda (d)
                         (file-exists-p (expand-file-name ".venv/bin/python" d))))))
      (expand-file-name ".venv/bin/python" dir)))

  (defun my/python-execution-roots (project)
    "Absolute directories of nested Python sub-projects under PROJECT.
A sub-project is a directory below PROJECT's root holding its own
`pyproject.toml' (e.g. one uv-managed app in a monorepo).  Found via
`project-files' so the search stays within the project's own tracked
files (respects `.gitignore'; submodules excluded via
`project-vc-merge-submodules').  Used by `eglot-workspace-folders' to
give each app its own LSP workspace folder."
    (let ((root (project-root project)))
      (seq-remove
        (lambda (dir) (file-equal-p dir root))
        (mapcar #'file-name-directory
          (seq-filter (lambda (f)
                        (string= (file-name-nondirectory f) "pyproject.toml"))
            (project-files project))))))

  (defun my/eglot-workspace-configuration (&optional _server)
    "Return per-language eglot workspace configuration.
gopls settings are static.  basedpyright's `typeCheckingMode' is lowered
to \"standard\" (its default is the far stricter \"recommended\", which
floods untyped libraries like Django with Unknown-family warnings).  Eglot
matches the server's requested config section (\"basedpyright\") against
this alist's keys as a flat string, so the key must be `:basedpyright'
rather than a dotted `:basedpyright.analysis'; within that section,
basedpyright itself reads `typeCheckingMode' off the top level, not nested
under `analysis'.  Python's `pythonPath' points at the nearest uv `.venv'
so imports resolve against the project venv; it is omitted when no
`.venv' is found."
    (append
      ;; gopls settings follow upstream recommendations, see
      ;; https://tip.golang.org/gopls/editor/emacs
      '((:gopls
          . ((completeUnimported . t)
              (gofumpt           . t)
              (staticcheck       . t)
              (analyses . ((unusedparams . t)
                            (unusedwrite  . t)
                            (nilness      . t)
                            (shadow       . t)
                            ;; Suppress selected Staticcheck style checks
                            (ST1000       . :json-false)
                            (ST1020       . :json-false)
                            (ST1021       . :json-false)))))
         (:basedpyright
           . ((typeCheckingMode . "standard"))))
      (when-let* ((python (my/python-venv-python)))
        `((:python . ((pythonPath . ,python)))))))
  :bind (:map eglot-mode-map
          ("C-c l t" . eglot-find-typeDefinition)
          ("C-c l i" . eglot-find-implementation)
          ("C-c l a" . eglot-code-actions)
          ("C-c l e" . eglot-rename)
          ("C-c l r" . eglot-reconnect))

  :hook ((eglot-managed-mode . my/eglot-setup-hooks)
          (eglot-managed-mode . eglot-inlay-hints-mode))

  :init
  (setq eglot-stay-out-of '(company))
  (setq read-process-output-max (* 1024 1024)) ; 1MB
  ;; Excludes submodules from eglot's file-watching to avoid exhausting
  ;; the fd limit in submodule-heavy monorepos.
  (setq project-vc-merge-submodules nil)
  ;; Set Eglot user options explicitly for robustness
  (setq eglot-autoshutdown t
    eglot-confirm-server-edits nil
    eglot-events-buffer-config '(:size 0)
    eglot-extend-to-xref t
    eglot-report-progress nil
    eglot-sync-connect nil
    eglot-watch-files-outside-project-root nil)

  :config
  (add-to-list 'eglot-server-programs
    '((python-mode python-ts-mode) . my/python-lsp-contact))

  ;; Installs the per-language configuration built above.
  (setq-default eglot-workspace-configuration #'my/eglot-workspace-configuration)

  ;; Gives each Python sub-project its own workspace folder so basedpyright
  ;; pulls a per-app `pythonPath' (a single server has only one otherwise).
  ;; Needs the raised fd limit in ~/.dotfiles/.zshrc: overlapping folders
  ;; duplicate file watches.
  (cl-defmethod eglot-workspace-folders :around ((server eglot-lsp-server))
    (let ((folders (cl-call-next-method)))
      (if (cl-intersection '(python-mode python-ts-mode)
            (eglot--major-modes server))
        (vconcat folders
          (mapcar (lambda (dir)
                    (list :uri (eglot-path-to-uri dir)
                      :name (abbreviate-file-name dir)))
            (my/python-execution-roots (eglot--project server))))
        folders))))

(use-package consult-eglot
  :after (consult eglot)
  :bind (:map eglot-mode-map
          ("C-c l l" . consult-eglot-symbols))
  :custom
  (consult-eglot-show-kind-name t))

(provide 'init-lsp)
;;; init-lsp.el ends here
