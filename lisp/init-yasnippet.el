;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package yasnippet
  :diminish yas-minor-mode
  :hook (after-init . yas-global-mode)
  :config
  (defun yasnippet-disable-inline (fn cmd &optional arg &rest _ignore)
    "Enable yasnippet but disable it inline."
    (if (eq cmd 'prefix)
        (when-let ((prefix (funcall fn 'prefix)))
          (unless (memq (char-before (- (point) (length prefix)))
                        '(?. ?< ?> ?\( ?\) ?\[ ?{ ?} ?\" ?' ?`))
            prefix))
      (progn
        (when (and (bound-and-true-p lsp-mode)
                   arg (not (get-text-property 0 'yas-annotation-patch arg)))
          (let* ((name (get-text-property 0 'yas-annotation arg))
                 (snip (format "%s (Snippet)" name))
                 (len (length arg)))
            (put-text-property 0 len 'yas-annotation snip arg)
            (put-text-property 0 len 'yas-annotation-patch t arg)))
        (funcall fn cmd arg))))
  (advice-add #'company-yasnippet :around #'yasnippet-disable-inline))

(use-package yasnippet-snippets
  :after yasnippet)

(provide 'init-yasnippet)
