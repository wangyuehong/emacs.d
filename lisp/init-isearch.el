;; Show number of matches while searching
(when (maybe-require-package 'anzu)
  (add-hook 'after-init-hook 'global-anzu-mode)
  (setq anzu-mode-lighter "")
  (global-set-key [remap query-replace-regexp] 'anzu-query-replace-regexp)
  (global-set-key [remap query-replace] 'anzu-query-replace))

;; Activate occur easily inside isearch
(after-load 'isearch
  ;; DEL during isearch should edit the search string, not jump back to the previous result
  (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

  (when (fboundp 'isearch-occur)
    ;; to match ivy conventions
    (define-key isearch-mode-map (kbd "C-c C-o") 'isearch-occur)))

(provide 'init-isearch)
