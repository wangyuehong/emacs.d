;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package elec-pair
  :ensure nil
  :hook (after-init . electric-pair-mode)
  :init (setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit))

(use-package autorevert
  :ensure nil
  :diminish
  :hook (after-init . global-auto-revert-mode))

(use-package uniquify
  :ensure nil
  :init
  (setq
   uniquify-buffer-name-style 'reverse
   ;; uniquify-buffer-name-style 'post-forward-angle-brackets ; Show path if names are same
   uniquify-separator " • "
   uniquify-after-kill-buffer-p t
   uniquify-ignore-buffers-re "^\\*"
   ))

(use-package mode-line-bell
  :hook (after-init . mode-line-bell-mode))

(use-package undo-tree
  :diminish
  :hook (after-init . global-undo-tree-mode)
  :bind (("C-r" . undo-tree-undo)))

(use-package subword
  :ensure nil
  :diminish
  :hook ((prog-mode . subword-mode)
         (minibuffer-setup . subword-mode)))

;; (add-hook 'after-init-hook 'show-paren-mode)

(use-package expand-region
  :init (setq expand-region-contract-fast-key "z"))

(use-package highlight-escape-sequences
  :hook (after-init . hes-mode))

(use-package iedit
  :bind (("C-;" . iedit-mode)
         :map isearch-mode-map ("C-;" . iedit-mode-from-isearch)
         :map esc-map ("C-;" . iedit-execute-last-modification)
         :map help-map ("C-;" . iedit-mode-toggle-on-function)))

(use-package simple
  :ensure nil
  :hook (((prog-mode markdown-mode conf-mode) . enable-trailing-whitespace))
  :init
  (setq column-number-mode t
        line-number-mode t
        line-move-visual nil
        track-eol t
        set-mark-command-repeat-pop t)

  (setq-default show-trailing-whitespace nil) ; Don't show trailing whitespace by default
  (defun enable-trailing-whitespace()
    "Show trailing spaces and delete on saving."
    (setq show-trailing-whitespace t)
    (add-hook 'before-save-hook #'delete-trailing-whitespace nil t)))

(use-package whitespace
  :ensure nil
  :hook (((prog-mode markdown-mode conf-mode) . whitespace-mode))
  :config
  (setq whitespace-style
        '(face spaces tabs newline space-mark tab-mark newline-mark lines-tail))
  (setq whitespace-line-column 120) ;; config for lines-tail style
  (setq whitespace-space-regexp "\\(\x3000+\\)")
  (setq whitespace-display-mappings
        '(
          (space-mark ?\x3000 [9633])
          ;;(space-mark 32 [183] [46]) ; normal space
          (space-mark 160 [164] [95])
          (space-mark 2208 [2212] [95])
          (space-mark 2336 [2340] [95])
          (space-mark 3616 [3620] [95])
          (space-mark 3872 [3876] [95])
          ;;(newline-mark 10 [8629 10]) ;; newlne
          (tab-mark 9 [187 9] [92 9]) ;; tab
          ))
  ;; color
  ;; (set-face-foreground 'whitespace-newline "#5c5cff")
  ;; (set-face-background 'whitespace-newline nil)

  (set-face-foreground 'whitespace-space "#5c5cff")
  (set-face-background 'whitespace-space nil)

  (set-face-foreground 'whitespace-tab "#5c5cff")
  (set-face-background 'whitespace-tab nil))

;; (use-package whitespace
;;   :hook (after-init . global-whitespace-mode)
;;   :config
;;   ;; Don't use different background for tabs.
;;   (face-spec-set 'whitespace-tab
;;                  '((t :background unspecified)))
;;   ;; Only use background and underline for long lines, so we can still have
;;   ;; syntax highlight.

;;   ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
;;   ;; is it's due to the variables with the same name as the faces in
;;   ;; whitespace.el.  Anyway, we have to manually set some attribute to
;;   ;; unspecified here.
;;   (face-spec-set 'whitespace-line
;;                  '((((background light))
;;                     :background "#d8d8d8" :foreground unspecified
;;                     :underline t :weight unspecified)
;;                    (t
;;                     :background "#404040" :foreground unspecified
;;                     :underline t :weight unspecified)))

;;   ;; Use softer visual cue for space before tabs.
;;   (face-spec-set 'whitespace-space-before-tab
;;                  '((((background light))
;;                     :background "#d8d8d8" :foreground "#de4da1")
;;                    (t
;;                     :inherit warning
;;                     :background "#404040" :foreground "#ee6aa7")))

;;   (setq
;;    whitespace-line-column nil
;;    whitespace-style
;;    '(face             ; visualize things below:
;;      empty            ; empty lines at beginning/end of buffer
;;      lines-tail       ; lines go beyond `fill-column'
;;      space-before-tab ; spaces before tab
;;      trailing         ; trailing blanks
;;      tabs             ; tabs (show by face)
;;      tab-mark         ; tabs (show by symbol)
;;      )))

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :init
  (setq flyspell-issue-message-flag nil)
  (setq ispell-program-name "aspell")
  (setq ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  )

(use-package youdao-dictionary)

;; (use-package flyspell-lazy
;;   :after flyspell
;;   :config (flyspell-lazy-mode 1)
;;   )
(provide 'init-edit)
