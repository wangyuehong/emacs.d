;;; init-evil.el --- evil configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package evil
  :init ;; tweak evil's configuration before loading it
  (setq-default
   evil-auto-indent t
   evil-want-keybinding nil
   evil-default-cursor t
   evil-cross-lines t
   evil-search-module 'evil-search
   evil-symbol-word-search t
   evil-ex-search-vim-style-regexp t
   evil-shift-width 4
   evil-find-skip-newlines nil
   evil-move-cursor-back nil
   evil-want-fine-undo t
   evil-kill-on-visual-paste nil
   )

  ;; Color the evil tag - colors taken from spaceline
  (setq evil-normal-state-tag       (propertize " <N> ")
        evil-emacs-state-tag        (propertize " <M> " 'face '((:background "SkyBlue2"   )))
        evil-insert-state-tag       (propertize " <I> " 'face '((:background "red"        )))
        evil-replace-state-tag      (propertize " <R> " 'face '((:background "chocolate"  )))
        evil-motion-state-tag       (propertize " <M> " 'face '((:background "plum3"      )))
        evil-visual-state-tag       (propertize " <V> " 'face '((:background "cyan"       )))
        evil-operator-state-tag     (propertize " <O> " 'face '((:background "sandy brown"))))
  (evil-mode t)
  :config ;; tweak evil after loading it
  (defalias #'forward-evil-word #'forward-evil-symbol)

  (define-key evil-visual-state-map (kbd "v") 'er/expand-region)
  (define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
  (define-key evil-insert-state-map (kbd "C-k") 'kill-line)
  (define-key evil-insert-state-map (kbd "C-d") 'delete-char)
  (define-key evil-normal-state-map (kbd "C-d") 'delete-char)

  (define-key evil-normal-state-map (kbd "q") 'quit-window)
  (define-key evil-normal-state-map (kbd "s") 'avy-goto-word-1)
  (define-key evil-visual-state-map (kbd "s") 'avy-goto-word-1)

  (define-key evil-normal-state-map "Y" (kbd "y$"))
  (define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
  (define-key evil-visual-state-map (kbd "TAB") 'evil-indent)

  ;; (key-chord-define evil-insert-state-map ";;" "\C-e;")
  (key-chord-define evil-insert-state-map ",," "\C-e,")
  ;; (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

  ;; modes to map to different default states
  (dolist (mode-map '((comint-mode . emacs)
                      (term-mode . emacs)
                      (eshell-mode . emacs)
                      (help-mode . emacs)))
    (evil-set-initial-state `,(car mode-map) `,(cdr mode-map)))

  ;; change mode-line color by evil state
  ;; (lexical-let ((default-color (cons (face-background 'mode-line)
  ;;                                    (face-foreground 'mode-line))))
  ;;   (defun change-mode-line-color ()
  ;;     (let ((color (cond ((minibufferp) default-color)
  ;;                        ;; ((evil-insert-state-p) '("red" . "white"))
  ;;                        ;; ((evil-emacs-state-p)  '("#444488" . "white"))
  ;;                        ((buffer-modified-p)   '("brightblue" . "white"))
  ;;                        (t default-color))))
  ;;       (set-face-foreground 'mode-line-buffer-id (cdr color))
  ;;       (set-face-foreground 'mode-line (cdr color))
  ;;       (set-face-background 'mode-line (car color))
  ;;       )
  ;;     )
  ;;   (add-hook 'post-command-hook 'change-mode-line-color)
  ;;   )
  ) ;; M-x pp-macroexpand-last-sexp here to test use-package

(use-package evil-collection :after evil :config (evil-collection-init))

(use-package evil-surround :after evil :config (global-evil-surround-mode))

(use-package evil-anzu :after evil :config (global-anzu-mode t))

(use-package evil-visualstar :after evil :init (setq evil-visualstar/persistent t) :config (global-evil-visualstar-mode))

(use-package evil-nerd-commenter :after evil)

(use-package evil-iedit-state
  :after (evil iedit)
  :commands (evil-iedit-state evil-iedit-state/iedit-mode)
  :init (setq evil-iedit-state-tag        (propertize " <E> " 'face '((:background "green"      )))
              evil-iedit-insert-state-tag (propertize " <Ei> " 'face '((:background "brightred" )))))

(provide 'init-evil)
;;; init-evil.el ends here
