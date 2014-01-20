;; Use C-f during file selection to switch to regular find-file
(ido-mode t)
(ido-everywhere t)
(setq ido-enable-flex-matching t)
(setq ido-use-filename-at-point nil)
(setq ido-auto-merge-work-directories-length -1)
(setq ido-use-virtual-buffers t)

(when (eval-when-compile (>= emacs-major-version 24))
 (require-package 'ido-ubiquitous)
 (ido-ubiquitous-mode t))

;; Use smex to handle M-x
(require-package 'smex)
(global-set-key [remap execute-extended-command] 'smex)
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))

(require-package 'idomenu)

;; Allow the same buffer to be open in different frames
(setq ido-default-buffer-method 'selected-window)

(when (eval-when-compile (< emacs-major-version 24))
 (defun sanityinc/ido-choose-from-recentf ()
   "Use ido to select a recently opened file from the `recentf-list'"
   (interactive)
   (if (and ido-use-virtual-buffers (fboundp 'ido-toggle-virtual-buffers))
       (ido-switch-buffer)
     (find-file (ido-completing-read "Open file: " recentf-list nil t))))

 (global-set-key [(meta f11)] 'sanityinc/ido-choose-from-recentf))

(when *is-a-mac*
  (add-to-list 'ido-ignore-files "\\.DS_Store"))

(defun wangyh/ido-define-keys()
  (define-key ido-completion-map (kbd "C-h") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-l") 'ido-prev-match))
(add-hook 'ido-setup-hook 'wangyh/ido-define-keys)

(provide 'init-ido)
