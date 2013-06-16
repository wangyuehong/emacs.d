;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

;;(require-package 'anything)

(defun my-cperl-mode-defaults ()
  (setq cperl-indent-level 4)
  (setq cperl-continued-statement-offset 0)
  ;; cperl-hairy affects all those variables, but I prefer
  ;; a more fine-grained approach as far as they are concerned
  (setq cperl-font-lock t)
  (setq cperl-electric-lbrace-space t)
  (setq cperl-electric-parens nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-info-on-command-no-prompt t)
  (setq cperl-clobber-lisp-bindings t)
  (setq cperl-lazy-help-time 3)
  (modify-syntax-entry ?_ "w")
  ;; if you want all the bells and whistles
  ;; (setq cperl-hairy)

  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil)
  (setq cperl-invalid-face nil)
  
;;  (setenv "PERL5LIB" "/home/ou.g/dev/HEAD/ATL/lib:/home/ou.g/dev/HEAD/Catalyst-Shanon/lib:/home/ou.g/dev/HEAD/SS/lib:/home/ou.g/dev/HEAD/SS/t/lib")

  ;;(require 'perl-completion)
  ;;(perl-completion-mode t)
  ;;(add-to-list 'ac-sources 'ac-source-perl-completion)
  ;;(add-to-list 'ac-sources 'ac-source-yasnippet)

  (load "pde-load")
  ;; run perl with f5
  (defun perl-eval () "Run selected region as Perl code" (interactive)
  (shell-command-on-region (mark) (point) "perl "))
  ;; (global-set-key (kbd "<f5>") 'perl-eval)

)


(add-hook 'cperl-mode-hook 'my-cperl-mode-defaults)

(require-package 'tt-mode)
(autoload 'tt-mode "tt-mode")
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))


(require-package 'yaml-mode)
(add-to-list 'auto-mode-alist '("\\.yaml$" . yaml-mode))
(provide 'init-perl)
