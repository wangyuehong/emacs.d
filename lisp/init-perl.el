;; use cperl-mode instead of perl-mode
(defalias 'perl-mode 'cperl-mode)

(require-package 'cperl-mode)

(after-load 'cperl-mode
  (setq cperl-close-paren-offset -4)
  (setq cperl-continued-statement-offset 4)
  (setq cperl-indent-level 4)
  (setq cperl-indent-parens-as-block t)
  (setq cperl-tab-always-indent t)
  (setq cperl-font-lock t)
  (setq cperl-electric-lbrace-space t)
  (setq cperl-electric-parens nil)
  (setq cperl-electric-linefeed nil)
  (setq cperl-electric-keywords nil)
  (setq cperl-info-on-command-no-prompt t)
  (setq cperl-clobber-lisp-bindings t)
  (setq cperl-lazy-help-time 3)
  (setq cperl-invalid-face nil)

  (set-face-background 'cperl-array-face nil)
  (set-face-background 'cperl-hash-face nil)

  ;;  (setenv "PERL5LIB" "/home/ou.g/dev/HEAD/ATL/lib:/home/ou.g/dev/HEAD/Catalyst-Shanon/lib:/home/ou.g/dev/HEAD/SS/lib:/home/ou.g/dev/HEAD/SS/t/lib")

  (modify-syntax-entry ?_ "w"))

(require-package 'tt-mode)
(autoload 'tt-mode "tt-mode")
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

(provide 'init-perl)
