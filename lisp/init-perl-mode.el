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

  ;; (setenv "PERL5LIB" "dir_path_1:dir_path_2")

  (key-chord-define cperl-mode-map "--" (smartchr '("->" "=>")))
  ;; (modify-syntax-entry ?_ "w")
  )

(add-hook 'cperl-mode-hook 'subword-mode)

(defun update-perl-ctags ()
  (interactive)
  ;; use universal-ctags
  (shell-command "ctags --language-force=perl -e -R `perl -e 'print join(q{ }, grep { -d } @INC);'`"))

(defun update-smp-ctags ()
  (interactive)
  ;; use universal-ctags
  (shell-command "ctags --language-force=perl -e -R `perl script/run_script.pl -e 'print join(q{ }, grep { -d } @INC);'`"))

(require-package 'tt-mode)
(autoload 'tt-mode "tt-mode")
(add-to-list 'auto-mode-alist '("\\.tt$" . tt-mode))

(provide 'init-perl-mode)
