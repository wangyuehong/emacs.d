;;; This file bootstraps the configuration, which is divided into
;;; a number of other files.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'init-benchmarking) ;; Measure startup time

;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(defconst *spell-check-support-enabled* nil) ;; Enable with t if you prefer
(defconst *is-a-mac* (eq system-type 'darwin))

;;----------------------------------------------------------------------------
;; Bootstrap config
;;----------------------------------------------------------------------------
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(require 'init-compat)
(require 'init-utils)
(require 'init-site-lisp) ;; Must come before elpa, as it may provide package.el
(require 'init-elpa)      ;; Machinery for installing required packages
(require 'init-exec-path) ;; Set up $PATH

;;----------------------------------------------------------------------------
;; Load configs for specific features and modes
;;----------------------------------------------------------------------------

(require-package 'wgrep)
;;(require-package 'project-local-variables)
(require-package 'diminish)
;;(require-package 'scratch)
(require-package 'mwe-log-commands)

(require 'init-frame-hooks)
(require 'init-xterm)
(require 'init-themes)
(require 'init-osx-keys)
(require 'init-gui-frames)
;;(require 'init-maxframe)
;;(require 'init-proxies)
(require 'init-dired)
(require 'init-search)
(require 'init-uniquify)
;;(require 'init-ibuffer)
(require 'init-flycheck)

(require 'init-yaml)
(require 'init-spelling)

(require 'init-recentf)
(require 'init-ido)
;;(require 'init-hippie-expand)
(require 'init-auto-complete)
(require 'init-windows)
(require 'init-sessions)
(require 'init-fonts)
;; (require 'init-mmm)
;;(require 'init-growl)

(require 'init-editing-utils)
(require 'init-grep)

;;(require 'init-darcs)
(require 'init-git)

;;(require 'init-crontab)
;;(require 'init-textile)
(require 'init-markdown)
;;(require 'init-csv)
;;(require 'init-erlang)
(require 'init-javascript)
;;(require 'init-sh)
;;(require 'init-php)
;;(require 'init-org)
(require 'init-web-mode)
(require 'init-css)
;;(require 'init-haml)
;;(require 'init-python-mode)
;;(require 'init-haskell)
(require 'init-golang)
;;(require 'init-rails)
;;(require 'init-sql)
;;
;;(require 'init-paredit)
;;(require 'init-lisp)
;;(require 'init-slime)
;;(require 'init-clojure)
;;(require 'init-common-lisp)

;;(when *spell-check-support-enabled*
;;  (require 'init-spelling))

;;(require 'init-marmalade)
(require 'init-misc)

;; Extra packages which don't require any configuration

;;(require-package 'gnuplot)
;;(require-package 'lua-mode)
;;(require-package 'htmlize)
;;(require-package 'dsvn)
;;(when *is-a-mac*
;;  (require-package 'osx-location))
(require-package 'regex-tool)


(require 'init-keybind)
(require 'init-helm)
(require 'init-whitespace)
(require 'init-linum)
(require 'init-sr-speedbar)
(require 'init-yasnippet)
(require 'init-highlight)
(require 'init-open-junk-file)
;;(require 'init-ace-jump)
(require 'init-avy)
(require 'init-rainbow)
(require 'init-iedit)
;; (require 'init-golden-ratio)
(require 'init-perl)
(require 'init-ruby)
(require 'init-bookmark)
(require 'init-ctags)
(require 'init-quickrun)
(require 'init-pair)
(require 'init-projectile)
;; (require 'init-tabbar)
(require 'init-neotree)

(require 'init-ediff)
(require 'init-paste)

;; make evil config at last
(require 'init-evil)

;;----------------------------------------------------------------------------
;; Allow access from emacsclient
;;----------------------------------------------------------------------------
(require 'server)
(unless (server-running-p)
  (server-start))


;;----------------------------------------------------------------------------
;; Variables configured via the interactive 'customize' interface
;;----------------------------------------------------------------------------
(when (file-exists-p custom-file)
  (load custom-file))


;;----------------------------------------------------------------------------
;; Allow users to provide an optional "init-local" containing personal settings
;;----------------------------------------------------------------------------
(when (file-exists-p (expand-file-name "init-local.el" user-emacs-directory))
  (error "Please move init-local.el to ~/.emacs.d/lisp"))
(require 'init-local nil t)


;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

(add-hook 'after-init-hook
          (lambda ()
            (message "init completed in %.2fms"
                     (sanityinc/time-subtract-millis after-init-time before-init-time))))


(provide 'init)

;; Local Variables:
;; coding: utf-8
;; no-byte-compile: t
;; End:
