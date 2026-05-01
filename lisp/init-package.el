;;; init-package.el --- Initialize package configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package package
  :ensure nil
  :init
  (setq use-package-always-ensure t
    use-package-always-defer t
    use-package-expand-minimally t
    package-install-upgrade-built-in t)
  :config
  ;; install into separate package dirs for each emacs version
  (let ((versioned-package-dir
          (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
            user-emacs-directory)))
    (setq package-user-dir versioned-package-dir))
  (setq package-archives
    '(("melpa"   . "https://melpa.org/packages/")
       ("gnu"    . "https://elpa.gnu.org/packages/")
       ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
  (unless (bound-and-true-p package--initialized)
    (package-initialize)))

;; Workaround for two upstream issues in Emacs 31's
;; `package--upgradeable-packages':
;;
;; 1. It splices the memoized result of `package--builtin-alist' with
;;    `package-alist' via `nconc', destructively mutating the cached
;;    builtin alist.  Each subsequent invocation re-mutates the same
;;    list, eventually turning both alists into circular lists and
;;    breaking any other code that walks them.
;; 2. The same merge causes a package that exists both as a built-in
;;    (or is registered with a nil version in `package--builtins') and
;;    as a newer MELPA install to be flagged via the built-in entry,
;;    even when the installed version already matches the archive.  The
;;    follow-up `package-upgrade' call then signals `Cannot upgrade
;;    `<pkg>'' for every such package on `package-upgrade-all'.
;;
;; The override below uses non-mutating `append', dedupes by name, and
;; compares the *installed* version against the archive so only true
;; upgrade candidates remain.
(defun my/package-upgradeable-packages (&optional include-builtins)
  "Drop-in replacement for `package--upgradeable-packages'."
  (package--archives-initialize)
  (let ((seen (make-hash-table :test #'eq))
         result)
    (dolist (elt (append (and include-builtins (package--builtin-alist))
                   (package--alist)))
      (let* ((name (car elt))
              (available (package-get-descriptor name 'archive))
              (installed (package-get-descriptor name 'installed)))
        (when (and available
                (not (gethash name seen))
                (not (package-vc-p (cadr elt)))
                (or (null installed)
                  (version-list-< (package-desc-version installed)
                    (package-desc-version available))))
          (puthash name t seen)
          (push name result))))
    (nreverse result)))

(advice-add 'package--upgradeable-packages
  :override #'my/package-upgradeable-packages)

;; Update GPG keyring for GNU ELPA
(use-package gnu-elpa-keyring-update)

(provide 'init-package)
;;; init-package.el ends here
