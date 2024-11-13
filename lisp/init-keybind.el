;;; init-keybind.el --- Keybind configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (which-key-add-key-based-replacements "C-c &" "yasnippet")
  (which-key-add-key-based-replacements "C-c @" "hideshow")
  (which-key-add-key-based-replacements "C-c t" "gotest")
  (which-key-add-key-based-replacements "C-c l" "lsp")

  (which-key-add-key-based-replacements "C-x 8" "unicode")
  (which-key-add-key-based-replacements "C-x 8 e" "emoji")
  (which-key-add-key-based-replacements "C-x a" "abbrev")
  (which-key-add-key-based-replacements "C-x c" "copilot")
  (which-key-add-key-based-replacements "C-x n" "narrow")
  (which-key-add-key-based-replacements "C-x p" "project")
  (which-key-add-key-based-replacements "C-x j" "bookmark")
  (which-key-add-key-based-replacements "C-x r" "register & bookmark")
  (which-key-add-key-based-replacements "C-x t" "tab")
  (which-key-add-key-based-replacements "C-x x" "buffer")
  (which-key-add-key-based-replacements "C-x RET" "coding-system")
  :custom
  (which-key-dont-use-unicode nil)
  (which-key-idle-delay 0.3)
  (which-key-show-remaining-keys t)
  (which-key-side-window-max-height 0.35)
  (which-key-sort-order 'which-key-key-order-alpha))

(use-package emacs
    :if (not (display-graphic-p))
    :init
    (defun character--apply-modifiers (c &rest modifiers)
      "Apply modifiers to the character C.
MODIFIERS must be a list of symbols amongst (meta control shift).
Return an event vector."
      (if (memq 'control modifiers) (setq c (if (and (<= ?a c) (<= c ?z))
                                                (logand c ?\x1f)
                                                (logior (lsh 1 26) c))))
      (if (memq 'meta modifiers) (setq c (logior (lsh 1 27) c)))
      (if (memq 'shift modifiers) (setq c (logior (lsh 1 25) c)))
      (vector c))

    (add-hook
     'after-make-frame-functions
     (lambda
         ;; Take advantage of iterm2's CSI u support (https://gitlab.com/gnachman/iterm2/-/issues/8382).
         (xterm--init-modify-other-keys)

       ;; Courtesy https://emacs.stackexchange.com/a/13957, modified per
       ;; https://gitlab.com/gnachman/iterm2/-/issues/8382#note_365264207
       (when (and (boundp 'xterm-extra-capabilities) (boundp 'xterm-function-map))
         (let ((c 32))
           (while (<= c 126)
             (mapc (lambda (x)
                     (define-key xterm-function-map (format (car x) c)
                       (apply 'character--apply-modifiers c (cdr x))))
                   '(;; with ?.VT100.formatOtherKeys: 0
                     ("\e\[27;3;%d~" meta)
                     ("\e\[27;5;%d~" control)
                     ("\e\[27;6;%d~" control shift)
                     ("\e\[27;7;%d~" control meta)
                     ("\e\[27;8;%d~" control meta shift)
                     ;; with ?.VT100.formatOtherKeys: 1
                     ("\e\[%d;3u" meta)
                     ("\e\[%d;5u" control)
                     ("\e\[%d;6u" control shift)
                     ("\e\[%d;7u" control meta)
                     ("\e\[%d;8u" control meta shift)))
             (setq c (1+ c))))))))

(provide 'init-keybind)
;;; init-keybind.el ends here
