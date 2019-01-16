;;; init-spelling.el --- Spell check settings -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

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

(use-package flyspell-lazy
  :after flyspell
  :config (flyspell-lazy-mode 1)
  )

(provide 'init-spelling)
;;; init-spelling.el ends here