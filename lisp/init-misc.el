;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package flyspell
  :ensure nil
  :diminish
  :if (executable-find "aspell")
  :hook (((text-mode outline-mode) . flyspell-mode)
         (prog-mode . flyspell-prog-mode)
         (flyspell-mode . (lambda ()
                            (dolist (key '("C-;" "C-," "C-."))
                              (unbind-key key flyspell-mode-map)))))
  :custom
  (flyspell-issue-message-flag nil)
  (ispell-program-name "aspell")
  (ispell-extra-args '("--sug-mode=ultra" "--lang=en_US" "--run-together"))
  )

;; (use-package flyspell-lazy
;;   :after flyspell
;;   :config (flyspell-lazy-mode 1)
;;   )

(use-package youdao-dictionary)

(use-package calendar
  :ensure nil
  :hook ((calendar-today-visible . calendar-mark-today))
  :custom
  (calendar-mark-holidays-flag t)
  (calendar-week-start-day 1)
  )

(use-package cal-china-x
  :after calendar
  :demand t
  :config
  (use-package japanese-holidays :demand t)
  (setq calendar-holidays (append cal-china-x-chinese-holidays japanese-holidays)))

(provide 'init-misc)
