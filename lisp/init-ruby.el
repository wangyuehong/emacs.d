(use-package ruby-mode
  :ensure nil
  :init
  (setq-default
   ruby-use-encoding-map nil
   ruby-insert-encoding-magic-comment nil)
  :config
  (key-chord-define ruby-mode-map "--" (smartchr '("->" "=>")))
  )

(provide 'init-ruby)
