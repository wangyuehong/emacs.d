;;; init-quickrun.el --- quickrun configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package quickrun
  :commands quickrun
  :init
  (setq quickrun-timeout-seconds 15)
  )

(provide 'init-quickrun)
;;; init-quickrun.el ends here
