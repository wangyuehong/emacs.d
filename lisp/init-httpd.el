;;; init-httpd.el --- highlight configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package simple-httpd
  :defer
  :commands(httpd-start httpd-stop)
  :init
  (setq httpd-port 5678)
  ;; (setq httpd-root "/var/www")
  (setq httpd-root default-directory)
  )

(provide 'init-httpd)
;;; init-httpd.el ends here