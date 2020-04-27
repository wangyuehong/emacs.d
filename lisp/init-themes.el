;;; init-themes.el  --- edit configurations. -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package srcery-theme
  :hook (after-init .(lambda () (load-theme 'srcery))))

(provide 'init-themes)
;;; init-themes.el ends here
