;; -*- coding: utf-8; lexical-binding: t; -*-

(use-package org
  :ensure org-plus-contrib
  :bind (("C-c a" . org-agenda)
         ("C-c b" . org-switchb))
  :custom
  (org-directory "~/.org")
  (org-tags-column -80)
  (org-log-done 'time)
  (org-startup-indented t)
  (org-hide-emphasis-markers t)
  )

(use-package ox
  :ensure nil
  :after org
  :custom
  (org-export-with-toc nil)
  (org-export-backends '(ascii html md)))

(use-package ox-gfm :demand t)

(provide 'init-org)
