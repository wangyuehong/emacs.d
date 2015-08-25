;; Mimic Vim's set paste
;; http://ceyes.github.io/2015-01/from-Vim-to-Emacs/
;; From http://stackoverflow.com/questions/18691973/is-there-a-set-paste-option-in-emacs-to-paste-paste-from-external-clipboard
(defvar ttypaste-mode nil)
(add-to-list 'minor-mode-alist '(ttypaste-mode " Paste"))
(defun ttypaste-mode ()
  (interactive)
  (let ((buf (current-buffer))
        (ttypaste-mode t))
    (with-temp-buffer
      (let ((stay t)
            (text (current-buffer)))
        (redisplay)
        (while stay
          (let ((char (let ((inhibit-redisplay t)) (read-event nil t 0.1))))
            (unless char
              (with-current-buffer buf (insert-buffer-substring text))
              (erase-buffer)
              (redisplay)
              (setq char (read-event nil t)))
            (cond
             ((not (characterp char)) (setq stay nil))
             ((eq char ?\r) (insert ?\n))
             ((eq char ?\e)
              (if (sit-for 0.1 'nodisp) (setq stay nil) (insert ?\e)))
             (t (insert char)))))
        (insert-buffer-substring text)))))

(provide 'init-paste)
