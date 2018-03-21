;; key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "C-j") 'join-line)


(defun jh-tmp-buffer ()
  "Create a temporary buffer"
  (interactive)
  (switch-to-buffer (make-temp-name "scratch+")))


(provide 'init-editing)
