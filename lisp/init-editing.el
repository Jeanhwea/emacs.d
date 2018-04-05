;; key bindings
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-x C-.") 'pop-global-mark)
(global-set-key (kbd "C-j") 'join-line)


;; Tab, Space, Indentation setup
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)


(defun jh/temporary-buffer ()
  "Create a temporary buffer"
  (interactive)
  (switch-to-buffer (make-temp-name "scratch+")))


(provide 'init-editing)
