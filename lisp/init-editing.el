;; key bindings
(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-x C-,") 'pop-global-mark)
(global-set-key (kbd "C-j") 'join-line)


;; Tab, Space, indentation setup
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-width 4)

;; Javascript indentation
(add-hook 'js-mode-hook
   (lambda()
      (setq indent-tabs-mode nil)
      (setq js-indent-level 2)
      (setq tab-width 2)))


;; Display trailing whitespace
(setq-default show-trailing-whitespace t)


(defun jh/temporary-buffer ()
  "Create a temporary buffer"
  (interactive)
  (switch-to-buffer (make-temp-name "scratch+")))


(provide 'init-editing)
