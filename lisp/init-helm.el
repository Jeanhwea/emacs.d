(require 'helm)

;; override default key bindings
(global-set-key (kbd "M-7") 'helm-recentf)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "M-8") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "M-9") 'helm-bookmarks)
(global-set-key (kbd "C-x x") 'helm-M-x)


(provide 'init-helm)
