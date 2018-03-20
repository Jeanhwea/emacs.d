(require 'helm)

;; override default key bindings
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)
(global-set-key (kbd "C-x x") 'helm-M-x)

(provide 'init-helm)
