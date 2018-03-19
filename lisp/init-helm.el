(require 'helm)

;; override default key bindings
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x r b") 'helm-bookmarks)

(provide 'init-helm)
