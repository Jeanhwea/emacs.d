;; smerge-mode
(setq smerge-command-prefix (kbd "M-0"))

(when (require 'magit)

  ;; repositories for magit-list-repositories
  (setq
    magit-repository-directories `((,user-emacs-directory . 0)))

  (when (jh/mac?)
    (add-to-list 'magit-repository-directories '("~/Code" . 3)))
  (when (jh/windows?)
    (add-to-list 'magit-repository-directories '("e:/Code" . 3)))

  ;; keybinding
  (global-set-key (kbd "<f1>") 'magit-status)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "<f2>") 'magit-file-popup)
  (global-set-key (kbd "C-x f") 'magit-file-popup))


(provide 'init-git)
