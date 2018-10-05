(when (require 'magit)

  ;; repositories for magit-list-repositories
  (setq magit-repository-directories `((,user-emacs-directory . 0)))
  (when (string-equal "darwin" system-type)
    (add-to-list 'magit-repository-directories '("~/Codes" . 3)))
  (when (string-equal "windows-nt" system-type)
    (add-to-list 'magit-repository-directories '("e:/Codes" . 3)))

  ;; keybinding
  (global-set-key (kbd "C-x g") 'magit-status))

(provide 'init-git)
