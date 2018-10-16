;; smerge-mode
(setq smerge-command-prefix (kbd "C-c m"))

(when (require 'magit)

  ;; repositories for magit-list-repositories
  (setq magit-repository-directories `((,user-emacs-directory . 0)))
  (when (jh/mac?)
    (add-to-list 'magit-repository-directories '("~/Codes" . 3)))
  (when (jh/windows?)
    (add-to-list 'magit-repository-directories '("e:/Codes" . 3)))

  ;; keybinding
  (global-set-key (kbd "<f1>") 'magit-status)
  (global-set-key (kbd "<f2>") 'magit-blame)
  (global-set-key (kbd "<f3>") 'magit-log-all-branches)
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-c g") 'magit-status))


(provide 'init-git)
