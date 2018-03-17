;; change meta key
(setq mac-command-modifier 'meta)

;; bind keys
(global-set-key (kbd "C-.") 'set-mark-command)

;; refuse backup on currect folder
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; always use 'y or n', refuse 'yes of no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; recently opened files
(recentf-mode 1)

(provide 'init-custom)
