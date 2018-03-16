;; change meta key
(setq mac-command-modifier 'meta)

;; refuse backup on currect folder
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

;; always use 'y or n', refuse 'yes of no'
(defalias 'yes-or-no-p 'y-or-n-p)

(provide 'init-custom)
