;; change meta key
(setq mac-command-modifier 'meta)


(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)

(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))

(provide 'init-custom)
