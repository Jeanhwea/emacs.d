;; --------------------------------------------------------------------------
;; more friendly interaction
;; --------------------------------------------------------------------------
;; change meta key, let command key be additional meta key
(setq mac-command-modifier 'meta)
;; always use 'y or n', refuse 'yes of no'
(defalias 'yes-or-no-p 'y-or-n-p)


;; --------------------------------------------------------------------------
;; backup setting
;; --------------------------------------------------------------------------
;; refuse backup on currect folder
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))


;; --------------------------------------------------------------------------
;; restore last configuration: files, workspace and so on
;; --------------------------------------------------------------------------
;; recently opened files
(recentf-mode 1)
;; save cursor position
(save-place-mode 1)
;; restore last opened files and config
;(desktop-save-mode 1)
;; auto complete pairs
(electric-pair-mode 1)
;; enable upcase/downcase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)


(provide 'init-startup)
