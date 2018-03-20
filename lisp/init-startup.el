;; --------------------------------------------------------------------------
;; bind keys
;; --------------------------------------------------------------------------
(global-set-key (kbd "M-6") 'bar-browse)
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "M-2") 'set-mark-command)
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)

;; useful shortcuts
;(global-set-key (kbd "C-c b") 'list-bookmarks)
;(global-set-key (kbd "C-c r") 'recentf-open-files)
;(global-set-key (kbd "C-c v") 'evil-mode)


;; --------------------------------------------------------------------------
;; backup setting
;; --------------------------------------------------------------------------
;; refuse backup on currect folder
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))


;; --------------------------------------------------------------------------
;; more friendly interaction
;; --------------------------------------------------------------------------
;; change meta key, let command key be additional meta key
(setq mac-command-modifier 'meta)
;; always use 'y or n', refuse 'yes of no'
(defalias 'yes-or-no-p 'y-or-n-p)


;; --------------------------------------------------------------------------
;; restore last configuration: files, workspace and so on
;; --------------------------------------------------------------------------
;; recently opened files
(recentf-mode 1)
;; save cursor position
(save-place-mode 1)
;; restore last opened files and config
;; (desktop-save-mode 1)


;; --------------------------------------------------------------------------
;; editing setting
;; --------------------------------------------------------------------------
;; auto complete () {} []
(electric-pair-mode 1)
;; enable upcase/downcase
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(provide 'init-startup)
