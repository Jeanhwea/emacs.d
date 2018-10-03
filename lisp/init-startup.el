;; -----------------------------------------------------------------------------
;; encoding
;; -----------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; -----------------------------------------------------------------------------
;; behavior
;; -----------------------------------------------------------------------------
(setq mac-command-modifier 'meta) ; change meta key
(defalias 'yes-or-no-p 'y-or-n-p) ; always use 'y or n', refuse 'yes of no'
(recentf-mode 1)
(save-place-mode 1)
(electric-pair-mode 1)
;;(desktop-save-mode 1)
(put 'narrow-to-page 'disable nil)
(put 'downcase-region 'disabled nil) ; C-x C-u
(put 'upcase-region 'disabled nil)   ; C-x C-l


;; --------------------------------------------------------------------------
;; backup
;; --------------------------------------------------------------------------
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq auto-save-file-name-transforms
  '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
  '((".*" . "~/.emacs.d/backups/")))


;; -----------------------------------------------------------------------------
;; recentf
;; -----------------------------------------------------------------------------
(setq-default
  recentf-max-saved-items 1000
  recentf-exclude '("/tmp/" "/ssh:"))


;; -----------------------------------------------------------------------------
;; when compact large fonts cause lots of resources, the editor will be very slow
;; so just inhibit compacting, when using chinese font
;; -----------------------------------------------------------------------------
(when (string-equal "windows-nt" system-type)
  (setq inhibit-compacting-font-caches t))


(provide 'init-startup)
