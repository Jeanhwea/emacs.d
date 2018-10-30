;; -----------------------------------------------------------------------------
;; custom behavior
;; -----------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

(setq system-time-locale "C")
(setq ring-bell-function 'ignore) ;; get rid of belling

;; setup windows key
(setq
  mac-command-modifier 'meta
  mac-option-modifier 'super
  w32-pass-lwindow-to-system nil
  w32-lwindow-modifier 'super
  w32-pass-rwindow-to-system nil
  w32-rwindow-modifier 'super)


(defalias 'yes-or-no-p 'y-or-n-p) ; always use 'y or n', refuse 'yes of no'

;; session
(save-place-mode 1)
(when (require 'desktop)
  (setq desktop-buffers-not-to-save
    (concat "\\("
      "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
      "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
	    "\\)$"))
  (add-to-list 'desktop-modes-not-to-save 'Info-mode)
  (add-to-list 'desktop-modes-not-to-save 'dired-mode)
  (add-to-list 'desktop-modes-not-to-save 'fundamental-mode)
  (add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
  (add-to-list 'desktop-modes-not-to-save 'magit-mode)
  (desktop-save-mode 1))

;; parenthesis behavior
(show-paren-mode 1)
(electric-pair-mode 1)

(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

(put 'downcase-region 'disabled nil) ; C-x C-u
(put 'upcase-region 'disabled nil)   ; C-x C-l


;; -----------------------------------------------------------------------------
;; backup and auto-save
;; -----------------------------------------------------------------------------
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq create-lockfiles nil)
(setq auto-save-file-name-transforms
  '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist
  '((".*" . "~/.emacs.d/backups/")))
(auto-save-visited-mode 1)


;; -----------------------------------------------------------------------------
;; exec-path-from-shell
;; -----------------------------------------------------------------------------
(when (require 'exec-path-from-shell)
  (when (jh/mac?)
    (exec-path-from-shell-initialize)))


;; -----------------------------------------------------------------------------
;; recentf
;; -----------------------------------------------------------------------------
(setq-default
  recentf-max-saved-items 1000
  recentf-exclude
    '("/.emacs.d/elfeed/*"
      "/.emacs.d/elpa/*"
      "/.emacs.d/ido.last"
      "/.emacs.d/bookmarks"
      "/agenda/"
      "/tmp/"
      "/ssh:"))
(recentf-mode 1)


;; -----------------------------------------------------------------------------
;; when compact large fonts cause lots of resources, the editor will be very slow
;; so just inhibit compacting, when using chinese font
;; -----------------------------------------------------------------------------
(when (jh/windows?)
  (setq inhibit-compacting-font-caches t))


(provide 'init-startup)
