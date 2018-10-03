;; -----------------------------------------------------------------------------
;; encoding
;; -----------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)

;; -----------------------------------------------------------------------------
;; behaviour
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

;; -----------------------------------------------------------------------------
;; doc-view-mode support https://www.emacswiki.org/emacs/DocViewMode
;; 1. Set up png support, you’ll have to look elsewhere for instructions on this.
;; 2. Install ghostscript and add the bin and lib directories to your path.
;; 3. Get xpdf for windows and put the executables somewhere on your path.
;; 4. Set this in your .emacs: (setq doc-view-ghostscript-program “gswin32c”)
;; 5. That should be it.
;; -----------------------------------------------------------------------------
(when (string-equal "windows-nt" system-type)
  (setq doc-view-ghostscript-program "gswin32c"))


;; --------------------------------------------------------------------------
;; backup
;; --------------------------------------------------------------------------
(make-directory "~/.emacs.d/autosaves/" t)
(make-directory "~/.emacs.d/backups/" t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/autosaves/" t)))
(setq backup-directory-alist '((".*" . "~/.emacs.d/backups/")))


(provide 'init-startup)
