;; --------------------------------------------------------------------------
;; more friendly interaction
;; --------------------------------------------------------------------------
;; change meta key, let command key be additional meta key
(setq mac-command-modifier 'meta)
;; always use 'y or n', refuse 'yes of no'
(defalias 'yes-or-no-p 'y-or-n-p)

;; --------------------------------------------------------------------------
;; Use utf-8 as default encoding system
;; --------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq default-buffer-file-coding-system 'utf-8)


;; --------------------------------------------------------------------------
;; doc-view-mode support https://www.emacswiki.org/emacs/DocViewMode
;; 1. Set up png support, you’ll have to look elsewhere for instructions on this.
;; 2. Install ghostscript and add the bin and lib directories to your path.
;; 3. Get xpdf for windows and put the executables somewhere on your path.
;; 4. Set this in your .emacs: (setq doc-view-ghostscript-program “gswin32c”)
;; 5. That should be it.
;; --------------------------------------------------------------------------
(when (string-equal "windows-nt" system-type)
  (setq doc-view-ghostscript-program "gswin32c"))


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
