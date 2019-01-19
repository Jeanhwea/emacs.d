(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------
;; custom system encoding, time locale
;; -----------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq-default
  default-buffer-file-coding-system 'utf-8
  system-time-locale "C")


;; -----------------------------------------------------------------------------
;; setup system keyboard
;; -----------------------------------------------------------------------------
(setq-default
  mac-command-modifier 'meta
  mac-option-modifier 'super
  w32-pass-lwindow-to-system nil
  w32-lwindow-modifier 'super
  w32-pass-rwindow-to-system nil
  w32-rwindow-modifier 'super)


;; -----------------------------------------------------------------------------
;; mark
;; -----------------------------------------------------------------------------
(global-set-key (kbd "C-,") 'set-mark-command)
(global-set-key (kbd "C-.") 'set-mark-command)
(global-set-key (kbd "C-;") 'pop-global-mark)


;; -----------------------------------------------------------------------------
;; windows and frames
;; -----------------------------------------------------------------------------
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "<f9>") 'make-frame-command)
(global-set-key (kbd "<f10>") 'delete-frame)
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)
(global-set-key (kbd "<f11>") 'toggle-frame-fullscreen)
;;(global-set-key (kbd "C-x n n") 'narrow-to-region)
;;(global-set-key (kbd "C-x n w") 'widen)

;; -----------------------------------------------------------------------------
;; many default mode that I prefer
;; -----------------------------------------------------------------------------
(delete-selection-mode 1)
(display-time-mode 1)
(electric-pair-mode 1)
(show-paren-mode 1)
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
;; (linum-mode 1)

;; (nyan-mode 1)
;; (when (require 'fill-column-indicator)
;;  (add-hook 'after-change-major-mode-hook 'fci-mode))
;; (global-hl-line-mode 1)
(global-prettify-symbols-mode 1)


;; -----------------------------------------------------------------------------
;; misc option
;; -----------------------------------------------------------------------------
(setq-default
  inhibit-startup-message t
  blink-cursor-mode t
  cursor-type 'bar
  blink-cursor-interval 1
  ;; ediff-split-window-function 'split-window-horizontally
  ;; ediff-window-setup-function 'ediff-setup-windows-plain
  display-time-format "%Y-%m-%d %H:%M"
  ring-bell-function 'ignore
  line-number-mode t
  column-number-mode t
  size-indication-mode t
  indent-tabs-mode nil
  fill-column 80
  tab-width 4)

;; -----------------------------------------------------------------------------
;; enable narrow, downcase and upcase
;; -----------------------------------------------------------------------------
(put 'narrow-to-region 'disabled nil)     ; C-c n
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'downcase-region 'disabled nil)      ; C-x C-u
(put 'upcase-region 'disabled nil)        ; C-x C-l


;; -----------------------------------------------------------------------------
;; some improvement on Windows PC
;; -----------------------------------------------------------------------------

;;
;; when compact large fonts cause lots of resources, the editor will be very slow
;; so just inhibit compacting, when using chinese font
(when (jh/windows?)
  (setq inhibit-compacting-font-caches t))

;;
;; doc-view-mode support https://www.emacswiki.org/emacs/DocViewMode
;; 1. Set up png support, you’ll have to look elsewhere for instructions on this.
;; 2. Install ghostscript and add the bin and lib directories to your path.
;; 3. Get xpdf for windows and put the executables somewhere on your path.
;; 4. Set this in your .emacs: (setq doc-view-ghostscript-program “gswin32c”)
;; 5. That should be it.
(when (jh/windows?)
  (setq doc-view-ghostscript-program "gswin32c"))

(provide 'init-basic)
