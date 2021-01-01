(defalias 'yes-or-no-p 'y-or-n-p)

;; -----------------------------------------------------------------------------
;; custom system encoding, time locale
;; -----------------------------------------------------------------------------
(prefer-coding-system 'utf-8)
(setq-default
  default-buffer-file-coding-system 'utf-8
  system-time-locale "C")
(modify-coding-system-alist 'file "\\.bat\\'" 'chinese-gbk)

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
;; many default mode that I prefer
;; -----------------------------------------------------------------------------
(delete-selection-mode 1)
(display-time-mode 1)
(electric-pair-mode 1)
(editorconfig-mode 1)
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
;; hideshow mode
;; -----------------------------------------------------------------------------
(when (require 'hi-lock)
  ;; common
  (add-hook 'c-mode-common-hook 'hs-minor-mode)
  (add-hook 'sh-mode-hook 'hs-minor-mode)
  (add-hook 'emacs-lisp-mode-hook 'hs-minor-mode)
  (add-hook 'lisp-mode-hook 'hs-minor-mode)
  ;; working
  (add-hook 'java-mode-hook 'hs-minor-mode)
  (add-hook 'python-mode-hook 'hs-minor-mode)
  ;; frontend
  (add-hook 'js2-mode-hook 'hs-minor-mode)
  (add-hook 'typescript-mode-hook 'hs-minor-mode)
  (add-hook 'css-mode-hook 'hs-minor-mode)
  (add-hook 'mhtml-mode-hook 'hs-minor-mode))

;; -----------------------------------------------------------------------------
;; misc option
;; -----------------------------------------------------------------------------
(setq-default
  inhibit-startup-message t
  blink-cursor-mode t
  ;; cursor-type 'bar
  blink-cursor-interval 1
  ;; ediff-split-window-function 'split-window-horizontally
  ;; ediff-window-setup-function 'ediff-setup-windows-plain
  display-time-format "%Y-%m-%d %H:%M"
  ;; disable recenter point
  scroll-conservatively 100
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
;; rainbow-delimiters
;; -----------------------------------------------------------------------------
(custom-set-faces
  '(rainbow-delimiters-depth-1-face ((t (:foreground "#8d5649"))))
  '(rainbow-delimiters-depth-2-face ((t (:foreground "#d8241f"))))
  '(rainbow-delimiters-depth-3-face ((t (:foreground "#9564bf"))))
  '(rainbow-delimiters-depth-4-face ((t (:foreground "#24a222"))))
  '(rainbow-delimiters-depth-5-face ((t (:foreground "#ff7f00"))))
  '(rainbow-delimiters-depth-6-face ((t (:foreground "#1776b6"))))
  '(rainbow-delimiters-depth-7-face ((t (:foreground "#00bed1"))))
  '(rainbow-delimiters-depth-8-face ((t (:foreground "#bcbf00"))))
  '(rainbow-delimiters-depth-9-face ((t (:foreground "#e574c3"))))
  '(rainbow-delimiters-unmatched-face ((t (:background "#d8241f"))))
  '(rainbow-delimiters-mismatched-face ((t (:background "#ffff99")))))

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
