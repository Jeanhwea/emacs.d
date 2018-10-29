;; -----------------------------------------------------------------------------
;; gui
;; -----------------------------------------------------------------------------
(setq-default
  inhibit-startup-message t
  fill-column 80
  blink-cursor-mode t
  ;; cursor-type 'bar
  blink-cursor-interval 1
  ;; ediff-split-window-function 'split-window-horizontally
  ;; ediff-window-setup-function 'ediff-setup-windows-plain
  display-time-format "%Y-%m-%d %H:%M"
  line-number-mode t
  column-number-mode t
  size-indication-mode t)

;;(when (require 'nyan-mode) (nyan-mode 1))

(menu-bar-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(display-time-mode 1)

;;(global-hl-line-mode 1) ; highlight the current line
;;(when (require 'fill-column-indicator)
;;  (add-hook 'after-change-major-mode-hook 'fci-mode))

(global-prettify-symbols-mode 1)

;; -----------------------------------------------------------------------------
;; theme
;; -----------------------------------------------------------------------------
(when (require 'color-theme-sanityinc-solarized nil t)
  ;; default use solarized dark theme
  (add-hook 'after-init-hook
    (lambda () (load-theme 'sanityinc-solarized-light t)))
  ;; Add helper command to make changing color theme more faster
  (defun jh/load-light-theme()
    "Activate a light color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-light t))
  (defun jh/load-dark-theme()
    "Activate a dark color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-dark t))
  (defun jh/toggle-light-dark-theme ()
    "Toggle solarized light/dark theme"
    (interactive)
    (let ((current-theme (car custom-enabled-themes)))
      (when (string-equal current-theme "sanityinc-solarized-dark")
        (load-theme 'sanityinc-solarized-light))
      (when (string-equal current-theme "sanityinc-solarized-light")
        (load-theme 'sanityinc-solarized-dark))))
  (global-set-key (kbd "<f12>") 'jh/toggle-light-dark-theme))


;; -----------------------------------------------------------------------------
;; font
;; -----------------------------------------------------------------------------
(when (jh/windows?)
  (set-face-attribute 'default nil :font "Consolas 10")
  ;; 中文字体单独设置
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
      charset
      (font-spec :family "Microsoft YaHei" :size 22))))

(provide 'init-apperence)
