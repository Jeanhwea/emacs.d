;; -----------------------------------------------------------------------------
;; gui
;; -----------------------------------------------------------------------------
(setq inhibit-startup-message t)

;(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)

(blink-cursor-mode -1)
;(setq-default cursor-type 'bar)


;; -----------------------------------------------------------------------------
;; theme
;; -----------------------------------------------------------------------------
(when (require 'color-theme-sanityinc-solarized nil t)

  ;; default use solarized dark theme
  (load-theme 'sanityinc-solarized-light t)

  ;; Add helper command to make changing color theme more faster
  (defun jh/light()
    "Activate a light color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-light t))

  (defun jh/dark()
    "Activate a light color theme"
    (interactive)
    (load-theme 'sanityinc-solarized-dark t)))


;; -----------------------------------------------------------------------------
;; font
;; -----------------------------------------------------------------------------
(when (string-equal "windows-nt" system-type)
  (set-face-attribute 'default nil :font "Consolas 14")
  ;; 中文字体单独设置
  (dolist (charset '(kana han symbol cjk-misc bopomofo))
    (set-fontset-font (frame-parameter nil 'font)
      charset
      (font-spec :family "Microsoft YaHei" :size 18))))


(provide 'init-apperence)